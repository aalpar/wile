// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package values_test

import (
	"context"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

const (
	// scopeOpsManifestPath is the repo-relative path of the committed manifest,
	// under testdata/ beside axis-b-manifest.scm, whose ratchet this mirrors.
	scopeOpsManifestPath = "testdata/scope-ops-manifest.scm"

	// scopeOpsToolPath is the walk. It lives under tools/ rather than beside
	// this test because it imports golang.org/x/tools/go/packages, and no test
	// under pkg/ does — the cost of loading the whole module with full type
	// information belongs in a command, invoked as a subprocess.
	scopeOpsToolPath = "./tools/cmd/scopeopslint"

	// scopeOpsUpdateEnvVar regenerates the committed manifest instead of
	// asserting against it, on the WILE_AXIS_B_UPDATE model.
	scopeOpsUpdateEnvVar = "WILE_SCOPE_OPS_UPDATE"

	// scopeOpsNonTestCeiling is the number of non-test operations the tree is
	// permitted, and it is the size of scopeOpsBoundaryFiles. It is spelled
	// separately so that a careless regeneration of the manifest cannot move it:
	// the manifest is generated, this number is not.
	scopeOpsNonTestCeiling = 1
)

// scopeOpsBoundaryFiles is the complete set of non-test files permitted to
// operate on a scope-set slice, with the number of operations each may perform.
//
// There is exactly ONE, and it is the slice-to-Scopes boundary itself:
//
//   - pkg/values/scopes.go — ScopesFromSlice, the declared boundary
//     constructor, ranges the slice it converts. A constructor from a slice
//     cannot avoid touching one; that is what makes it the boundary, and it is
//     why this number is 1 rather than 0.
//
// pkg/values/source_context.go was the second entry until 2026-09-28. Its
// WithScope handed ScopesFromSlice a one-element []*Scope literal on the
// nil-receiver path; that was reducible to Scopes{}.Add(scope) and was reduced,
// so the row and the ceiling came down with it. Note the direction: a row leaves
// this list when the site is FIXED, never to accommodate a new one.
//
// Any other non-test file is a DEFECT, asserted as the ABSENCE of a site rather
// than as a count, because zero is the only defensible number for a
// representation that no longer exists.
var scopeOpsBoundaryFiles = map[string]int{
	"pkg/values/scopes.go": 1,
}

// scopeOpsSite mirrors one element of tools/cmd/scopeopslint's -json output.
type scopeOpsSite struct {
	File string `json:"file"`
	Line int    `json:"line"`
	Col  int    `json:"col"`
	Kind string `json:"kind"`
	Text string `json:"text"`
}

// scopeOpsCensus mirrors tools/cmd/scopeopslint's -json envelope.
type scopeOpsCensus struct {
	Sites     []scopeOpsSite `json:"sites"`
	ExprFiles []string       `json:"exprFiles"`
	PkgErrors []string       `json:"pkgErrors"`
	Unalias   bool           `json:"unalias"`
}

// TestScopeOpsManifest is the permanent ratchet on the scope-set representation.
//
// A scope set used to be a []*syntax.Scope / []*values.Scope that every touch
// copied. It is now the opaque values.Scopes persistent chain, which has no
// Slice() method and no indexed access, deliberately: member order is an
// implementation invariant, not part of the contract. Nothing in the type system
// stops a caller from reintroducing a slice beside it, so the prohibition is
// measured. tools/cmd/scopeopslint enumerates every expression go/types types as
// []*values.Scope and classifies it by what its parent node does to it.
//
// The numbers that make the measurement auditable:
//
//   - 423 operations at the pre-flip commit d2927bea (94 non-test, 329 test)
//     over 49 files, out of 91 files holding any scope-slice-typed expression.
//   - 118 over 11 files at that same commit with -noalias (13 files holding any
//     scope-slice-typed expression, not 91), and 37 over 4 files with -noalias
//     today. syntax.Scope is a type ALIAS of values.Scope, so without
//     types.Unalias the walk sees only pkg/values. This trap has bitten this
//     project twice; -noalias ships with the tool so the difference stays
//     measurable rather than folkloric, and -check refuses to run with it.
//   - 246 (2 non-test, 244 test) over 24 files now. The non-test pair is the
//     slice-to-Scopes boundary, named in scopeOpsBoundaryFiles. The test
//     remainder is overwhelmingly []*Scope{...} literals being handed to that
//     boundary constructor, which is the sanctioned idiom, not drift.
//
// What is asserted:
//
//   - Non-test: ABSENCE. Any non-test file outside scopeOpsBoundaryFiles must
//     carry zero operations, and the boundary pair must not grow. This is the
//     load-bearing assertion, and it is checked against the MEASUREMENT rather
//     than against the manifest, so regenerating the manifest cannot defeat it.
//   - Test files: per-file ceilings from the committed manifest. Fewer passes;
//     more fails. Regenerate to ratchet down.
//   - The manifest header still states the baseline and the rule.
//
// Regenerate with:
//
//	WILE_SCOPE_OPS_UPDATE=1 go test -count=1 -run TestScopeOpsManifest ./pkg/values/
//
// NEVER regenerate to silence a diff you have not read: a new non-test row is a
// defect, not drift. The rule is inherited from testdata/axis-b-manifest.scm,
// where regenerating past an unread diff is a recorded incident rather than a
// hypothetical.
func TestScopeOpsManifest(t *testing.T) {
	root := scopeOpsRepoRoot(t)

	if os.Getenv(scopeOpsUpdateEnvVar) != "" {
		out, errOut, err := runScopeOpsTool(t, root, "-write", scopeOpsManifestPath)
		if err != nil {
			t.Fatalf("scopeopslint -write %s: %v\n%s%s", scopeOpsManifestPath, err, errOut, out)
		}
		t.Logf("regenerated %s:\n%s", scopeOpsManifestPath, out)
		return
	}

	// Per-file ceilings, for every file including the test files. The tool owns
	// this comparison so there is exactly one implementation of it.
	checkOut, checkErrOut, checkErr := runScopeOpsTool(t, root, "-check", scopeOpsManifestPath)
	if checkErr != nil {
		t.Errorf("scopeopslint -check %s failed: %v\n%s%s",
			scopeOpsManifestPath, checkErr, checkErrOut, checkOut)
		t.Errorf("a row that GREW or APPEARED means code went back to the slice "+
			"representation. Fix the site. Regenerate (%s=1) only to record a "+
			"REDUCTION you have read.", scopeOpsUpdateEnvVar)
	} else {
		t.Log(strings.TrimSpace(checkOut))
	}

	// Non-test absence, asserted against the measurement rather than the file.
	census := scopeOpsMeasure(t, root)
	if len(census.PkgErrors) > 0 {
		t.Fatalf("scopeopslint reported %d package error(s): %v",
			len(census.PkgErrors), census.PkgErrors)
	}
	if !census.Unalias {
		t.Fatal("census was taken with -noalias: the walk sees only pkg/values and the count is meaningless")
	}
	if len(census.ExprFiles) == 0 {
		t.Fatal("census found no scope-slice-typed expression anywhere — the walk matched nothing, " +
			"which is a broken tool rather than a clean tree")
	}

	perFile := map[string]int{}
	total := 0
	for _, s := range census.Sites {
		if strings.HasSuffix(s.File, "_test.go") {
			continue
		}
		perFile[s.File]++
		total++
		_, permitted := scopeOpsBoundaryFiles[s.File]
		if permitted {
			continue
		}
		t.Errorf("non-test scope-slice operation at %s:%d:%d (%s): %s\n"+
			"  %s is not the slice-to-Scopes boundary. A scope set is values.Scopes: "+
			"ask Has/SubsetOf/Len/IsEmpty for semantics, Fingerprint for a key, All to "+
			"walk. Do not reintroduce []*Scope.",
			s.File, s.Line, s.Col, s.Kind, s.Text, s.File)
	}
	for path, limit := range scopeOpsBoundaryFiles {
		if perFile[path] > limit {
			t.Errorf("boundary file %s has %d operations, %d permitted — the boundary "+
				"is one conversion, not a place to accumulate slice code",
				path, perFile[path], limit)
		}
	}
	if total > scopeOpsNonTestCeiling {
		t.Errorf("%d non-test operations, ceiling %d. The ceiling is a constant in "+
			"this file, not a generated row: raising it is a decision, not a regeneration",
			total, scopeOpsNonTestCeiling)
	}

	// The header is the only place the baseline and the rule live. A
	// regeneration that dropped it would leave a manifest nobody can read.
	manifest, readErr := os.ReadFile(filepath.Join(root, scopeOpsManifestPath))
	if readErr != nil {
		t.Fatalf("read %s: %v", scopeOpsManifestPath, readErr)
	}
	for _, want := range []string{
		"423",
		"NEVER regenerate to silence a diff you have not read",
		"-noalias",
	} {
		if !strings.Contains(string(manifest), want) {
			t.Errorf("%s header no longer states %q", scopeOpsManifestPath, want)
		}
	}
	t.Logf("non-test operations: %d (ceiling %d); measured files with any "+
		"scope-slice-typed expression: %d", total, scopeOpsNonTestCeiling, len(census.ExprFiles))
}

// scopeOpsMeasure runs the walk and decodes its census. The census is read from
// stdout alone: the go command and the tool both report trouble on stderr, and
// mixing the two streams would turn a diagnostic into a JSON decode error.
func scopeOpsMeasure(t *testing.T, root string) scopeOpsCensus {
	t.Helper()
	out, errOut, err := runScopeOpsTool(t, root, "-json")
	if err != nil {
		t.Fatalf("scopeopslint -json: %v\n%s", err, errOut)
	}
	var q scopeOpsCensus
	decErr := json.Unmarshal([]byte(out), &q)
	if decErr != nil {
		t.Fatalf("decode scopeopslint -json: %v\nstderr:\n%s", decErr, errOut)
	}
	return q
}

// runScopeOpsTool runs the walk as a subprocess and returns stdout, stderr and
// the exit status.
//
// `go run` rather than a prebuilt binary, following pkg/wile's axis-b smoke
// test: the tool then tracks the tree it measures with no build step to forget.
// GOWORK=off keeps the load inside this module — the workspace resolves sibling
// checkouts whose packages are not this ratchet's business.
func runScopeOpsTool(t *testing.T, root string, args ...string) (stdout, stderr string, err error) {
	t.Helper()
	argv := append([]string{"run", scopeOpsToolPath}, args...)
	cmd := exec.CommandContext(context.Background(), "go", argv...)
	cmd.Dir = root
	cmd.Env = append(os.Environ(), "GOWORK=off")
	var outBuf, errBuf strings.Builder
	cmd.Stdout = &outBuf
	cmd.Stderr = &errBuf
	err = cmd.Run()
	return outBuf.String(), errBuf.String(), err
}

// scopeOpsRepoRoot locates the module root from this file's own position, so the
// test does not depend on the working directory it is run from.
func scopeOpsRepoRoot(t *testing.T) string {
	t.Helper()
	_, thisFile, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("runtime.Caller(0) failed: cannot locate the repo root")
	}
	return filepath.Join(filepath.Dir(thisFile), "..", "..")
}
