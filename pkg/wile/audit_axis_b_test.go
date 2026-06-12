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

// Axis-B analyzer regression guard (Phase 3.B).
//
// Runs audit/wile-axis-b.scm filtered to 4 well-known primitives via
// WILE_AXIS_B_NAMES and asserts the raw output contains bucket
// classifications for each. The script lives in wile because the
// analysis is wile-specific (sink methods, Go→wile type mapping,
// declared-return-type comparison against wile's TypeConstraint
// vocabulary). The generic Go-SSA primitives it invokes — go-ssa-build
// and go-ssa-narrow — live in the wile-goast repo.
//
// The test invokes wile-goast via `go run` against the workspace
// go.work, so no pre-built binary is required.

package wile

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestAxisBSmoke runs audit/wile-axis-b.scm filtered via WILE_AXIS_B_NAMES
// to 4 well-known primitives and asserts each appears in the raw output.
// Does not pin bucket assignments — those evolve with narrowing
// improvements in wile-goast. Guards against script crashes, missing
// entries, or output format drift.
func TestAxisBSmoke(t *testing.T) {
	if testing.Short() {
		t.Skip("smoke test runs go build + script — slow")
	}
	c := qt.New(t)

	// This test lives at pkg/wile/; the audit/ scripts and testdata/ manifest
	// live at the module root, two directories up.
	_, thisFile, _, _ := runtime.Caller(0)
	repoRoot := filepath.Join(filepath.Dir(thisFile), "..", "..")
	script := filepath.Join(repoRoot, "audit", "wile-axis-b.scm")

	_, err := os.Stat(script)
	c.Assert(err, qt.IsNil, qt.Commentf("script missing: %s", script))

	// wile-goast is resolved via the workspace-level go.work. In bare
	// checkouts (CI, `go install` consumers) the module isn't available.
	// Probe resolvability and skip if not present — the smoke test is a
	// local dev-loop guard, not a CI gate.
	probe := exec.CommandContext(context.Background(), "go", "list", "-m",
		"github.com/aalpar/wile-goast")
	probe.Dir = repoRoot
	_, probeErr := probe.CombinedOutput()
	if probeErr != nil {
		t.Skipf("wile-goast module not resolvable (no go.work?): %v", probeErr)
	}

	rawOut := filepath.Join(t.TempDir(), "axis-b-raw.scm")
	invOut := filepath.Join(t.TempDir(), "axis-b-inventory.md")

	// The go.work workspace resolves github.com/aalpar/wile-goast to the
	// sibling checkout, so `go run` picks up local changes without a
	// pre-built binary or a published version.
	cmd := exec.CommandContext(context.Background(), "go", "run",
		"github.com/aalpar/wile-goast/cmd/wile-goast",
		"-f", script)
	cmd.Dir = repoRoot
	cmd.Env = append(os.Environ(),
		"WILE_AXIS_B_NAMES=cons,null?,length,car",
		"WILE_AXIS_B_RAW_OUTPUT="+rawOut,
		"WILE_AXIS_B_INVENTORY="+invOut,
	)
	output, err := cmd.CombinedOutput()
	c.Assert(err, qt.IsNil, qt.Commentf("script failed: %s", string(output)))

	raw, err := os.ReadFile(rawOut)
	c.Assert(err, qt.IsNil)
	rawStr := string(raw)

	for _, name := range []string{"cons", "null?", "length", "car"} {
		c.Assert(strings.Contains(rawStr, `(name "`+name+`")`), qt.IsTrue,
			qt.Commentf("primitive %q missing from raw output", name))
	}

	c.Assert(strings.Contains(string(output), "4 primitives"), qt.IsTrue,
		qt.Commentf("stdout did not report 4 primitives: %s", string(output)))
}
