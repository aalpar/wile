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
// Runs audit/wile-axis-b.scm against a 4-entry fixture manifest and
// asserts the raw output contains bucket classifications for all four
// primitives. The script and its fixture live here in wile because the
// analysis it performs is wile-specific (sink methods, Go→wile type
// mapping, declared-return-type comparison). The generic Go-SSA
// primitives it invokes — go-ssa-build and go-ssa-narrow — live in the
// wile-goast repo.
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

// TestAxisBSmoke runs audit/wile-axis-b.scm against a 4-entry fixture
// and asserts each fixture primitive appears in the raw output. Does not
// pin bucket assignments — those evolve with narrowing improvements in
// wile-goast. Guards against script crashes, missing entries, or output
// format drift.
func TestAxisBSmoke(t *testing.T) {
	if testing.Short() {
		t.Skip("smoke test runs go build + script — slow")
	}
	c := qt.New(t)

	_, thisFile, _, _ := runtime.Caller(0)
	repoRoot := filepath.Dir(thisFile)
	script := filepath.Join(repoRoot, "audit", "wile-axis-b.scm")
	fixture := filepath.Join(repoRoot, "audit", "testdata", "axis-b-fixture-manifest.scm")

	for _, path := range []string{script, fixture} {
		_, err := os.Stat(path)
		c.Assert(err, qt.IsNil, qt.Commentf("required file missing: %s", path))
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
		"WILE_AXIS_B_MANIFEST="+fixture,
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
