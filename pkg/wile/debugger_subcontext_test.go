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

package wile_test

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// TestDebuggerReachesLoadedCode pins Wave 3 item 13's 70b. load and eval run
// their freshly compiled template on a context built by NewSubContextWithTemplate,
// which copies twelve parent fields and used to omit the debugger, so every
// breakpoint inside a loaded file was silently inert. Observed at 003b3353: the
// load arm fired 0 times while the identical calls made from the top-level
// context fired repeatedly.
//
// The control arm is in the same test on purpose: it is what makes the load arm
// discriminating rather than merely non-zero, and it is measured from the same
// breakpoint on the same source line of the same file.
func TestDebuggerReachesLoadedCode(t *testing.T) {
	ctx := context.Background()
	dir := t.TempDir()
	path := filepath.Join(dir, "bp.scm")
	// Line 2 is the procedure body: one break per call, four calls.
	src := "(define (bp-target x)\n" +
		"  (+ x 1))\n" +
		"(bp-target 1)\n" +
		"(bp-target 2)\n" +
		"(bp-target 3)\n" +
		"(bp-target 4)\n"
	err := os.WriteFile(path, []byte(src), 0o600)
	qt.Assert(t, err, qt.IsNil)

	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)

	hits := 0
	dbg := wile.NewDebugger()
	dbg.SetBreakpoint(path, 2, 0)
	dbg.OnBreak(func(_ values.DebugState, _ *wile.BreakpointInfo) {
		hits++
	})
	eng.SetDebugger(dbg)

	_, err = eng.EvalMultiple(ctx, `(load "`+path+`")`)
	qt.Assert(t, err, qt.IsNil)
	loadHits := hits
	qt.Assert(t, loadHits > 0, qt.IsTrue,
		qt.Commentf("breakpoint inside a loaded file never fired (0 at 003b3353)"))

	// Control: the same procedure, the same breakpoint, called from the
	// top-level context, which has always carried the debugger.
	hits = 0
	_, err = eng.EvalMultiple(ctx, `(begin (bp-target 5) (bp-target 6) (bp-target 7) (bp-target 8))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, hits, qt.Equals, loadHits,
		qt.Commentf("four calls under load must break as often as four calls at top level"))
}
