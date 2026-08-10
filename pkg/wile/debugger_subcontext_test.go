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

// oneLineBodySource defines a procedure whose entire body is one source line,
// so a breakpoint on line 2 sits on several instructions of the same line.
const oneLineBodySource = "(define (foo x)\n" +
	"  (+ x x))\n"

// newDebuggedEngine builds a KitchenSink engine, writes src to a file in a
// fresh temp dir, loads it, and returns the engine, the file path, and a
// debugger already attached. Breakpoints are set by the caller AFTER the load
// so that defining the procedure cannot itself fire them.
func newDebuggedEngine(t *testing.T, src string) (*wile.Engine, string, *wile.Debugger) {
	t.Helper()
	ctx := context.Background()
	path := filepath.Join(t.TempDir(), "bp.scm")
	err := os.WriteFile(path, []byte(src), 0o600)
	qt.Assert(t, err, qt.IsNil)

	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)

	dbg := wile.NewDebugger()
	eng.SetDebugger(dbg)

	_, err = eng.EvalMultiple(ctx, `(load "`+path+`")`)
	qt.Assert(t, err, qt.IsNil)
	return eng, path, dbg
}

// TestDebuggerSuspendsAndTheVerdictControlsExecution is GATE (1) for Wave 3
// item 13a: it must fail before the break-interrupt suspension lands and pass
// after.
//
// The discriminating assertion is that the handler's VERDICT changes what the
// program computes. Asserting a non-nil CurrentLocation or a non-empty
// FormatStackTrace from INSIDE the callback would be vacuous: the render-only
// callback is invoked inline from the live MachineContext, so both already
// worked at dfd8e230 (the REPL printed "Breakpoint 0 hit at bp.scm:2:5"). Only
// a callback that can stop the VM can change the result.
//
// Arm A additionally pins that BreakAbandon routes through the ABORT path
// rather than simply returning an error: the dynamic-wind after-thunk between
// the break point and the top level must still run.
func TestDebuggerSuspendsAndTheVerdictControlsExecution(t *testing.T) {
	ctx := context.Background()

	t.Run("abandon", func(t *testing.T) {
		eng, path, dbg := newDebuggedEngine(t, oneLineBodySource)
		fires := 0
		dbg.OnBreakSuspend(func(_ values.DebugState, _ *wile.BreakpointInfo) wile.BreakAction {
			fires++
			return wile.BreakAbandon
		})
		dbg.SetBreakpoint(path, 2, 0)

		// The flag is a vector cell, not a set! on a top-level name: the default
		// immutable top level refuses to compile the latter.
		_, err := eng.EvalMultiple(ctx, `(define ran (make-vector 1 #f))`)
		qt.Assert(t, err, qt.IsNil)

		val, err := eng.EvalMultiple(ctx,
			`(dynamic-wind (lambda () #f) (lambda () (foo 21)) (lambda () (vector-set! ran 0 #t)))`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, val.SchemeString(), qt.Not(qt.Equals), "42",
			qt.Commentf("abandoning the break must discard the computation, not run it to 42"))
		qt.Assert(t, fires, qt.Equals, 1)

		ran, err := eng.EvalMultiple(ctx, `(vector-ref ran 0)`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, ran.SchemeString(), qt.Equals, "#t",
			qt.Commentf("abandon aborts to the break prompt, so after-thunks below it still run"))
	})

	t.Run("continue", func(t *testing.T) {
		eng, path, dbg := newDebuggedEngine(t, oneLineBodySource)
		fires := 0
		dbg.OnBreakSuspend(func(_ values.DebugState, _ *wile.BreakpointInfo) wile.BreakAction {
			fires++
			return wile.BreakContinue
		})
		dbg.SetBreakpoint(path, 2, 0)

		val, err := eng.EvalMultiple(ctx, `(foo 21)`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, val.SchemeString(), qt.Equals, "42")
		qt.Assert(t, fires, qt.Equals, 1)
	})
}

// TestBreakpointFiresOncePerSourceLine is GATE (2) for Wave 3 item 13a: it must
// fail before the CheckBreakpoint de-duplication lands and pass after.
//
// A breakpoint names a source LINE, so one entry to that line is one stop. At
// dfd8e230 CheckBreakpoint fired per INSTRUCTION carrying the line: one call to
// the one-line body below produced three stops (columns 5, 7 and 3) and drove
// HitCount to 3, which is also what ,list reported to the user.
func TestBreakpointFiresOncePerSourceLine(t *testing.T) {
	ctx := context.Background()
	eng, path, dbg := newDebuggedEngine(t, oneLineBodySource)

	fires := 0
	dbg.OnBreak(func(_ values.DebugState, _ *wile.BreakpointInfo) {
		fires++
	})
	dbg.SetBreakpoint(path, 2, 0)

	_, err := eng.EvalMultiple(ctx, `(foo 21)`)
	qt.Assert(t, err, qt.IsNil)

	qt.Assert(t, fires, qt.Equals, 1,
		qt.Commentf("one call entering line 2 once must stop once (3 at dfd8e230)"))
	bps := dbg.Breakpoints()
	qt.Assert(t, len(bps), qt.Equals, 1)
	qt.Assert(t, bps[0].HitCount, qt.Equals, 1,
		qt.Commentf("HitCount is what ,list prints (3 at dfd8e230)"))
}
