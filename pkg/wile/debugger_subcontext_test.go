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
	"errors"
	"os"
	"path/filepath"
	"strings"
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

// TestDebuggerReachesLoadedCodeWithSuspensionArmed is a REGRESSION PIN, not a
// gate: it is green on both sides of item 13a. It records the delivered scope.
//
// Suspension does not reach load and eval. The break boundary is installed once
// per run on the TOP-LEVEL chain; a sub-context built by
// NewSubContextWithTemplate inherits the debugger but not the boundary, so a
// breakpoint inside a loaded file still takes the render-only fallback. This
// re-runs 70b's count with a suspension handler installed and pins that the
// count is unchanged — i.e. the two features compose by falling back, not by
// suspending.
func TestDebuggerReachesLoadedCodeWithSuspensionArmed(t *testing.T) {
	ctx := context.Background()
	path := filepath.Join(t.TempDir(), "bp.scm")
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

	renderHits := 0
	suspendHits := 0
	dbg := wile.NewDebugger()
	dbg.SetBreakpoint(path, 2, 0)
	dbg.OnBreak(func(_ values.DebugState, _ *wile.BreakpointInfo) {
		renderHits++
	})
	dbg.OnBreakSuspend(func(_ values.DebugState, _ *wile.BreakpointInfo) wile.BreakAction {
		suspendHits++
		return wile.BreakContinue
	})
	eng.SetDebugger(dbg)

	_, err = eng.EvalMultiple(ctx, `(load "`+path+`")`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, renderHits, qt.Equals, 4,
		qt.Commentf("four calls inside the loaded file, all through the fallback"))
	qt.Assert(t, suspendHits, qt.Equals, 0,
		qt.Commentf("a load sub-context carries no break boundary, so it cannot suspend"))
}

// TestBreakStateOutlivesTheEvaluation is a REGRESSION PIN, not a gate. The
// debugger used to hand back the live *MachineContext, which
// ReleaseTopLevelContext returns to the pool and zeroes, so every inspection
// made after the run read a blank context. The recorded state is now a snapshot
// and must still answer once the evaluation that produced it is over.
func TestBreakStateOutlivesTheEvaluation(t *testing.T) {
	ctx := context.Background()
	// No suspend handler: this is the render-only path, where TriggerBreak
	// records the snapshot inline.
	eng, path, dbg := newDebuggedEngine(t, oneLineBodySource)
	dbg.SetBreakpoint(path, 2, 0)

	_, err := eng.EvalMultiple(ctx, `(foo 21)`)
	qt.Assert(t, err, qt.IsNil)

	state := dbg.CurrentState()
	qt.Assert(t, state, qt.IsNotNil)
	loc := state.CurrentLocation()
	qt.Assert(t, loc, qt.IsNotNil,
		qt.Commentf(",where reported 'No source location available' at dfd8e230"))
	qt.Assert(t, loc.Line, qt.Equals, 2)
	qt.Assert(t, state.FormatStackTrace(20), qt.Not(qt.Equals), "",
		qt.Commentf(",backtrace reported 'Empty stack trace' at dfd8e230"))
}

// TestStepOverSurvivesTheRoundTrip is a REGRESSION PIN, not a gate: step-over
// was already depth-keyed before item 13a, and the point is that the
// capture/resume round trip does not break it the way it broke step-out.
func TestStepOverSurvivesTheRoundTrip(t *testing.T) {
	ctx := context.Background()
	src := "(define (inner x)\n" +
		"  (+ x 1))\n" +
		"(define (outer x)\n" +
		"  (inner x)\n" +
		"  (+ x 100))\n"
	eng, path, dbg := newDebuggedEngine(t, src)

	var lines []int
	dbg.OnBreakSuspend(func(state values.DebugState, _ *wile.BreakpointInfo) wile.BreakAction {
		loc := state.CurrentLocation()
		if loc != nil {
			lines = append(lines, loc.Line)
		}
		if len(lines) == 1 {
			return wile.BreakNext
		}
		return wile.BreakContinue
	})
	dbg.SetBreakpoint(path, 4, 0)

	val, err := eng.EvalMultiple(ctx, `(outer 5)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, val.SchemeString(), qt.Equals, "105")
	qt.Assert(t, len(lines) >= 2, qt.IsTrue,
		qt.Commentf("step-over must produce a second stop, got stops %v", lines))
	qt.Assert(t, lines[1], qt.Equals, 5,
		qt.Commentf("step-over must run (inner x) to completion and stop on line 5"))
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
// worked at dfd8e230 (the REPL printed "Breakpoint 0 hit at <file>:2:5"). Only
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

// TestStepOutDoesNotDegenerateIntoStepInto is GATE (3) for Wave 3 item 13a: it
// must fail before ShouldStep's step-out arm is re-keyed onto call depth and
// pass after.
//
// Step-out was keyed on the frame POINTER stashed at the stop. Suspension made
// that key worthless: SliceContinuationAt deep-copies every frame, so resuming
// hands the VM a chain of Copy()s and `mc.cont != stepFrame` is already true at
// the first opcode after the resume, which is step-INTO behaviour.
//
// inner's body spans TWO source lines, and that is the whole discriminating
// power of this test. With a one-line body the only line left to stop on after
// the break is the caller's, so every candidate step-out arm — `true`,
// `CurrentSource() != nil`, `<=`, `<` — lands on it and the gate passes
// vacuously: the one-stop-per-source-line cursor, not the depth comparison, is
// what forbids stopping twice on line 2. Line 3 is reachable, carries source,
// and sits at inner's own depth, so only a comparison that is STRICTLY shallower
// than the break's depth skips it.
func TestStepOutDoesNotDegenerateIntoStepInto(t *testing.T) {
	ctx := context.Background()
	src := "(define (inner x)\n" +
		"  (let ((y (+ x 1)))\n" +
		"    (* y 2)))\n" +
		"(define (outer x)\n" +
		"  (+ (inner x) 100))\n"
	eng, path, dbg := newDebuggedEngine(t, src)

	var lines []int
	dbg.OnBreakSuspend(func(state values.DebugState, _ *wile.BreakpointInfo) wile.BreakAction {
		line := 0
		loc := state.CurrentLocation()
		if loc != nil {
			line = loc.Line
		}
		lines = append(lines, line)
		if len(lines) == 1 {
			return wile.BreakFinish
		}
		return wile.BreakContinue
	})
	dbg.SetBreakpoint(path, 2, 0)

	val, err := eng.EvalMultiple(ctx, `(outer 5)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, val.SchemeString(), qt.Equals, "112")

	qt.Assert(t, len(lines) >= 2, qt.IsTrue,
		qt.Commentf("finishing inner must produce a second stop, got stops %v", lines))
	qt.Assert(t, lines[1], qt.Equals, 5,
		qt.Commentf("finish must land back in outer on line 5; line 3 is inner's own "+
			"second body line and stopping there is step-into, got stops %v", lines))
}

// TestBreakpointFiresOncePerSourceLine is GATE (2) for Wave 3 item 13a: it must
// fail before the CheckBreakpoint de-duplication lands and pass after.
//
// A breakpoint names a source LINE, so one entry to that line is one stop. At
// dfd8e230 CheckBreakpoint fired per INSTRUCTION carrying the line: one call to
// the one-line body below produced four stops and drove HitCount to 4, which is
// also what ,list reported to the user.
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
		qt.Commentf("one call entering line 2 once must stop once (4 at dfd8e230)"))
	bps := dbg.Breakpoints()
	qt.Assert(t, len(bps), qt.Equals, 1)
	qt.Assert(t, bps[0].HitCount, qt.Equals, 1,
		qt.Commentf("HitCount is what ,list prints (4 at dfd8e230)"))
}

// TestEveryBreakpointOnALineFires pins that the once-per-line de-duplication is
// keyed on the BREAKPOINT, not on the line.
//
// Keyed on the line, the first breakpoint to fire set the cursor and every other
// breakpoint on that line went inert for the whole entry — including one on a
// column not yet reached. Which one survived was Go's map iteration order over
// the breakpoint table, so the loser was not even stable. Measured at 2e599c64:
// callback ids [0], hits 1 and 0.
func TestEveryBreakpointOnALineFires(t *testing.T) {
	ctx := context.Background()
	eng, path, dbg := newDebuggedEngine(t, "(define (foo x)\n  (+ (* x 2) (* x 3)))\n")

	var ids []int
	dbg.OnBreak(func(_ values.DebugState, bp *wile.BreakpointInfo) {
		if bp != nil {
			ids = append(ids, bp.ID)
		}
	})
	// Two columns of line 2: the two multiplications. Distinct instructions, so
	// a per-breakpoint cursor reaches both and a per-line cursor reaches one.
	first := dbg.SetBreakpoint(path, 2, 6)
	second := dbg.SetBreakpoint(path, 2, 14)

	_, err := eng.EvalMultiple(ctx, `(foo 21)`)
	qt.Assert(t, err, qt.IsNil)

	qt.Assert(t, len(ids), qt.Equals, 2,
		qt.Commentf("both breakpoints on line 2 must stop, got ids %v", ids))
	hits := map[int]int{}
	for _, bp := range dbg.Breakpoints() {
		hits[bp.ID] = bp.HitCount
	}
	qt.Assert(t, hits[first], qt.Equals, 1)
	qt.Assert(t, hits[second], qt.Equals, 1,
		qt.Commentf("the later column must not be masked by the earlier one"))
}

// TestBreakSnapshotHonoursMaxDepth pins values.DebugState's "walking at most
// maxDepth frames" for the snapshot implementation a debugger UI actually
// receives.
//
// The snapshot rendered its trace once at capture time and ignored the
// parameter: measured at 2e599c64, FormatStackTrace returned the same 33 frame
// lines for 1, 5, 20 and 100 alike, so the REPL's ,backtrace printed 32 frames
// after asking for 20.
func TestBreakSnapshotHonoursMaxDepth(t *testing.T) {
	ctx := context.Background()
	src := "(define (deep n)\n" +
		"  (if (= n 0)\n" +
		"      0\n" +
		"      (+ 1 (deep (- n 1)))))\n"
	eng, path, dbg := newDebuggedEngine(t, src)

	dbg.OnBreakSuspend(func(_ values.DebugState, _ *wile.BreakpointInfo) wile.BreakAction {
		return wile.BreakContinue
	})
	dbg.SetBreakpoint(path, 3, 0)

	_, err := eng.EvalMultiple(ctx, `(deep 40)`)
	qt.Assert(t, err, qt.IsNil)

	state := dbg.CurrentState()
	qt.Assert(t, state, qt.IsNotNil)
	// A budget of 1 and a budget of 5 must differ, and neither may exceed its
	// own budget. The trailing "... N more frames ..." line is the truncation
	// marker, not a frame, so each render carries one of them.
	for _, budget := range []int{1, 5, 20} {
		frames := countTraceFrames(state.FormatStackTrace(budget))
		qt.Assert(t, frames, qt.Equals, budget,
			qt.Commentf("FormatStackTrace(%d) walked %d frames", budget, frames))
	}
}

// countTraceFrames counts real frame lines in a rendered trace. The truncation
// marker is itself a StackFrame, so it renders with the same "  at " prefix and
// has to be excluded by its text.
func countTraceFrames(trace string) int {
	q := 0
	for line := range strings.SplitSeq(trace, "\n") {
		if !strings.HasPrefix(line, "  at ") {
			continue
		}
		if strings.Contains(line, "more frames") {
			continue
		}
		q++
	}
	return q
}

// TestUnarmedDebuggerCostsNoFrame pins that a registered suspension handler
// alone does not install the break boundary.
//
// The REPL registers its handler for the whole session, so arming on the
// handler alone put a break prompt frame on EVERY evaluation typed at the
// prompt: a locationless <anonymous> at the bottom of every stack trace, and one
// unit of the call-depth budget spent, for a user who never set a breakpoint.
// Measured at 2e599c64: `(car (list))` reported three frames where the same
// engine without a debugger reported two.
func TestUnarmedDebuggerCostsNoFrame(t *testing.T) {
	ctx := context.Background()

	traceOf := func(t *testing.T, arm func(*wile.Debugger)) string {
		t.Helper()
		eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
		qt.Assert(t, err, qt.IsNil)
		if arm != nil {
			dbg := wile.NewDebugger()
			arm(dbg)
			eng.SetDebugger(dbg)
		}
		_, err = eng.EvalMultiple(ctx, `(car (list))`)
		qt.Assert(t, err, qt.IsNotNil)
		var rt *wile.RuntimeError
		qt.Assert(t, errors.As(err, &rt), qt.IsTrue)
		return rt.StackTrace
	}

	bare := traceOf(t, nil)
	qt.Assert(t, countTraceFrames(bare) > 0, qt.IsTrue,
		qt.Commentf("the control arm must produce a trace to compare against"))

	handlerOnly := traceOf(t, func(dbg *wile.Debugger) {
		dbg.OnBreakSuspend(func(_ values.DebugState, _ *wile.BreakpointInfo) wile.BreakAction {
			return wile.BreakContinue
		})
	})
	qt.Assert(t, handlerOnly, qt.Equals, bare,
		qt.Commentf("a suspension handler with nothing armed must not change the stack"))

	// Arming a breakpoint installs the boundary, and the boundary must still not
	// show up as a frame: it is VM scaffolding, not a call the program made.
	armed := traceOf(t, func(dbg *wile.Debugger) {
		dbg.OnBreakSuspend(func(_ values.DebugState, _ *wile.BreakpointInfo) wile.BreakAction {
			return wile.BreakContinue
		})
		dbg.SetBreakpoint("no-such-file.scm", 1, 0)
	})
	qt.Assert(t, armed, qt.Equals, bare,
		qt.Commentf("the break boundary must be elided from stack traces"))
}
