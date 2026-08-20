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

// Per-iteration cell identity for a loop parameter rebound in place by
// OpSelfTailCall (flat-closure arc, design §5.1-§5.2).
//
// OpSelfTailCall rebinds the parameter frame's slots WITHOUT re-running the
// binder: it drains the evaluated arguments, stores them over the old slot
// values, and jumps to pc=0. A parameter that Pass 2 boxed would therefore have
// its cell REUSED across iterations — every closure the loop builds sharing one
// — were it not for where the cell is installed.
//
// THE MECHANISM IS THE PLACEMENT, and it is the only thing standing between this
// arc and a silent per-iteration semantics break. compileBody emits OpBoxSlot at
// pc 0..k (compile_closure.go, "The ops land at pc 0..k"), which is exactly where
// OpSelfTailCall jumps back to, so re-entering the body allocates a FRESH cell
// per iteration. See OperationBoxSlot's doc comment for the same statement from
// the opcode's side.
//
// Nothing else in the suite asserts it. The fixtures here evaluate to a
// different list under a shared cell, so a regression surfaces as a wrong value
// rather than as a missing optimization.
//
// WHY THE map IS OUTSIDE THE LOOP. A call to map inside the loop body refuses
// the whole rewrite at bodyCalleesAllCaptureSafe — map is stamped
// InvokesProcedure, so it is not capture-safe — and a fixture that refuses there
// never reaches OpSelfTailCall at all, whatever the escaping-closure clause
// says. Applying the collected closures after the loop returns keeps the body's
// callees to =, +, cons and the self call.
//
// WHY pkg/wile AND NOT pkg/machine: nothing arms under the internal test
// helpers, which yield a mutable top level with unstamped primitives. See the
// header of flat_closure_ratchet_test.go for the measurement.

package wile

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/machine"
)

// countBoxSlotOps counts the OpBoxSlot instructions in a compiled expression's
// whole template tree.
func countBoxSlotOps(t *testing.T, code string) int {
	t.Helper()
	ctx := context.Background()
	engine, err := NewEngine(ctx)
	if err != nil {
		t.Fatalf("new engine: %v", err)
	}
	expr, err := engine.Parse(ctx, code)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	compiled, err := engine.Compile(ctx, expr)
	if err != nil {
		t.Fatalf("compile: %v", err)
	}
	return countOpInTemplateTree(
		compiled.template, machine.OpBoxSlot, map[*machine.NativeTemplate]bool{})
}

// evalThroughEngine evaluates code on a default Engine and returns its printed
// form. Default options, so the top level is immutable and the base primitives
// carry the Stable stamp OpSelfTailCall's callee check needs.
func evalThroughEngine(t *testing.T, code string) string {
	t.Helper()
	ctx := context.Background()
	engine, err := NewEngine(ctx)
	if err != nil {
		t.Fatalf("new engine: %v", err)
	}
	got, err := engine.EvalMultiple(ctx, code)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	return got.SchemeString()
}

// selfTailBoxedLoop is the boxed fixture: i is captured by the per-iteration
// closure AND assigned through it, which is Pass 2's conjunction, so i holds a
// cell.
//
// The closure MUTATES rather than merely reading, so the assertion discriminates
// on cell identity and not merely on the value the loop left behind: each
// closure adds 100 to whatever its own cell holds. One cell per iteration means
// the k-th closure sees k; a shared cell means they all read the same number and
// the additions compound in call order.
const selfTailBoxedLoop = `(map (lambda (f) (f))
                             (let loop ((i 0) (acc '()))
                               (if (= i 3)
                                   acc
                                   (loop (+ i 1)
                                         (cons (lambda () (set! i (+ i 100)) i) acc)))))`

// selfTailUnboxedLoop is design §5.1's control: the same loop with a read-only
// capture. The recursive call's rebind is not a set!, so captured alone leaves i
// unboxed and each closure copies the value at creation. This is why §5.1 works
// without §5.2's fix, and it must keep working after it.
const selfTailUnboxedLoop = `(map (lambda (f) (f))
                               (let loop ((i 0) (acc '()))
                                 (if (= i 3)
                                     acc
                                     (loop (+ i 1) (cons (lambda () i) acc)))))`

// TestSelfTailCallFreshBoxPerIteration asserts that a boxed loop parameter gets
// a NEW cell on every rebind.
//
// acc is built by consing, so map walks the closures newest-first: the closure
// made when i was 2 runs first and reports 102.
func TestSelfTailCallFreshBoxPerIteration(t *testing.T) {
	// Vacuity guard. A fixture that does not box is not testing anything, and
	// this one boxes only because the capture and the set! are BOTH present —
	// either alone leaves i unboxed and this silently becomes the control below.
	boxes := countBoxSlotOps(t, selfTailBoxedLoop)
	if boxes == 0 {
		t.Fatal("the fixture compiles to no OpBoxSlot, so it cannot be testing " +
			"per-iteration cell identity — restore the captured-and-assigned shape")
	}
	t.Logf("OpBoxSlot in fixture: %d", boxes)

	got := evalThroughEngine(t, selfTailBoxedLoop)
	const want = "(102 101 100)"
	if got != want {
		t.Errorf("boxed loop parameter evaluates to %s, want %s; closures reporting "+
			"a common base means the iterations SHARED one cell, which is OpBoxSlot "+
			"having left the pc=0 re-entry window OpSelfTailCall jumps back to",
			got, want)
	}
}

// TestSelfTailCallUnboxedPerIterationCapture is the negative control: a
// captured-but-unassigned loop parameter must stay unboxed and must still give
// each iteration's closure its own value.
func TestSelfTailCallUnboxedPerIterationCapture(t *testing.T) {
	boxes := countBoxSlotOps(t, selfTailUnboxedLoop)
	if boxes != 0 {
		t.Errorf("captured-but-unassigned loop parameter compiled to %d OpBoxSlot, "+
			"want 0 — a copy is correct here and a cell is pure overhead on an "+
			"inner loop", boxes)
	}

	got := evalThroughEngine(t, selfTailUnboxedLoop)
	const want = "(2 1 0)"
	if got != want {
		t.Errorf("unboxed loop parameter evaluates to %s, want %s", got, want)
	}
}
