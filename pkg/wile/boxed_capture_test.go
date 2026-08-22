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

// The VALUE half of flat-closure boxing.
//
// Box-sharing is observable: two closures over the same boxed variable must see
// each other's writes, and a copy would not. Every fixture here passes with
// LINKED closures too — they share the frame — which is exactly why they belong
// in the tree BEFORE the representation changes. They are the regression guard
// for the phase that replaces the frame pointer with copied values, not a red
// test for the phase that introduced them.

package wile

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/machine"
)

// TestBoxedCaptureIsShared drives the shapes a value-copying closure would get
// wrong.
func TestBoxedCaptureIsShared(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{
			// Two closures, one cell. A copy gives 0.
			name: "two closures share one boxed cell",
			code: `(let ((x 0))
			         (let ((inc (lambda () (set! x (+ x 1)) x))
			               (get (lambda () x)))
			           (inc) (inc) (get)))`,
			want: "2",
		},
		{
			// §2.2.1 defect 1: an immediately-applied lambda is not an ESCAPING
			// closure, so a depth-qualified boxing predicate left `a` unboxed
			// and this evaluated to 0.
			name: "set! through an immediately-applied lambda is visible",
			code: `(let ((a 0)) ((lambda (x) (set! a x)) 7) a)`,
			want: "7",
		},
		{
			// §2.2.1 defect 2, and the one that forced the boxing decision to
			// move to the binder: the set! is in the BINDER's own body, at no
			// closure depth at all, and is just as invisible to a closure that
			// already copied the value.
			name: "set! in the binder's body reaches an earlier capture",
			code: `(let ((a 0)) (let ((f (lambda () a))) (set! a 9) (f)))`,
			want: "9",
		},
		{
			// A lambda PARAMETER, not a let binding: the binder is the apply
			// frame, and the cell has to be installed at the body's first
			// instruction.
			name: "a boxed parameter is shared with its closures",
			code: `(((lambda (n) (lambda () (set! n (+ n 5)) n)) 1))`,
			want: "6",
		},
		{
			// An internal define's slot is predeclared to void and boxed with
			// the parameters, so the define's own store must write THROUGH the
			// cell rather than replacing it.
			name: "an internal define's store goes through its box",
			code: `((lambda ()
			          (define counter 0)
			          (define bump (lambda () (set! counter (+ counter 1))))
			          (bump) (bump)
			          counter))`,
			want: "2",
		},
		{
			// letrec: the cell must exist before any init runs, because the
			// init that closes over the binding runs first.
			name: "a letrec binding's cell exists before its init runs",
			code: `(letrec ((acc 0)
			                (add (lambda (n) (set! acc (+ acc n)) acc)))
			         (add 3) (add 4))`,
			want: "7",
		},
		{
			// let* binders are created one at a time, so their cells are
			// installed one at a time. A later init closing over an earlier
			// binding must still see the cell.
			name: "a let* binding captured by a later init",
			code: `(let* ((a 1)
			              (f (lambda () (set! a (+ a 10)) a)))
			         (f) (f))`,
			want: "21",
		},
		{
			// Each iteration's closure must get its own cell. The box ops sit at
			// pc 0, which is where OpSelfTailCall jumps back to, so re-entry
			// re-allocates.
			name: "a boxed loop parameter gets a fresh cell per iteration",
			code: `(let loop ((i 0) (acc '()))
			         (if (= i 3)
			             (map (lambda (f) (f)) acc)
			             (loop (+ i 1) (cons (lambda () (set! i i) i) acc))))`,
			want: "(2 1 0)",
		},
		{
			// The unboxed control from design §5.1: a named-let parameter
			// rebound by the recursive call is NOT a set!, so it stays unboxed
			// and copied. This must keep working.
			name: "an unboxed loop parameter is captured per iteration",
			code: `(let loop ((i 0) (acc '()))
			         (if (= i 3)
			             (map (lambda (f) (f)) acc)
			             (loop (+ i 1) (cons (lambda () i) acc))))`,
			want: "(2 1 0)",
		},
	}
	ctx := context.Background()
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			engine, err := NewEngine(ctx)
			if err != nil {
				t.Fatalf("new engine: %v", err)
			}
			got, err := engine.EvalMultiple(ctx, tc.code)
			if err != nil {
				t.Fatalf("eval: %v", err)
			}
			if got.SchemeString() != tc.want {
				t.Errorf("= %s, want %s", got.SchemeString(), tc.want)
			}
		})
	}
}

// TestBoxedCaptureThroughOpaqueSubtree drives the set! the boxing pass could not
// see at all.
//
// newRefIndex enumerates occurrences with validate.WalkSubExprs, which reports a
// quasiquote template, a cond-expand and every other passthrough form CHILDLESS
// — correctly, since they hold no VALIDATED children, but the pass then read
// that as "nothing in there". A set! concealed by one left the binder unboxed,
// so the flat closure copied the raw value and kept it. Silent wrong answer:
// Chez and Racket both give the boxed one.
//
// All three binder spellings are here because the review's filing claimed the
// internal-define spelling was saved by bodyBindersOfRegion's forcesBox and it
// is not. The fourth shape is not a stale value but a hard VM error on legal
// source — the named-let binder is set! through a quasiquote, so codegen emits a
// free-slot store against an unboxed slot.
func TestBoxedCaptureThroughOpaqueSubtree(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{
			// The passthrough spelling: cond-expand's selected clause is compiled
			// in its own unit, so its set! is invisible here.
			name: "set! inside a cond-expand clause reaches an earlier capture",
			code: `(define (make-counter)
			         (let ((counter 0))
			           (let ((get (lambda () counter)))
			             (cond-expand (r7rs (set! counter 10)) (else (set! counter 20)))
			             get)))
			       ((make-counter))`,
			want: "10",
		},
		{
			// The quasiquote spelling, let binder.
			name: "set! inside a quasiquote unquote, let binder",
			code: `(define (mk) (let ((n 0)) (let ((f (lambda () n))) ` + "`" + `(,(set! n 99)) f)))
			       ((mk))`,
			want: "99",
		},
		{
			// Lambda parameter, no enclosing let.
			name: "set! inside a quasiquote unquote, lambda parameter",
			code: `(define (mk3 n) (let ((f (lambda () n))) ` + "`" + `(,(set! n 99)) f))
			       ((mk3 0))`,
			want: "99",
		},
		{
			// Internal define — the spelling the filing said forcesBox saved.
			name: "set! inside a quasiquote unquote, internal define",
			code: `(define (mk4) (let ((n 0)) (define (f) n) ` + "`" + `(,(set! n 99)) f))
			       ((mk4))`,
			want: "99",
		},
		{
			// Not a stale value: unfixed this is
			// "store-free: free slot 0 holds *machine.MachineClosure, not a box".
			name: "set! of a named-let binder through a quasiquote is not a VM error",
			code: `(let loop ((n 3))
			         (begin ` + "`" + `(,(set! loop (lambda (k) k)))
			                (if (= n 0) 0 (+ 0 (loop (- n 1))))))`,
			want: "2",
		},
		{
			// Control: the same set! written where the pass can see it. Without
			// it the arms above could pass by boxing everything unconditionally.
			name: "control: a visible set! still answers, and still boxes",
			code: `(define (mk5) (let ((n 0)) (let ((f (lambda () n))) (set! n 99) f)))
			       ((mk5))`,
			want: "99",
		},
	}
	ctx := context.Background()
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			engine, err := NewEngine(ctx)
			if err != nil {
				t.Fatalf("new engine: %v", err)
			}
			got, err := engine.EvalMultiple(ctx, tc.code)
			if err != nil {
				t.Fatalf("eval: %v", err)
			}
			if got.SchemeString() != tc.want {
				t.Errorf("= %s, want %s", got.SchemeString(), tc.want)
			}
		})
	}
}

// TestBoxedSlotNeverEscapesToScheme is the containment assertion. A box is a
// first-class Scheme value (#&), so a missed unbox does not crash — it returns
// a #<box> where the program wanted a number, and only an equality assertion on
// the printed form notices.
func TestBoxedSlotNeverEscapesToScheme(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{
			name: "reading a boxed variable yields its value",
			code: `(let ((x 41)) (lambda () (set! x (+ x 1))) x)`,
			want: "41",
		},
		{
			name: "a boxed variable in operator position is applied, not returned",
			code: `(let ((f (lambda (n) (* n 2))))
			         (lambda () (set! f (lambda (n) n)))
			         (f 21))`,
			want: "42",
		},
		{
			name: "a boxed variable passed as an argument arrives unboxed",
			code: `(let ((x 5)) (lambda () (set! x 6)) (+ x 1))`,
			want: "6",
		},
		{
			name: "box? is false for an ordinary captured-and-assigned variable",
			code: `(let ((x 1)) (lambda () (set! x 2)) (box? x))`,
			want: "#f",
		},
		{
			// compileSyntaxTemplateToOps emitted a bare OpLoadLocal for a local
			// it resolves as a pattern variable, bypassing emitLocalLoad and so
			// the unbox. The control below is the same source without the set!:
			// it answered 5 while this one answered #&5.
			name: "a boxed local read through a syntax template arrives unboxed",
			code: `(syntax->datum (let ((x 5)) (lambda () (set! x 1)) (syntax x)))`,
			want: "5",
		},
		{
			name: "control: the same read with nothing to box",
			code: `(syntax->datum (let ((x 5)) (syntax x)))`,
			want: "5",
		},
		{
			// The shape that surfaced it: a nested quasisyntax expands to a
			// (syntax x) for the inner unsyntax's argument.
			name: "a boxed local under a nested quasisyntax arrives unboxed",
			code: `(syntax->datum
			         (let ((x 5)) (lambda () (set! x 1)) (quasisyntax (quasisyntax #,x))))`,
			want: "(quasisyntax (unsyntax 5))",
		},
	}
	ctx := context.Background()
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			engine, err := NewEngine(ctx)
			if err != nil {
				t.Fatalf("new engine: %v", err)
			}
			got, err := engine.EvalMultiple(ctx, tc.code)
			if err != nil {
				t.Fatalf("eval: %v", err)
			}
			if got.SchemeString() != tc.want {
				t.Errorf("= %s, want %s", got.SchemeString(), tc.want)
			}
		})
	}
}

// TestBoxedSlotEmitsTheProtocol pins that the fixtures above are actually
// exercising boxing rather than passing because nothing was boxed.
//
// Every value assertion in this file passes with the boxing codegen deleted
// outright — linked closures share the frame, so the values are right either
// way. This is the test that fails in that case.
func TestBoxedSlotEmitsTheProtocol(t *testing.T) {
	ctx := context.Background()
	engine, err := NewEngine(ctx)
	if err != nil {
		t.Fatalf("new engine: %v", err)
	}
	const code = `(let ((x 0)) (lambda () (set! x (+ x 1))) x)`
	expr, err := engine.Parse(ctx, code)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	compiled, err := engine.Compile(ctx, expr)
	if err != nil {
		t.Fatalf("compile: %v", err)
	}
	for _, op := range []machine.OpCode{machine.OpBoxSlot, machine.OpUnbox, machine.OpStoreThroughBox} {
		if !templateTreeHasOpcode(compiled.template, op) {
			t.Errorf("compiled tree has no %s — the boxing protocol is not being "+
				"emitted, so every value assertion in this file is vacuous", op)
		}
	}
}
