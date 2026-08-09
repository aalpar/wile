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
	"testing"

	"github.com/aalpar/wile/pkg/wile"
)

// Gates for review wave 1 §1: syntax-case corrupts the frame for ordinary code.
//
// Three defects share one entry point:
//
//   - 15a — the pattern-variable frame is pushed on the matched path and popped
//     only on the fender-false cleanup edge, so everything after the form does
//     depth-relative local access against a frame one level too deep.
//   - 15b — the epilogue (ClearSyntaxCaseInput) sits after the clause body's
//     tail position, so a tail CALL in a clause body never reaches it and the
//     matcher state stays installed for a later stale (syntax ...).
//   - N1 — syntax-case is not reentrant: a nested syntax-case in a clause body
//     overwrites the enclosing form's state.
//   - N3 — extent asymmetry: (syntax a) has lexical extent (the escaping
//     closure captured the pattern-variable frame) while (syntax (a ...)) has
//     dynamic extent (it read a single per-MachineContext slot that the
//     epilogue nils). Pattern variables are one lexical binding, R6RS §12.4.
//
// Every case here is a single (begin ...) form on purpose. The 15b leak is
// bounded to one MachineContext and REPL top-level forms do not share one, so
// two separate Eval calls cannot see it.
func TestSyntaxCaseDoesNotCorruptTheEnclosingFrame(t *testing.T) {
	cases := []struct {
		name string
		src  string
		want string
	}{
		{
			// 15a. Observed at 003b3353: #'2 — `y` read the pattern variable's
			// slot through the leaked frame.
			name: "trailing local read after syntax-case",
			src: `(begin
                    (define (f stx y) (syntax-case stx () ((_ a) 1)) y)
                    (f #'(m 2) 99))`,
			want: "99",
		},
		{
			// 15a. Observed at 003b3353: panic "index out of range [2] with
			// length 2", surfaced as "RunResumable: recovered panic".
			name: "three trailing locals after syntax-case",
			src: `(begin
                    (define (f stx y)
                      (let ((p 1) (q 2) (r 3))
                        (syntax-case stx () ((_ a) 1))
                        (list p q r y)))
                    (f #'(m 2) 99))`,
			want: "(1 2 3 99)",
		},
		{
			// 15a through with-syntax, which is an ambient phase-0 compiler onto
			// syntax-case. Observed at 003b3353: #!void. The form MUST have at
			// least one binding: (with-syntax () ...) compiles the body directly
			// and is correct, so a test written that way passes without the fix.
			name: "trailing local read after with-syntax",
			src: `(begin
                    (define (f y) (with-syntax ((a #'2)) 1) y)
                    (f 99))`,
			want: "99",
		},
		{
			// N1. Observed at 003b3353: "syntax transformer produced no result:
			// unexpected nil value". The identical macro without the nested form
			// prints 99 (the negative control below).
			name: "nested syntax-case in a clause body",
			src: `(begin
                    (define-syntax m
                      (lambda (stx)
                        (syntax-case stx ()
                          ((_ a)
                           (let ((tmp (syntax-case (syntax (1 2)) () ((x y) (syntax x)))))
                             (syntax a))))))
                    (m 99))`,
			want: "99",
		},
		{
			// N3 E1. Observed at 003b3353: "syntax-case: no state on
			// MachineContext (no expansion in flight)" — ClearSyntaxCaseInput
			// nils the slot at the end of the DYNAMIC extent, so the escaping
			// closure has nothing to expand against.
			name: "escaping closure over an ellipsis template, non-tail body",
			src: `(begin
                    (define esc #f)
                    (define (f stx)
                      (syntax-case stx ()
                        ((_ a ...) (begin (set! esc (lambda () (syntax (a ...)))) 'done))))
                    (f #'(m 1 2))
                    (esc))`,
			want: "#'(#'1 #'2)",
		},
		{
			// 15b, re-specified. Identical to the E1 case except that the clause
			// body's tail form is a CALL, so control never reaches the epilogue.
			// Observed at 003b3353 this returned #'(#'1 #'2) by reading LEAKED
			// state while its non-tail twin above raised: the two disagreed. The
			// gate is that they agree, and agree on the right answer.
			//
			// The design's original assertion 4 here cannot fail and was
			// replaced by this pair: a bare (syntax (q)) outside any form
			// returns #'(#'q) identically with a tail body, a non-tail body, and
			// with no syntax-case in the file at all.
			name: "escaping closure over an ellipsis template, tail-call body",
			src: `(begin
                    (define esc #f)
                    (define (f stx)
                      (syntax-case stx ()
                        ((_ a ...) (list (set! esc (lambda () (syntax (a ...)))) 'done))))
                    (f #'(m 1 2))
                    (esc))`,
			want: "#'(#'1 #'2)",
		},
		{
			// Reentrancy, the ellipsis half of N1: an inner syntax-case runs to
			// completion inside the outer clause body, and the outer form's
			// ellipsis template still expands against the OUTER match. Under the
			// single per-MachineContext slot the inner form's
			// ClearSyntaxCaseInput destroyed the outer state.
			// No intervening `let`: a nested compile scope inside a clause body
			// does not carry the enclosing clause's pattern variables into
			// CompileSyntax, an INDEPENDENT defect filed as N4. Wile evaluates
			// arguments left to right, so the inner form still runs to
			// completion before the outer template expands.
			//
			// Observed at 003b3353: "syntax-case: no state on MachineContext
			// (no expansion in flight)" — the inner form's epilogue nilled the
			// one slot the outer form was still using.
			name: "nested syntax-case does not destroy the outer ellipsis state",
			src: `(begin
                    (define (f stx)
                      (syntax-case stx ()
                        ((_ a ...)
                         (list (syntax-case #'(7 8) () ((x y) (syntax x)))
                               (syntax (a ...))))))
                    (f #'(m 1 2)))`,
			want: "(#'7 #'(#'1 #'2))",
		},
		{
			// N3 E2, the working half of the asymmetry, asserted UNCHANGED. The
			// non-ellipsis path compiles to a local load, so the closure captured
			// the pattern-variable frame lexically and this already returns #'1.
			name: "escaping closure over a non-ellipsis template, non-tail body",
			src: `(begin
                    (define esc #f)
                    (define (f stx)
                      (syntax-case stx ()
                        ((_ a) (begin (set! esc (lambda () (syntax a))) 'done))))
                    (f #'(m 1))
                    (esc))`,
			want: "#'1",
		},
		{
			// Negative control: no bindings, so with-syntax compiles the body
			// directly and never reaches syntax-case. Correct at 003b3353.
			name: "control: with-syntax with no bindings",
			src: `(begin
                    (define (f y) (with-syntax () 1) y)
                    (f 99))`,
			want: "99",
		},
		{
			// Negative control: syntax-case as the sole body form. Correct at
			// 003b3353 — nothing follows the form, so nothing observes the leak.
			name: "control: syntax-case as the sole body form",
			src: `(begin
                    (define (f stx) (syntax-case stx () ((_ a) (syntax a))))
                    (f #'(m 7)))`,
			want: "#'7",
		},
		{
			// Negative control: the nested-syntax-case macro without the nesting.
			// Correct at 003b3353; pins that N1's repro isolates the nesting.
			name: "control: transformer with no nested syntax-case",
			src: `(begin
                    (define-syntax m
                      (lambda (stx)
                        (syntax-case stx ()
                          ((_ a) (let ((tmp 7)) (syntax a))))))
                    (m 99))`,
			want: "99",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			ctx := context.Background()
			engine, err := wile.NewEngine(ctx)
			if err != nil {
				t.Fatal(err)
			}
			got, err := engine.Eval(ctx, engine.MustParse(ctx, tc.src))
			if err != nil {
				t.Fatalf("eval: %v", err)
			}
			if got.SchemeString() != tc.want {
				t.Errorf("got %s, want %s", got.SchemeString(), tc.want)
			}
		})
	}
}

// A (syntax (a ...)) outside any syntax-case has no pattern-variable frame in
// scope and must raise rather than expand against whatever state a previous
// form left behind. This is 15b's visible symptom: at 003b3353 this SUCCEEDED,
// because the clause body's tail call skipped the epilogue that would have
// cleared the matcher.
func TestSyntaxEllipsisOutsideSyntaxCaseRaises(t *testing.T) {
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx)
	if err != nil {
		t.Fatal(err)
	}
	src := `(begin
              (define (f stx)
                (syntax-case stx ()
                  ((_ a ...) (list 'done))))
              (f #'(m 1 2))
              (syntax (q ...)))`
	got, err := engine.Eval(ctx, engine.MustParse(ctx, src))
	if err == nil {
		t.Fatalf("expected an error from (syntax (q ...)) outside any syntax-case, got %s", got.SchemeString())
	}
}

// End-to-end acceptance: a recursive syntax-list walk written the way real code
// writes one — `let loop`, an inner syntax-case per iteration, and the loop
// state read AFTER the form. That last part is what makes it discriminating:
// the plainer walk (nothing read after the form, base case tested outside
// syntax-case) already ran at 003b3353 and proves nothing.
//
// Observed at 003b3353: "+: expected number, got #'(#'2 #'3)" — `n` resolved
// through the leaked pattern-variable frame and read the `rest` capture.
//
// This is NOT the canonical shape yet. That one uses a `()` whole-clause
// pattern as its base case, which CompileSyntaxPattern still refuses (N2, filed
// in TODO.md); with N2 fixed, this test's shape is what it reduces to.
func TestRecursiveSyntaxListWalk(t *testing.T) {
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx)
	if err != nil {
		t.Fatal(err)
	}
	src := `(begin
              (define (walk stx)
                (let loop ((s stx) (acc '()) (n 0))
                  (if (null? (syntax->datum s))
                      (list n (reverse acc))
                      (let ((next (syntax-case s ()
                                    ((x . rest) (cons (syntax->datum #'x) #'rest)))))
                        (loop (cdr next) (cons (car next) acc) (+ n 1))))))
              (walk #'(1 2 3)))`
	got, err := engine.Eval(ctx, engine.MustParse(ctx, src))
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	want := "(3 (1 2 3))"
	if got.SchemeString() != want {
		t.Errorf("got %s, want %s", got.SchemeString(), want)
	}
}
