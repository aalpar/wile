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
// This commit closes 15a and N1. The 15b and N3 rows arrive with the state
// relocation in the next commit.
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
