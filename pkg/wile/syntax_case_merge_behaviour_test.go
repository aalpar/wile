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

	qt "github.com/frankban/quicktest"
)

// TestSyntaxCaseClauseBodyLetBehaviour is the value arm of
// compilation.TestSyntaxCaseClauseBodyMergesLets.
//
// A `let` in a syntax-case clause body now takes its slots out of the
// pattern-variable frame BindPatternVars pushes, instead of pushing one of its
// own. The two sides of that frame are built in two different files and nothing
// type-checks that they agree, so the failure mode is an INDEX collision: a
// merged binding landing on a pattern variable, or on the reserved slot holding
// the form's own syntax-case state. Neither crashes. Both produce a macro that
// expands to the wrong thing.
//
// Every fixture below is chosen so that a collision changes the VALUE:
// pattern variables and let bindings are read on both sides of each other, the
// state slot is exercised by a `(syntax ...)` template after the lets, and the
// reentrancy case nests a whole syntax-case inside a merged let.
func TestSyntaxCaseClauseBodyLetBehaviour(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{
			// Two lets, then a template naming BOTH pattern variables. The
			// template resolves through the frame's state slot, so this fails if
			// a merged binding took it.
			name: "nested lets before a syntax template",
			code: `(begin
			         (define-syntax m
			           (lambda (stx)
			             (syntax-case stx ()
			               ((_ a b)
			                (let ((x (syntax a)))
			                  (let ((y (syntax b)))
			                    (syntax (list a b))))))))
			         (m 10 20))`,
			want: "(10 20)",
		},
		{
			// A let binder SPELLED like a pattern variable. The slots must stay
			// distinct — the same property "let shadowing a parameter" pins for
			// a lambda frame, here against a nominal namespace rather than a
			// hygienic one.
			name: "a let binder shadowing a pattern variable",
			code: `(begin
			         (define-syntax m
			           (lambda (stx)
			             (syntax-case stx ()
			               ((_ a) (let ((a (syntax (quote shadowed)))) a)))))
			         (m 99))`,
			want: "shadowed",
		},
		{
			// Three frames deep. Each merges through the ones above it, and the
			// template still reaches the pattern variables.
			name: "three nested lets",
			code: `(begin
			         (define-syntax m
			           (lambda (stx)
			             (syntax-case stx ()
			               ((_ a b c)
			                (let ((p 1)) (let ((q 2)) (let ((r 3))
			                  (syntax (list a b c)))))))))
			         (m 1 2 3))`,
			want: "(1 2 3)",
		},
		{
			// REENTRANCY. The inner syntax-case pushes its own pattern-variable
			// frame from inside a merged let, so the outer form's state slot must
			// be shadowed rather than clobbered — and the inner frame's own
			// merged let must not reach into it.
			name: "a syntax-case nested inside a merged let",
			code: `(begin
			         (define-syntax m
			           (lambda (stx)
			             (syntax-case stx ()
			               ((_ a)
			                (let ((q 5))
			                  (syntax-case (syntax (a)) ()
			                    ((b) (let ((r 7)) (syntax (list b))))))))))
			         (m 42))`,
			want: "(42)",
		},
		{
			// The syntax-rules spelling of the same shape: the lets are in the
			// TEMPLATE, so they are compiled in the expansion's own frame rather
			// than the clause body's. Included because it is the form that
			// actually occurs, and because it must be unaffected.
			name: "nested lets in a syntax-rules template",
			code: `(begin
			         (define-syntax m
			           (syntax-rules ()
			             ((_ a b) (let ((t a)) (let ((u b)) (+ t u))))))
			         (m 3 4))`,
			want: "7",
		},
		{
			// A closure created inside a merged let, reading a pattern variable.
			// This is the RetainsLexicalEnv path — the body contains a `syntax`
			// template, so it keeps the creating frame as its static link and
			// reads through the chain rather than through its free vector.
			name: "a lambda in a merged let reading a pattern variable",
			code: `(begin
			         (define-syntax m
			           (lambda (stx)
			             (syntax-case stx ()
			               ((_ a) (let ((f (lambda () (syntax a)))) (f))))))
			         (m 7))`,
			want: "7",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			ctx := context.Background()
			engine, err := wile.NewEngine(ctx)
			c.Assert(err, qt.IsNil)
			v, err := engine.EvalMultiple(ctx, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(v.SchemeString(), qt.Equals, tc.want)
		})
	}
}
