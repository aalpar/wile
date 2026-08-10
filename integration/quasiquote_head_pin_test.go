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

package integration_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// TestQuasiquote_HeadsSurviveTopLevelRebinding pins that the list-construction
// heads quasiquote synthesizes carry their definition-site meaning. Each case is
// a SINGLE compilation unit in which the rebinding precedes the quasiquote, so
// ordinary use-site resolution finds the user's binding — which is exactly what
// made every one of these return HIJACKED before the pin.
//
// R7RS §5.2 carries the argument: redefining a name imported from (scheme base)
// is unspecified, so the value below is Wile's choice, not the report's. (§5.2's
// closing "However, a REPL should permit these actions" is why the headline
// (define list …) repro is only partially a defect; the reference is still the
// expander's own, and the expander did not write `list`.)
func TestQuasiquote_HeadsSurviveTopLevelRebinding(t *testing.T) {
	tests := []struct {
		name string
		code string
		want string
	}{
		{
			name: "list",
			code: `(define x 2) (define list (lambda a 'HIJACKED)) ` + "`(1 ,x 3)",
			want: "(1 2 3)",
		},
		{
			name: "cons via dotted unquote",
			code: `(define x 2) (define cons (lambda a 'HIJACKED)) ` + "`(1 ,x . ,x)",
			want: "(1 2 . 2)",
		},
		{
			name: "append via unquote-splicing",
			code: `(define x '(2 3)) (define append (lambda a 'HIJACKED)) ` + "`(1 ,@x 4)",
			want: "(1 2 3 4)",
		},
		{
			name: "list->vector via vector quasiquote",
			code: `(define x 2) (define list->vector (lambda a 'HIJACKED)) ` + "`#(1 ,x 3)",
			want: "#(1 2 3)",
		},
		{
			name: "list via nested quasiquote",
			code: `(define x 2) (define list (lambda a 'HIJACKED)) ` + "``(1 ,,x 3)",
			want: "(quasiquote (1 (unquote 2) 3))",
		},
		{
			name: "list via quasisyntax",
			code: `(define list (lambda a 'HIJACKED)) (syntax->datum ` + "#`(1 #,(+ 1 1) 3))",
			want: "(1 2 3)",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.Console))
			c.Assert(err, qt.IsNil)
			result, err := engine.EvalMultiple(context.Background(), tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestQuasiquote_HeadPinIsBoundedToGlobals is the control half: the pin is a
// FALLBACK consulted only after scope-set local resolution, so local binders of
// the same names must keep behaving exactly as they did — which is "not at all",
// since the synthesized reference carries the quasiquote form's scopes and a
// local binder's scope set is not a subset of them.
//
// The synthesized `quote` head is the other control: it is a special form, is
// not hijackable, and is deliberately not routed through the pin.
func TestQuasiquote_HeadPinIsBoundedToGlobals(t *testing.T) {
	tests := []struct {
		name string
		code string
		want string
	}{
		{
			name: "let shadow",
			code: `(let ((list (lambda a 'HIJACKED))) ` + "`(1 2 3))",
			want: "(1 2 3)",
		},
		{
			name: "lambda parameter shadow",
			code: `((lambda (list) ` + "`(1 2 3)) 5)",
			want: "(1 2 3)",
		},
		{
			name: "internal define shadow",
			code: `((lambda () (define list (lambda a 'HIJACKED)) ` + "`(1 2 3)))",
			want: "(1 2 3)",
		},
		{
			name: "quote is not hijackable",
			code: `(define quote (lambda a 'HIJACKED)) ` + "`(1 2 3)",
			want: "(1 2 3)",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.Console))
			c.Assert(err, qt.IsNil)
			result, err := engine.EvalMultiple(context.Background(), tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}
