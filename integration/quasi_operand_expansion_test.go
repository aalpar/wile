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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

// TestI144UnquotedOperandIsAnExpression: the operand an unquote, a splice or an
// unsyntax fires is an ordinary expression, so a macro use in it expands. The
// expander passes these templates through whole, and the compiler used to take
// the operand raw, compiling `,(when #t 2) as a call to an unbound `when`.
//
// Expected column: Chez 10.4.1 (petite -q --script) and Racket 9.2, identical.
func TestI144UnquotedOperandIsAnExpression(t *testing.T) {
	const two = `(define-syntax two (syntax-rules () ((_) 2))) `
	tcs := []struct {
		name     string
		program  string
		expected string
	}{
		{name: "unquote", program: "`(1 ,(when #t 2))", expected: "(1 2)"},
		{name: "splice", program: "`(1 ,@(when #t (list 2 3)))", expected: "(1 2 3)"},
		{name: "dotted_tail", program: "`(1 . ,(when #t 2))", expected: "(1 . 2)"},
		{name: "vector", program: "`#(1 ,(when #t 2) ,@(when #t (list 3)))", expected: "#(1 2 3)"},
		{name: "local_in_operand", program: "(let ((x 5)) `(1 ,(when #t x)))", expected: "(1 5)"},
		{name: "unsyntax", program: "(syntax->datum #`(1 #,(when #t 2)))", expected: "(1 2)"},
		{name: "syntax_case_input", program: "(syntax-case (when #t '(a)) () ((x) 1))", expected: "1"},
		{name: "with_syntax_rhs", program: "(with-syntax ((a (when #t 1))) (syntax->datum (syntax a)))", expected: "1"},
		// The depth rule: only the depth-1 operand fires, so only it expands.
		// Expanding the inner one as well would render (unquote 2).
		{name: "depth2_macro_operand", program: two + "`(1 `(2 ,(two)) ,(two))", expected: "(1 (quasiquote (2 (unquote (two)))) 2)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
			c.Assert(err, qt.IsNil)
			result, err := engine.EvalMultiple(context.Background(), tc.program)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}

// TestI144Pins are green on master and stay green. Neither discriminates the
// fix; each guards a regression the fix could introduce.
func TestI144Pins(t *testing.T) {
	tcs := []struct {
		name     string
		program  string
		expected string
	}{
		// Blind to the depth rule: expanding the call (two) yields (two), so a
		// fix that wrongly expanded the inner operand would still pass this.
		{
			name:     "depth2_procedure_operand",
			program:  "(define (two) 2) `(1 `(2 ,(two)) ,(two))",
			expected: "(1 (quasiquote (2 (unquote (two)))) 2)",
		},
		// with-syntax synthesizes a (list expr …) head with no scopes. Were it
		// expanded, a user list macro would capture it. Racket also answers 1.
		{
			name:     "with_syntax_synthesized_list_not_captured",
			program:  "(define-syntax list (syntax-rules () ((_ x ...) 'captured))) (with-syntax ((a 1)) (syntax->datum (syntax a)))",
			expected: "1",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
			c.Assert(err, qt.IsNil)
			result, err := engine.EvalMultiple(context.Background(), tc.program)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}
