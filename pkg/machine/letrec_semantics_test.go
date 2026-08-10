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

package machine_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestLetrecSemantics(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "letrec basic bindings",
			code:     "(letrec ((x 1) (y 2)) (+ x y))",
			expected: values.NewInteger(3),
		},
		{
			name:     "letrec* sequential bindings",
			code:     "(letrec* ((x 1) (y (+ x 1))) y)",
			expected: values.NewInteger(2),
		},
		{
			name:     "letrec forward reference via lambdas",
			code:     "(letrec ((f (lambda () (g))) (g (lambda () 42))) (f))",
			expected: values.NewInteger(42),
		},
		{
			name:     "letrec mutual recursion",
			code:     "(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 10))",
			expected: values.TrueValue,
		},
		{
			name:     "letrec* body with multiple expressions",
			code:     "(letrec* ((x 10)) (+ x 1) (+ x 2))",
			expected: values.NewInteger(12),
		},
		{
			name:     "letrec single binding",
			code:     "(letrec ((x 99)) x)",
			expected: values.NewInteger(99),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := evalScheme(t, tc.code)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

// TestInternalDefineThroughBegin pins that a lambda body's forward reference
// reaches a define nested inside a begin — the shape define-values expands to.
//
// R7RS §5.3.2 makes a body's internal definitions a letrec* group, so g may
// name a and b before their defines run. The compiler's pre-declare pass for
// the CLOSURE path type-asserted *validate.ValidatedDefine only, missing the
// begin, so a and b were never predeclared and g failed to compile with
// `no such binding "a" with compatible scopes`. The let () twin already worked
// (compile_let.go's pass recurses), which is the control below.
func TestInternalDefineThroughBegin(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "lambda body: begin-wrapped defines are predeclared",
			code:     "(define (f) (define (g) (+ a b)) (begin (define a 1) (define b 2)) (g)) (f)",
			expected: values.NewInteger(3),
		},
		{
			name:     "lambda body: define-values expands to the same begin",
			code:     "(define (f) (define (g) (+ a b)) (define-values (a b) (values 1 2)) (g)) (f)",
			expected: values.NewInteger(3),
		},
		{
			// Control: the let path already recursed, so this passed before the
			// closure path was taught the same recursion.
			name:     "let body: the control that already worked",
			code:     "(let () (define (g) (+ a b)) (begin (define a 1) (define b 2)) (g))",
			expected: values.NewInteger(3),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := evalScheme(t, tc.code)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}
