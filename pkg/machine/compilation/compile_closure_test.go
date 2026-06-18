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

package compilation_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestCompileClosure tests closure compilation: parameter binding, capture,
// nested closures, and mutation over closed-over variables.
//
// Source: compile_closure.go (compileClosure, compileClosureBody, compileBody,
// bindRestParameter).
func TestCompileClosure(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic closure capture — call inner closure to retrieve captured value
		{
			Name:     "closure captures outer variable",
			Code:     `(((lambda (x) (lambda () x)) 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name:     "closure captures and uses outer variable",
			Code:     `(let ((f (lambda (x) (lambda (y) (+ x y))))) ((f 10) 20))`,
			Expected: values.NewInteger(30),
		},

		// Nested closures
		{
			Name:     "triple nested closure",
			Code:     `(let ((f (lambda (a) (lambda (b) (lambda (c) (+ a (+ b c))))))) (((f 1) 2) 3))`,
			Expected: values.NewInteger(6),
		},

		// machine.Closure over mutable binding
		{
			Name: "closure over mutable variable with set!",
			Code: `(let ((x 0))
			         (let ((inc (lambda () (set! x (+ x 1)) x)))
			           (inc)
			           (inc)))`,
			Expected: values.NewInteger(2),
		},
		{
			Name: "two closures share same mutable binding",
			Code: `(let ((x 0))
			         (let ((inc (lambda () (set! x (+ x 1)) x))
			               (get (lambda () x)))
			           (inc)
			           (inc)
			           (get)))`,
			Expected: values.NewInteger(2),
		},

		// Rest parameter binding
		{
			Name:     "variadic lambda collects rest args",
			Code:     `((lambda (a . rest) rest) 1 2 3)`,
			Expected: values.List(values.NewInteger(2), values.NewInteger(3)),
		},
		{
			Name:     "variadic lambda with no rest args",
			Code:     `((lambda (a . rest) rest) 1)`,
			Expected: values.EmptyList,
		},
		{
			Name:     "variadic lambda all-rest",
			Code:     `((lambda args args) 1 2 3)`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},

		// Body with internal defines (letrec* semantics)
		{
			Name: "lambda body with internal define",
			Code: `((lambda ()
			          (define x 10)
			          (define y 20)
			          (+ x y)))`,
			Expected: values.NewInteger(30),
		},
		{
			Name: "lambda body forward reference via letrec*",
			Code: `((lambda ()
			          (define (even? n) (if (= n 0) #t (odd? (- n 1))))
			          (define (odd? n) (if (= n 0) #f (even? (- n 1))))
			          (even? 4)))`,
			Expected: values.TrueValue,
		},

		// Zero-arg closure
		{
			Name:     "zero-arg lambda",
			Code:     `((lambda () 42))`,
			Expected: values.NewInteger(42),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
