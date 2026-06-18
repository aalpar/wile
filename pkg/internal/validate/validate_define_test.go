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

package validate_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// ============================================================================
// define — variable definition
// ============================================================================

func TestDefineVariable(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "define integer",
			Code:     `(begin (define x 1) x)`,
			Expected: values.NewInteger(1),
		},
		{
			Name:     "define string",
			Code:     `(begin (define s "hello") s)`,
			Expected: values.NewString("hello"),
		},
		{
			Name:     "define with expression",
			Code:     `(begin (define x (+ 2 3)) x)`,
			Expected: values.NewInteger(5),
		},
		{
			Name:     "define boolean",
			Code:     `(begin (define b #t) b)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "define with nested define",
			Code:     `(begin (define a 10) (define b (+ a 5)) b)`,
			Expected: values.NewInteger(15),
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

// ============================================================================
// define — function definition
// ============================================================================

func TestDefineFunction(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "simple function",
			Code:     `(begin (define (f x) x) (f 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name:     "function with multiple params",
			Code:     `(begin (define (add a b) (+ a b)) (add 3 4))`,
			Expected: values.NewInteger(7),
		},
		{
			Name:     "function with multiple body expressions",
			Code:     `(begin (define (g x) (+ x 1) (+ x 2)) (g 10))`,
			Expected: values.NewInteger(12),
		},
		{
			Name:     "function no params",
			Code:     `(begin (define (f) 99) (f))`,
			Expected: values.NewInteger(99),
		},
		{
			Name: "function with rest parameter",
			Code: `(begin
				(define (f x . rest) rest)
				(f 1 2 3))`,
			Expected: values.List(values.NewInteger(2), values.NewInteger(3)),
		},
		{
			Name: "recursive function",
			Code: `(begin
				(define (fact n)
					(if (= n 0) 1 (* n (fact (- n 1)))))
				(fact 5))`,
			Expected: values.NewInteger(120),
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

// ============================================================================
// define — error cases
// ============================================================================

func TestDefine_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "missing value in variable define",
			Code: `(define x)`,
		},
		{
			Name: "duplicate parameter names",
			Code: `(define (f x x) x)`,
		},
		{
			// Pins the multi-dup error-accumulation behavior. After
			// the validate_define refactor to post-parse detection +
			// full-slice iteration, all three duplicate pairs should
			// be reported in a single validation pass rather than
			// stopping at the first.
			Name: "multiple duplicate parameter names",
			Code: `(define (f x x y y z z) x)`,
		},
		{
			// Rest-vs-required collision exercises the post-parse
			// detection path that runs after both Required and Rest
			// have been collected.
			Name: "rest parameter shadows required parameter",
			Code: `(define (f x . x) x)`,
		},
		{
			Name: "function missing body",
			Code: `(define (f x))`,
		},
		{
			Name: "non-symbol non-list after define",
			Code: `(define 42 1)`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}
