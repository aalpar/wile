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
// lambda
// ============================================================================

func TestLambda(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "identity lambda",
			Code:     `((lambda (x) x) 42)`,
			Expected: values.NewInteger(42),
		},
		{
			Name:     "lambda no params",
			Code:     `((lambda () 99))`,
			Expected: values.NewInteger(99),
		},
		{
			Name:     "lambda multiple params",
			Code:     `((lambda (a b) (+ a b)) 3 4)`,
			Expected: values.NewInteger(7),
		},
		{
			Name:     "lambda multiple body expressions",
			Code:     `((lambda (x) (+ x 1) (+ x 2)) 10)`,
			Expected: values.NewInteger(12),
		},
		{
			Name: "variadic lambda with dot notation",
			Code: `(begin
				(define f (lambda (x . rest) rest))
				(f 1 2 3))`,
			Expected: values.List(values.NewInteger(2), values.NewInteger(3)),
		},
		{
			Name: "lambda as bare rest parameter",
			Code: `(begin
				(define f (lambda args args))
				(f 1 2 3))`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			Name: "lambda closure captures variable",
			Code: `(begin
				(define (make-adder n)
					(lambda (x) (+ n x)))
				((make-adder 10) 5))`,
			Expected: values.NewInteger(15),
		},
		{
			Name: "nested lambda",
			Code: `(begin
				(define f
					(lambda (x)
						(lambda (y) (+ x y))))
				((f 3) 4))`,
			Expected: values.NewInteger(7),
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

func TestLambda_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "lambda missing body",
			Code: `(lambda (x))`,
		},
		{
			Name: "duplicate params",
			Code: `(lambda (x x) x)`,
		},
		{
			Name: "lambda no args at all",
			Code: `(lambda)`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ============================================================================
// case-lambda
// ============================================================================

func TestCaseLambda(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "single clause",
			Code: `(begin
				(define f (case-lambda
					((x) x)))
				(f 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "multiple clauses dispatch by arity",
			Code: `(begin
				(define f (case-lambda
					(() 0)
					((x) x)
					((x y) (+ x y))))
				(f 3 4))`,
			Expected: values.NewInteger(7),
		},
		{
			Name: "case-lambda zero arg clause",
			Code: `(begin
				(define f (case-lambda
					(() 99)
					((x) x)))
				(f))`,
			Expected: values.NewInteger(99),
		},
		{
			Name: "case-lambda with rest parameter",
			Code: `(begin
				(define f (case-lambda
					((x . rest) rest)))
				(f 1 2 3))`,
			Expected: values.List(values.NewInteger(2), values.NewInteger(3)),
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
// docstring extraction
// ============================================================================

func TestDocstringExtraction(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "lambda with docstring",
			Code:     `(begin (define f (lambda (x) "Adds one." (+ x 1))) (procedure-documentation f))`,
			Expected: values.NewString("Adds one."),
		},
		{
			Name:     "define-function with docstring",
			Code:     `(begin (define (g x) "Doubles x." (* x 2)) (procedure-documentation g))`,
			Expected: values.NewString("Doubles x."),
		},
		{
			Name:     "case-lambda clause with docstring",
			Code:     `(begin (define h (case-lambda ((x) "One arg." (+ x 1)) ((x y) (+ x y)))) (procedure-documentation h))`,
			Expected: values.NewString("One arg."),
		},
		{
			Name:     "no docstring returns false",
			Code:     `(begin (define (f x) (+ x 1)) (procedure-documentation f))`,
			Expected: values.FalseValue,
		},
		{
			Name:     "string-only body is return value not docstring",
			Code:     `(begin (define (f) "hello") (procedure-documentation f))`,
			Expected: values.FalseValue,
		},
		{
			Name:     "string-only body returns the string",
			Code:     `(begin (define (f) "hello") (f))`,
			Expected: values.NewString("hello"),
		},
		{
			Name:     "docstring stripped from body",
			Code:     `(begin (define (f x) "doc" (+ x 1)) (f 10))`,
			Expected: values.NewInteger(11),
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

func TestCaseLambda_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "no clauses",
			Code: `(case-lambda)`,
		},
		{
			Name: "clause missing body",
			Code: `(case-lambda ((x)))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}
