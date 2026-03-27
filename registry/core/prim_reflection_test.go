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

package core_test

import (
	"testing"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// =============================================================================
// procedure-arity Tests
// =============================================================================

func TestProcedureArity(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "fixed foreign 1-arg",
			Code:     `(procedure-arity car)`,
			Expected: values.NewInteger(1),
		},
		{
			Name:     "fixed foreign 2-arg",
			Code:     `(procedure-arity cons)`,
			Expected: values.NewInteger(2),
		},
		{
			Name:     "fixed lambda",
			Code:     `(procedure-arity (lambda (x y) x))`,
			Expected: values.NewInteger(2),
		},
		{
			Name:     "variadic foreign",
			Code:     `(procedure-arity +)`,
			Expected: values.NewCons(values.NewInteger(0), values.FalseValue),
		},
		{
			Name:     "variadic lambda",
			Code:     `(procedure-arity (lambda (x . rest) x))`,
			Expected: values.NewCons(values.NewInteger(1), values.FalseValue),
		},
		{
			Name: "case-lambda",
			Code: `(procedure-arity (case-lambda ((x) x) ((x y) x)))`,
			Expected: values.List(
				values.NewInteger(1),
				values.NewInteger(2),
			),
		},
		{
			Name:     "parameter",
			Code:     `(procedure-arity (make-parameter 0))`,
			Expected: values.NewCons(values.NewInteger(0), values.FalseValue),
		},
		{
			Name: "composable continuation",
			Code: `(let ((tag (default-continuation-prompt-tag)))
				(call-with-continuation-prompt
					(lambda ()
						(call-with-composable-continuation
							(lambda (k) (procedure-arity k))
							tag))
					tag
					(lambda (v) v)))`,
			Expected: values.NewInteger(1),
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

func TestProcedureArityErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "integer", Code: `(procedure-arity 42)`},
		{Name: "string", Code: `(procedure-arity "hello")`},
		{Name: "boolean", Code: `(procedure-arity #t)`},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// =============================================================================
// procedure-name Tests
// =============================================================================

func TestProcedureName(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "foreign procedure",
			Code:     `(procedure-name car)`,
			Expected: values.NewString("car"),
		},
		{
			Name:     "anonymous lambda",
			Code:     `(procedure-name (lambda (x) x))`,
			Expected: values.FalseValue,
		},
		{
			Name:     "defined function",
			Code:     `(begin (define (foo x) x) (procedure-name foo))`,
			Expected: values.NewString("foo"),
		},
		{
			Name:     "case-lambda",
			Code:     `(procedure-name (case-lambda ((x) x) ((x y) x)))`,
			Expected: values.FalseValue,
		},
		{
			Name:     "parameter",
			Code:     `(procedure-name (make-parameter 0))`,
			Expected: values.FalseValue,
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

func TestProcedureNameErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "integer", Code: `(procedure-name 42)`},
		{Name: "string", Code: `(procedure-name "hello")`},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// =============================================================================
// procedure-source-location Tests
// =============================================================================

func TestProcedureSourceLocation(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "foreign has no source",
			Code:     `(procedure-source-location car)`,
			Expected: values.FalseValue,
		},
		{
			Name:     "eval lambda has no source file",
			Code:     `(procedure-source-location (lambda (x) x))`,
			Expected: values.FalseValue,
		},
		{
			Name:     "eval case-lambda has no source file",
			Code:     `(procedure-source-location (case-lambda ((x) x) ((x y) x)))`,
			Expected: values.FalseValue,
		},
		{
			Name:     "parameter has no source",
			Code:     `(procedure-source-location (make-parameter 0))`,
			Expected: values.FalseValue,
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

func TestProcedureSourceLocationErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "integer", Code: `(procedure-source-location 42)`},
		{Name: "string", Code: `(procedure-source-location "hello")`},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// =============================================================================
// procedure-bound-symbols Tests
// =============================================================================

func TestProcedureBoundSymbols(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "foreign returns false",
			Code:     `(procedure-bound-symbols car)`,
			Expected: values.FalseValue,
		},
		{
			Name:     "parameter returns false",
			Code:     `(procedure-bound-symbols (make-parameter 0))`,
			Expected: values.FalseValue,
		},
		{
			Name:     "lambda with no params",
			Code:     `(procedure-bound-symbols (lambda () 42))`,
			Expected: values.EmptyList,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}

	// Lambda bound symbols return a list from the local environment.
	// Map iteration order is non-deterministic, so we check membership
	// and length rather than exact list order. Symbols carry scope sets
	// so we compare via symbol->string rather than eq?.
	memberTcs := []struct {
		name         string
		code         string
		expectedLen  int
		memberChecks []string
	}{
		{
			name:         "lambda with params",
			code:         `(procedure-bound-symbols (lambda (x y) x))`,
			expectedLen:  2,
			memberChecks: []string{"x", "y"},
		},
		{
			name:         "closure captures only own params",
			code:         `(let ((a 1)) (procedure-bound-symbols (lambda (x) (+ x a))))`,
			expectedLen:  1,
			memberChecks: []string{"x"},
		},
	}

	for _, tc := range memberTcs {
		t.Run(tc.name, func(t *testing.T) {
			lenCode := "(length " + tc.code + ")"
			result, err := testhelpers.RunSchemeCode(t, lenCode)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals,
				values.NewInteger(int64(tc.expectedLen)))

			for _, sym := range tc.memberChecks {
				// Use member with a custom comparator via symbol->string
				// since bound symbols carry scope sets that differ from
				// reader-interned symbols.
				memCode := `(member "` + sym + `" (map symbol->string ` + tc.code + `))`
				memResult, memErr := testhelpers.RunSchemeCode(t, memCode)
				qt.Assert(t, memErr, qt.IsNil)
				qt.Assert(t, memResult != values.FalseValue, qt.IsTrue,
					qt.Commentf("expected %s in bound symbols", sym))
			}
		})
	}
}

func TestProcedureBoundSymbolsErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "integer", Code: `(procedure-bound-symbols 42)`},
		{Name: "string", Code: `(procedure-bound-symbols "hello")`},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// =============================================================================
// procedure-type Tests
// =============================================================================

func TestProcedureType(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "lambda",
			Code:     `(procedure-type (lambda (x) x))`,
			Expected: values.NewSymbol("lambda"),
		},
		{
			Name:     "foreign",
			Code:     `(procedure-type car)`,
			Expected: values.NewSymbol("foreign"),
		},
		{
			Name:     "case-lambda",
			Code:     `(procedure-type (case-lambda ((x) x) ((x y) x)))`,
			Expected: values.NewSymbol("case-lambda"),
		},
		{
			Name:     "parameter",
			Code:     `(procedure-type (make-parameter 0))`,
			Expected: values.NewSymbol("parameter"),
		},
		{
			Name: "composable continuation",
			Code: `(let ((tag (default-continuation-prompt-tag)))
				(call-with-continuation-prompt
					(lambda ()
						(call-with-composable-continuation
							(lambda (k) (procedure-type k))
							tag))
					tag
					(lambda (v) v)))`,
			Expected: values.NewSymbol("continuation"),
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

func TestProcedureTypeErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "integer", Code: `(procedure-type 42)`},
		{Name: "string", Code: `(procedure-type "hello")`},
		{Name: "boolean", Code: `(procedure-type #t)`},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// =============================================================================
// procedure-documentation Tests
// =============================================================================

func TestProcedureDocumentation(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "define with docstring",
			Code:     `(begin (define (f x) "Adds one to x." (+ x 1)) (procedure-documentation f))`,
			Expected: values.NewString("Adds one to x."),
		},
		{
			Name:     "lambda with docstring",
			Code:     `(begin (define f (lambda (x) "Doubles x." (* x 2))) (procedure-documentation f))`,
			Expected: values.NewString("Doubles x."),
		},
		{
			Name:     "no docstring returns false",
			Code:     `(begin (define (g x) (+ x 1)) (procedure-documentation g))`,
			Expected: values.FalseValue,
		},
		{
			Name:     "string-only body is return value not docstring",
			Code:     `(begin (define (h) "just a string") (procedure-documentation h))`,
			Expected: values.FalseValue,
		},
		{
			Name:     "foreign procedure returns doc",
			Code:     `(procedure-documentation car)`,
			Expected: values.NewString("Returns the first element (car) of a pair. Raises an error if the argument is not a pair."),
		},
		{
			Name: "case-lambda with docstring in first clause",
			Code: `(begin (define f (case-lambda
			           ((x) "One arg." (+ x 1))
			           ((x y) (+ x y))))
			       (procedure-documentation f))`,
			Expected: values.NewString("One arg."),
		},
		{
			Name: "case-lambda without docstring",
			Code: `(begin (define f (case-lambda
			           ((x) (+ x 1))
			           ((x y) (+ x y))))
			       (procedure-documentation f))`,
			Expected: values.FalseValue,
		},
		{
			Name:     "multiline docstring",
			Code:     "(begin (define (f x)\n  \"Adds one to x.\\nReturns an integer.\"\n  (+ x 1))\n(procedure-documentation f))",
			Expected: values.NewString("Adds one to x.\nReturns an integer."),
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

func TestProcedureDocumentationErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "wrong arity zero", Code: `(procedure-documentation)`},
		{Name: "wrong arity two", Code: `(procedure-documentation car car)`},
		{Name: "integer", Code: `(procedure-documentation 42)`},
		{Name: "string", Code: `(procedure-documentation "hello")`},
		{Name: "boolean", Code: `(procedure-documentation #t)`},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
