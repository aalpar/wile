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

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

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
			Name:     "closure",
			Code:     `(procedure-type (lambda (x) x))`,
			Expected: values.NewSymbol("closure"),
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
			Expected: values.NewString("Returns the first element (car) of PAIR. Raises an error if the argument is not a pair.\n\nExamples:\n  (car '(1 2 3))    => 1\n  (car '(a . b))    => a"),
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

// =============================================================================
// library-description Tests
// =============================================================================

func TestLibraryDescription(t *testing.T) {
	// Note: The test environment (NewNamespaceFrame) does not configure a
	// library registry, so (import ...) is unavailable. Tests for loaded
	// libraries with/without descriptions belong in integration/ or
	// engine-level tests once .sld files gain (description ...) clauses.
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "nonexistent library returns false",
			Code:     `(library-description '(nonexistent lib))`,
			Expected: values.FalseValue,
		},
		{
			Name:     "single-part library name returns false",
			Code:     `(library-description '(nonexistent))`,
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

func TestLibraryDescriptionErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "empty library name", Code: `(library-description '())`},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// =============================================================================
// apropos Tests
// =============================================================================

func TestApropos(t *testing.T) {
	tcs := []struct {
		name  string
		code  string
		check func(t *testing.T, result values.Value)
	}{
		{
			name: "find by name",
			code: `(memq 'string-append (apropos "string-app"))`,
			check: func(t *testing.T, result values.Value) {
				// memq returns a pair when found
				_, ok := result.(*values.Pair)
				qt.Assert(t, ok, qt.IsTrue, qt.Commentf("got %T: %s", result, result.SchemeString()))
			},
		},
		{
			name: "returns list of symbols",
			code: `(let ((results (apropos "car")))
				(and (list? results) (symbol? (car results))))`,
			check: func(t *testing.T, result values.Value) {
				qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
			},
		},
		{
			name: "no matches returns empty list",
			code: `(apropos "zzzzzzzzzzz")`,
			check: func(t *testing.T, result values.Value) {
				qt.Assert(t, result, valuestest.SchemeEquals, values.EmptyList)
			},
		},
		{
			name: "matches category",
			code: `(memq '+ (apropos "arithmetic"))`,
			check: func(t *testing.T, result values.Value) {
				_, ok := result.(*values.Pair)
				qt.Assert(t, ok, qt.IsTrue, qt.Commentf("got %T: %s", result, result.SchemeString()))
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			tc.check(t, result)
		})
	}
}

func TestAproposErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "wrong arity zero", Code: `(apropos)`},
		{Name: "wrong arity two", Code: `(apropos "a" "b")`},
		{Name: "wrong type", Code: `(apropos 42)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// =============================================================================
// doc-topics Tests
// =============================================================================

func TestDocTopics(t *testing.T) {
	tcs := []struct {
		name  string
		code  string
		check func(t *testing.T, result values.Value)
	}{
		{
			name: "returns list of strings",
			code: `(let ((ts (doc-topics)))
				(and (list? ts) (string? (car ts))))`,
			check: func(t *testing.T, result values.Value) {
				qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
			},
		},
		{
			name: "contains arithmetic",
			code: `(member "arithmetic" (doc-topics))`,
			check: func(t *testing.T, result values.Value) {
				// member returns pair when found
				_, ok := result.(*values.Pair)
				qt.Assert(t, ok, qt.IsTrue, qt.Commentf("got %T: %s", result, result.SchemeString()))
			},
		},
		{
			name: "sorted",
			code: `(let ((ts (doc-topics)))
				(let check ((prev (car ts)) (rest (cdr ts)))
				  (cond
				    ((null? rest) #t)
				    ((string<=? prev (car rest))
				     (check (car rest) (cdr rest)))
				    (else #f))))`,
			check: func(t *testing.T, result values.Value) {
				qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			tc.check(t, result)
		})
	}
}

func TestDocTopicsErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "wrong arity", Code: `(doc-topics "extra")`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// =============================================================================
// doc-topic Tests
// =============================================================================

func TestDocTopic(t *testing.T) {
	tcs := []struct {
		name  string
		code  string
		check func(t *testing.T, result values.Value)
	}{
		{
			name: "returns list of symbols",
			code: `(let ((procs (doc-topic "arithmetic")))
				(and (list? procs) (symbol? (car procs))))`,
			check: func(t *testing.T, result values.Value) {
				qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
			},
		},
		{
			name: "contains +",
			code: `(memq '+ (doc-topic "arithmetic"))`,
			check: func(t *testing.T, result values.Value) {
				_, ok := result.(*values.Pair)
				qt.Assert(t, ok, qt.IsTrue, qt.Commentf("got %T: %s", result, result.SchemeString()))
			},
		},
		{
			name: "unknown category returns empty list",
			code: `(doc-topic "nonexistent")`,
			check: func(t *testing.T, result values.Value) {
				qt.Assert(t, result, valuestest.SchemeEquals, values.EmptyList)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			tc.check(t, result)
		})
	}
}

func TestDocTopicErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "wrong arity zero", Code: `(doc-topic)`},
		{Name: "wrong arity two", Code: `(doc-topic "a" "b")`},
		{Name: "wrong type", Code: `(doc-topic 42)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestRegisteredPrimitives(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "returns a non-empty list",
			Code:     `(> (length (registered-primitives)) 100)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "car of list satisfies primitive-spec?",
			Code:     `(primitive-spec? (car (registered-primitives)))`,
			Expected: values.TrueValue,
		},
		{
			Name:     "primitive-spec? rejects non-specs",
			Code:     `(primitive-spec? 42)`,
			Expected: values.FalseValue,
		},
		{
			Name:     "cons's spec has name cons",
			Code:     "(let loop ((ps (registered-primitives)))\n  (cond ((null? ps) #f)\n        ((string=? (primitive-spec-name (car ps)) \"cons\") #t)\n        (else (loop (cdr ps)))))",
			Expected: values.TrueValue,
		},
		{
			Name:     "cons param-count is 2",
			Code:     "(let loop ((ps (registered-primitives)))\n  (cond ((null? ps) -1)\n        ((string=? (primitive-spec-name (car ps)) \"cons\")\n         (primitive-spec-param-count (car ps)))\n        (else (loop (cdr ps)))))",
			Expected: values.NewInteger(2),
		},
		{
			Name:     "cons go-function is non-empty",
			Code:     "(let loop ((ps (registered-primitives)))\n  (cond ((null? ps) #f)\n        ((string=? (primitive-spec-name (car ps)) \"cons\")\n         (> (string-length (primitive-spec-go-function (car ps))) 0))\n        (else (loop (cdr ps)))))",
			Expected: values.TrueValue,
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

func TestRegisteredPrimitivesErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "accessor on non-spec", Code: `(primitive-spec-name 42)`},
		{Name: "accessor on wrong-tag opaque", Code: `(primitive-spec-name "hello")`},
		{Name: "return-type on non-spec", Code: `(primitive-spec-return-type '())`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
