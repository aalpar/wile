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

func TestEval(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "eval simple arithmetic",
			code: "(eval '(+ 1 2) (interaction-environment))",
			out:  values.NewInteger(3),
		},
		{
			name: "eval multiplication",
			code: "(eval '(* 3 4) (interaction-environment))",
			out:  values.NewInteger(12),
		},
		{
			name: "eval quoted symbol",
			code: "(eval ''hello (interaction-environment))",
			out:  values.NewSymbol("hello"),
		},
		{
			name: "eval list constructor",
			code: "(eval '(list 1 2 3) (interaction-environment))",
			out:  values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestInteractionEnvironment(t *testing.T) {
	t.Run("returns environment", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(interaction-environment)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, qt.IsNotNil)
	})
}

func TestEvalExtended(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "eval subtraction",
			Code:     `(eval '(- 10 3) (interaction-environment))`,
			Expected: values.NewInteger(7),
		},
		{
			Name:     "eval nested expression",
			Code:     `(eval '(+ (* 2 3) 4) (interaction-environment))`,
			Expected: values.NewInteger(10),
		},
		{
			Name:     "eval if expression",
			Code:     `(eval '(if (> 5 3) 1 2) (interaction-environment))`,
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

// =============================================================================
// eval Additional Tests (R7RS §6.12)
// =============================================================================

// TestEvalWithLambda tests eval with lambda expressions
func TestEvalWithLambda(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "eval lambda application",
			Code:     `(eval '((lambda (x) (+ x 1)) 5) (interaction-environment))`,
			Expected: values.NewInteger(6),
		},
		{
			Name:     "eval lambda with multiple args",
			Code:     `(eval '((lambda (x y) (* x y)) 3 4) (interaction-environment))`,
			Expected: values.NewInteger(12),
		},
		{
			Name:     "eval let expression",
			Code:     `(eval '(let ((x 10)) (+ x 5)) (interaction-environment))`,
			Expected: values.NewInteger(15),
		},
		{
			Name:     "eval cond expression",
			Code:     `(eval '(cond ((= 1 2) 'no) ((= 2 2) 'yes) (else 'maybe)) (interaction-environment))`,
			Expected: values.NewSymbol("yes"),
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

// TestEvalErrors tests error conditions for eval
func TestEvalErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "eval with non-environment", Code: `(eval '(+ 1 2) 42)`},
		{Name: "eval with string env", Code: `(eval '(+ 1 2) "env")`},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// =============================================================================
// scheme-report-environment Extended Tests (R7RS §6.12)
// =============================================================================

// TestSchemeReportEnvironmentErrors tests error conditions
func TestSchemeReportEnvironmentErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "unsupported version", Code: `(scheme-report-environment 6)`},
		{Name: "non-integer version", Code: `(scheme-report-environment "5")`},
		{Name: "version 0", Code: `(scheme-report-environment 0)`},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// =============================================================================
// null-environment Extended Tests (R7RS §6.12)
// =============================================================================

// TestNullEnvironmentErrors tests error conditions
func TestNullEnvironmentErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "unsupported version", Code: `(null-environment 6)`},
		{Name: "non-integer version", Code: `(null-environment "5")`},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// =============================================================================
// compile Tests
// =============================================================================

// TestCompile tests the compile primitive
func TestCompile(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "compile and execute simple expression",
			code: `(let ((thunk (compile '(+ 1 2)))) (thunk))`,
			out:  values.NewInteger(3),
		},
		{
			name: "compile and execute multiplication",
			code: `(let ((thunk (compile '(* 3 4)))) (thunk))`,
			out:  values.NewInteger(12),
		},
		{
			name: "compile and execute quoted value",
			code: `(let ((thunk (compile ''hello))) (thunk))`,
			out:  values.NewSymbol("hello"),
		},
		{
			name: "compile and execute list",
			code: `(let ((thunk (compile '(list 1 2 3)))) (thunk))`,
			out:  values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name: "compile returns procedure",
			code: `(procedure? (compile '(+ 1 2)))`,
			out:  values.TrueValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

// =============================================================================
// syntax->datum and datum->syntax Tests (R7RS syntax)
// =============================================================================

// TestSyntaxToDatum tests syntax->datum
func TestSyntaxToDatum(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "syntax->datum on symbol",
			code: `(syntax->datum (datum->syntax #f 'hello))`,
			out:  values.NewSymbol("hello"),
		},
		{
			name: "syntax->datum on number",
			code: `(syntax->datum (datum->syntax #f 42))`,
			out:  values.NewInteger(42),
		},
		{
			name: "syntax->datum on list",
			code: `(syntax->datum (datum->syntax #f '(a b c)))`,
			out:  values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
		},
		{
			name: "syntax->datum on nested list",
			code: `(syntax->datum (datum->syntax #f '(+ 1 (* 2 3))))`,
			out: values.List(values.NewSymbol("+"),
				values.NewInteger(1),
				values.List(values.NewSymbol("*"), values.NewInteger(2), values.NewInteger(3))),
		},
		{
			name: "syntax->datum on empty list",
			code: `(syntax->datum (datum->syntax #f '()))`,
			out:  values.EmptyList,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

// TestDatumToSyntax tests datum->syntax
func TestDatumToSyntax(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "datum->syntax with #f template",
			code: `(identifier? (datum->syntax #f 'foo))`,
			out:  values.TrueValue,
		},
		{
			name: "datum->syntax preserves symbol name",
			code: `(syntax->datum (datum->syntax #f 'bar))`,
			out:  values.NewSymbol("bar"),
		},
		{
			name: "datum->syntax with number",
			code: `(syntax->datum (datum->syntax #f 123))`,
			out:  values.NewInteger(123),
		},
		{
			name: "datum->syntax with string",
			code: `(syntax->datum (datum->syntax #f "hello"))`,
			out:  values.NewString("hello"),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

// TestDatumToSyntaxErrors tests error conditions
func TestDatumToSyntaxErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "invalid template - number", Code: `(datum->syntax 42 'foo)`},
		{Name: "invalid template - string", Code: `(datum->syntax "bad" 'foo)`},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// =============================================================================
// identifier? Tests
// =============================================================================

// TestIdentifierQ tests identifier? predicate
func TestIdentifierQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "identifier? on syntax symbol is true",
			code: `(identifier? (datum->syntax #f 'foo))`,
			out:  values.TrueValue,
		},
		{
			name: "identifier? on number is false",
			code: `(identifier? 42)`,
			out:  values.FalseValue,
		},
		{
			name: "identifier? on string is false",
			code: `(identifier? "hello")`,
			out:  values.FalseValue,
		},
		{
			name: "identifier? on list is false",
			code: `(identifier? '(a b c))`,
			out:  values.FalseValue,
		},
		{
			name: "identifier? on boolean is false",
			code: `(identifier? #t)`,
			out:  values.FalseValue,
		},
		{
			name: "identifier? on syntax list is false",
			code: `(identifier? (datum->syntax #f '(a b)))`,
			out:  values.FalseValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

// =============================================================================
// expand Tests
// =============================================================================

// TestExpand tests the expand primitive
func TestExpand(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "expand simple expression returns syntax",
			code: `(syntax->datum (expand (datum->syntax #f '(+ 1 2))))`,
			out:  values.List(values.NewSymbol("+"), values.NewInteger(1), values.NewInteger(2)),
		},
		{
			name: "expand and macro expands",
			code: `(let ((result (syntax->datum (expand (datum->syntax #f '(and #t #f))))))
				(list? result))`,
			out: values.TrueValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

// TestExpandErrors tests error conditions for expand
func TestExpandErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "expand non-syntax", Code: `(expand 42)`},
		{Name: "expand string", Code: `(expand "hello")`},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// =============================================================================
// expand-once Tests
// =============================================================================

// TestExpandOnce tests the expand-once primitive
func TestExpandOnce(t *testing.T) {
	t.Run("expand-once on non-macro returns two values", func(t *testing.T) {
		// expand-once returns multiple values, test that second value is boolean
		code := `(call-with-values
			(lambda () (expand-once (datum->syntax #f '(+ 1 2))))
			(lambda (stx expanded?) expanded?))`
		result, err := testhelpers.RunSchemeCode(t, code)
		qt.Assert(t, err, qt.IsNil)
		// For a primitive form like +, no expansion happens
		qt.Assert(t, result, valuestest.SchemeEquals, values.FalseValue)
	})

	t.Run("expand-once returns syntax object", func(t *testing.T) {
		code := `(call-with-values
			(lambda () (expand-once (datum->syntax #f '(+ 1 2))))
			(lambda (stx expanded?)
				(syntax->datum stx)))`
		result, err := testhelpers.RunSchemeCode(t, code)
		qt.Assert(t, err, qt.IsNil)
		expected := values.List(values.NewSymbol("+"), values.NewInteger(1), values.NewInteger(2))
		qt.Assert(t, result, valuestest.SchemeEquals, expected)
	})
}

// TestExpandOnceErrors tests error conditions for expand-once
func TestExpandOnceErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "expand-once non-syntax", Code: `(expand-once 42)`},
		{Name: "expand-once string", Code: `(expand-once "hello")`},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// =============================================================================
// make-compile-time-value Tests
// =============================================================================

// TestMakeCompileTimeValue tests the make-compile-time-value primitive
func TestMakeCompileTimeValue(t *testing.T) {
	t.Run("make-compile-time-value wraps value", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(make-compile-time-value 42)`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, qt.IsNotNil)
	})

	t.Run("make-compile-time-value wraps string", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(make-compile-time-value "hello")`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, qt.IsNotNil)
	})

	t.Run("make-compile-time-value wraps list", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(make-compile-time-value '(a b c))`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, qt.IsNotNil)
	})
}

// =============================================================================
// environment Symbol Identity Tests (R7RS §6.5, §6.12)
// =============================================================================

// TestEnvironmentSymbolIdentity tests that environments created by (environment)
// share symbol interning with the caller, ensuring R7RS §6.5 symbol identity.
func TestEnvironmentSymbolIdentity(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "empty environment eval quoted symbol",
			Code:     `(let ((e (environment))) (eval ''hello e))`,
			Expected: values.NewSymbol("hello"),
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

// TestNullEnvironmentSymbolIdentity tests that null-environment shares
// symbol interning with the caller for R7RS §6.5 symbol identity.
func TestNullEnvironmentSymbolIdentity(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "null-environment 5 returns environment",
			Code:     `(let ((e (null-environment 5))) (eval ''hello e))`,
			Expected: values.NewSymbol("hello"),
		},
		{
			Name:     "null-environment 7 returns environment",
			Code:     `(let ((e (null-environment 7))) (eval ''hello e))`,
			Expected: values.NewSymbol("hello"),
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
