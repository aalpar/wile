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
// make-parameter Tests (R7RS §4.2.6)
//
// R7RS §4.2.6: "Returns a newly allocated parameter object, which is a
// procedure that accepts zero arguments and returns the value associated
// with the parameter object. Initially, this value is the value of
// (converter init), or of init if the converter is not specified."
// =============================================================================

func TestMakeParameter(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "make-parameter creates parameter with initial value",
			code: `(let ((p (make-parameter 42)))
				(p))`,
			out: values.NewInteger(42),
		},
		{
			name: "make-parameter with string initial value",
			code: `(let ((p (make-parameter "hello")))
				(p))`,
			out: values.NewString("hello"),
		},
		{
			name: "make-parameter with symbol initial value",
			code: `(let ((p (make-parameter 'foo)))
				(p))`,
			out: values.NewSymbol("foo"),
		},
		{
			name: "make-parameter with list initial value",
			code: `(let ((p (make-parameter '(1 2 3))))
				(p))`,
			out: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name: "make-parameter with boolean initial value",
			code: `(let ((p (make-parameter #t)))
				(p))`,
			out: values.TrueValue,
		},
		{
			name: "make-parameter with empty list",
			code: `(let ((p (make-parameter '())))
				(p))`,
			out: values.EmptyList,
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

func TestMakeParameterWithConverter(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "converter applied to initial value",
			code: `(let ((p (make-parameter 5 (lambda (x) (* x 2)))))
				(p))`,
			out: values.NewInteger(10),
		},
		{
			name: "converter transforms string",
			code: `(let ((p (make-parameter "hello" string-upcase)))
				(p))`,
			out: values.NewString("HELLO"),
		},
		{
			name: "converter with arithmetic",
			code: `(let ((p (make-parameter 3 (lambda (x) (+ x 1)))))
				(p))`,
			out: values.NewInteger(4),
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

func TestMakeParameterErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "converter not a procedure",
			Code: `(make-parameter 42 "not-a-proc")`,
		},
		{
			Name: "converter is integer",
			Code: `(make-parameter 42 123)`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// =============================================================================
// parameterize Tests (R7RS §4.2.6)
//
// R7RS §4.2.6: "A parameterize expression is used to change the values returned
// by specified parameter objects during the evaluation of the body."
// =============================================================================

func TestParameterize(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "parameterize changes parameter value in body",
			code: `(let ((p (make-parameter 1)))
				(parameterize ((p 2))
					(p)))`,
			out: values.NewInteger(2),
		},
		{
			name: "parameterize restores value after body",
			code: `(let ((p (make-parameter 10)))
				(parameterize ((p 20))
					(p))
				(p))`,
			out: values.NewInteger(10),
		},
		{
			name: "nested parameterize",
			code: `(let ((p (make-parameter 1)))
				(parameterize ((p 2))
					(parameterize ((p 3))
						(p))))`,
			out: values.NewInteger(3),
		},
		{
			name: "parameterize with multiple parameters",
			code: `(let ((p1 (make-parameter 'a))
			       (p2 (make-parameter 'b)))
				(parameterize ((p1 'x) (p2 'y))
					(list (p1) (p2))))`,
			out: values.List(values.NewSymbol("x"), values.NewSymbol("y")),
		},
		{
			name: "parameterize returns body result",
			code: `(let ((p (make-parameter 0)))
				(parameterize ((p 5))
					(+ (p) 10)))`,
			out: values.NewInteger(15),
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

func TestParameterizeRestoresOnException(t *testing.T) {
	// Test that parameterize restores parameter value even when exception occurs
	result, err := testhelpers.RunSchemeCode(t, `
		(let ((p (make-parameter 'original)))
			(with-exception-handler
				(lambda (e) (p))
				(lambda ()
					(parameterize ((p 'modified))
						(raise-continuable 'test)))))
	`)
	qt.Assert(t, err, qt.IsNil)
	// After the exception handler runs, we should see 'original
	// because parameterize should restore the value
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewSymbol("original"))
}

func TestParameterizeWithConverter(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "parameterize applies converter to new value",
			code: `(let ((p (make-parameter 0 (lambda (x) (* x 10)))))
				(parameterize ((p 5))
					(p)))`,
			out: values.NewInteger(50),
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
// Parameter as Procedure Tests
// =============================================================================

func TestParameterAsProcedure(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "parameter called with no args returns value",
			code: `(let ((p (make-parameter 42)))
				(p))`,
			out: values.NewInteger(42),
		},
		{
			name: "parameter can be called like procedure",
			code: `(let ((p (make-parameter 'test)))
				(symbol? (p)))`,
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

// =============================================================================
// Parameter Mutation via Call Tests (R7RS §4.2.6)
//
// R7RS §4.2.6: Parameter objects can be called with one argument to set
// the value directly (applying the converter if present).
// =============================================================================

func TestParameterMutationViaCall(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "set parameter value by calling with 1 arg",
			code: `(let ((p (make-parameter 10)))
				(p 20)
				(p))`,
			out: values.NewInteger(20),
		},
		{
			name: "mutation applies converter",
			code: `(let ((p (make-parameter 0 (lambda (x) (* x 10)))))
				(p 3)
				(p))`,
			out: values.NewInteger(30),
		},
		{
			name: "mutation persists outside parameterize after direct set",
			code: `(let ((p (make-parameter 'a)))
				(p 'b)
				(p))`,
			out: values.NewSymbol("b"),
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
// Parameter Converter Error Tests (R7RS §4.2.6)
//
// R7RS §4.2.6: The converter is applied to the value passed to parameterize.
// If the converter raises an error, the parameterize body is not entered.
// =============================================================================

func TestParameterConverterErrors(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "converter error caught by guard",
			code: `(let ((p (make-parameter 0 (lambda (x)
						(if (not (number? x))
							(error "not a number" x)
							x)))))
				(guard (e (#t 'caught))
					(parameterize ((p "bad"))
						(p))))`,
			out: values.NewSymbol("caught"),
		},
		{
			name: "original value preserved after converter error",
			code: `(let ((p (make-parameter 42 (lambda (x)
						(if (not (number? x))
							(error "not a number")
							x)))))
				(guard (e (#t (p)))
					(parameterize ((p "bad"))
						'unreachable)))`,
			out: values.NewInteger(42),
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
// Deeply Nested Parameterize Tests (R7RS §4.2.6)
//
// R7RS §4.2.6: parameterize forms can be nested arbitrarily; each level
// restores the previous value on exit.
// =============================================================================

func TestDeeplyNestedParameterize(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "3 levels deep returns innermost value",
			code: `(let ((p (make-parameter 0)))
				(parameterize ((p 1))
					(parameterize ((p 2))
						(parameterize ((p 3))
							(p)))))`,
			out: values.NewInteger(3),
		},
		{
			name: "all levels restore correctly",
			code: `(let ((p (make-parameter 0)))
				(parameterize ((p 1))
					(parameterize ((p 2))
						(parameterize ((p 3))
							'ignore))
					(p)))`,
			out: values.NewInteger(1),
		},
		{
			name: "multiple parameters at multiple levels",
			code: `(let ((a (make-parameter 'a0))
				      (b (make-parameter 'b0)))
				(parameterize ((a 'a1) (b 'b1))
					(parameterize ((a 'a2))
						(parameterize ((b 'b3))
							(list (a) (b))))))`,
			out: values.List(values.NewSymbol("a2"), values.NewSymbol("b3")),
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
// Parameterize with call/cc Tests (R7RS §4.2.6, §6.10)
//
// R7RS §4.2.6: parameterize is defined in terms of dynamic-wind, so
// continuations captured inside parameterize should restore parameter
// values correctly when invoked.
// =============================================================================

func TestParameterizeWithCallCC(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "call/cc escape from parameterize returns modified value",
			code: `(let ((p (make-parameter 'original)))
				(call-with-current-continuation
					(lambda (escape)
						(parameterize ((p 'modified))
							(escape (p))))))`,
			out: values.NewSymbol("modified"),
		},
		{
			name: "parameter restored after call/cc escape",
			code: `(let ((p (make-parameter 'original)))
				(call-with-current-continuation
					(lambda (escape)
						(parameterize ((p 'modified))
							(escape 'done))))
				(p))`,
			out: values.NewSymbol("original"),
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
// current-input-port / current-output-port Parameter Tests
// =============================================================================

func TestCurrentPortParameters(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "current-input-port returns input port",
			code: "(input-port? (current-input-port))",
			out:  values.TrueValue,
		},
		{
			name: "current-output-port returns output port",
			code: "(output-port? (current-output-port))",
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
// C1: parameterize with converter must not double-apply converter on restore
// =============================================================================

func TestParameterizeConverterNoDoubleApply(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		// C1: After parameterize exits, the parameter must restore to the value
		// that was captured before entry — NOT re-convert it.
		// make-parameter applies the converter to init, storing 20.
		// parameterize captures old=20, sets p=10 (converted to 20 inside), then
		// restores. The restore must set p=20, not converter(20)=40.
		{
			name: "restore does not double-apply converter",
			code: `(let ((p (make-parameter 10 (lambda (x) (* x 2)))))
			   (parameterize ((p 5))
			     'ok)
			   (p))`,
			out: values.NewInteger(20),
		},
		// Identity converter: restoring with no converter is always correct.
		{
			name: "no converter - parameterize restores correctly",
			code: `(let ((p (make-parameter 10)))
			   (parameterize ((p 5))
			     'ok)
			   (p))`,
			out: values.NewInteger(10),
		},
		// Value inside parameterize is converter(new), not new.
		{
			name: "inside body sees converted value",
			code: `(let ((p (make-parameter 10 (lambda (x) (* x 2)))))
			   (parameterize ((p 3))
			     (p)))`,
			out: values.NewInteger(6),
		},
		// Original value is preserved across nested parameterize.
		{
			name: "nested parameterize restores correctly",
			code: `(let ((p (make-parameter 1 (lambda (x) (* x 2)))))
			   (parameterize ((p 3))
			     (parameterize ((p 5))
			       'inner))
			   (p))`,
			out: values.NewInteger(2),
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

// TestParameterizeValueEvalOrder verifies R7RS §4.2.6: every value expression
// (and converter) in a parameterize form is evaluated in the dynamic environment
// in effect *before* the parameterize, then bound. A later clause's value must NOT
// observe an earlier clause's freshly-installed binding.
func TestParameterizeValueEvalOrder(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		// The fix: b's init reads a; it must see the OLD a (1), not the new a (10).
		// Pre-fix this returned (10 1000); R7RS / Chez require (10 100).
		{
			name: "later value reads earlier param's OLD binding",
			code: `(let ((a (make-parameter 1)) (b (make-parameter 2)))
				(parameterize ((a 10) (b (* (a) 100)))
					(list (a) (b))))`,
			out: values.List(values.NewInteger(10), values.NewInteger(100)),
		},
		// Three clauses: c reads both a and b; both must be the OLD bindings.
		{
			name: "third value reads earlier params' OLD bindings",
			code: `(let ((a (make-parameter 1)) (b (make-parameter 2)) (c (make-parameter 3)))
				(parameterize ((a 10) (b (* (a) 100)) (c (+ (a) (b))))
					(list (a) (b) (c))))`,
			out: values.List(values.NewInteger(10), values.NewInteger(100), values.NewInteger(3)),
		},
		// Minimal case: q's init reads p; must see OLD p (1), not 10.
		{
			name: "minimal later-reads-earlier",
			code: `(let ((p (make-parameter 1)) (q (make-parameter 99)))
				(parameterize ((p 10) (q (p)))
					(q)))`,
			out: values.NewInteger(1),
		},
		// Symbolic form of the fix: q's value captures (p); must capture the old p.
		{
			name: "value expression sees pre-parameterize binding",
			code: `(let ((p (make-parameter 'old)) (q (make-parameter 'q0)))
				(parameterize ((p 'X) (q (list 'saw (p))))
					(q)))`,
			out: values.List(values.NewSymbol("saw"), values.NewSymbol("old")),
		},
		// Hygiene: the macro's introduced temporary (pt) must not capture a
		// user-bound identifier of the same name visible at the use site.
		{
			name: "introduced temp does not capture user pt",
			code: `(let ((p (make-parameter 0)) (pt 'user-pt))
				(parameterize ((p 5))
					(list (p) pt)))`,
			out: values.List(values.NewInteger(5), values.NewSymbol("user-pt")),
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
