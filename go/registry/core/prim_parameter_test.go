// Copyright 2025 Aaron Alpar
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

	"wile/values"

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
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestMakeParameterErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "converter not a procedure",
			code: `(make-parameter 42 "not-a-proc")`,
		},
		{
			name: "converter is integer",
			code: `(make-parameter 42 123)`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
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
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestParameterizeRestoresOnException(t *testing.T) {
	// Test that parameterize restores parameter value even when exception occurs
	result, err := runSchemeCode(t, `
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
	qt.Assert(t, result, values.SchemeEquals, values.NewSymbol("original"))
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
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}
