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

package primitives_test

import (
	"testing"

	"wile/values"

	qt "github.com/frankban/quicktest"
)

// =============================================================================
// with-exception-handler Tests (R7RS §6.11)
// =============================================================================

func TestWithExceptionHandler(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "handler receives exception value with raise-continuable",
			code: `(with-exception-handler
				(lambda (e) e)
				(lambda () (raise-continuable 42)))`,
			out: values.NewInteger(42),
		},
		{
			name: "thunk returns normally without exception",
			code: `(with-exception-handler
				(lambda (e) 'error)
				(lambda () 'ok))`,
			out: values.NewSymbol("ok"),
		},
		{
			name: "thunk computes result",
			code: `(with-exception-handler
				(lambda (e) 0)
				(lambda () (+ 1 2 3)))`,
			out: values.NewInteger(6),
		},
		{
			name: "handler can transform exception value",
			code: `(with-exception-handler
				(lambda (e) (+ e 100))
				(lambda () (raise-continuable 5)))`,
			out: values.NewInteger(105),
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

func TestWithExceptionHandlerContinuable(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "continuable exception returns handler value",
			code: `(with-exception-handler
				(lambda (e) (+ e 10))
				(lambda () (raise-continuable 5)))`,
			out: values.NewInteger(15),
		},
		{
			name: "handler returns symbol",
			code: `(with-exception-handler
				(lambda (e) 'handled)
				(lambda () (raise-continuable 'error)))`,
			out: values.NewSymbol("handled"),
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

func TestWithExceptionHandlerNested(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "nested handlers - inner catches",
			code: `(with-exception-handler
				(lambda (e) 'outer)
				(lambda ()
					(with-exception-handler
						(lambda (e) 'inner)
						(lambda () (raise-continuable 'err)))))`,
			out: values.NewSymbol("inner"),
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

func TestWithExceptionHandlerNonContinuable(t *testing.T) {
	// Non-continuable exceptions (from raise) require the handler to
	// not return. If it returns, an error is raised.
	// We test that raise is non-continuable by verifying the error.
	_, err := runSchemeCode(t, `
		(with-exception-handler
			(lambda (e) 'tried-to-return)
			(lambda () (raise 'error)))
	`)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestWithExceptionHandlerErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "thunk is not a procedure",
			code: `(with-exception-handler (lambda (e) e) 42)`,
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
// raise Tests (R7RS §6.11)
// =============================================================================

func TestRaise(t *testing.T) {
	// raise creates a non-continuable exception
	// The handler cannot return from a non-continuable exception
	_, err := runSchemeCode(t, `
		(with-exception-handler
			(lambda (e) e)
			(lambda () (raise 'error)))
	`)
	// Should error because handler tried to return from non-continuable
	qt.Assert(t, err, qt.IsNotNil)
}

func TestRaiseWithCallCC(t *testing.T) {
	// Use call/cc to escape from non-continuable exception handler
	result, err := runSchemeCode(t, `
		(call/cc
			(lambda (escape)
				(with-exception-handler
					(lambda (e) (escape e))
					(lambda () (raise 'my-error)))))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewSymbol("my-error"))
}

// =============================================================================
// raise-continuable Tests (R7RS §6.11)
// =============================================================================

func TestRaiseContinuable(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "raise-continuable allows handler to return",
			code: `(with-exception-handler
				(lambda (e) 'handled)
				(lambda () (raise-continuable 'warning)))`,
			out: values.NewSymbol("handled"),
		},
		{
			name: "raise-continuable returns handler value",
			code: `(with-exception-handler
				(lambda (e) 42)
				(lambda () (raise-continuable 'test)))`,
			out: values.NewInteger(42),
		},
		{
			name: "raise-continuable passes value to handler",
			code: `(with-exception-handler
				(lambda (e) e)
				(lambda () (raise-continuable 'test-value)))`,
			out: values.NewSymbol("test-value"),
		},
		{
			name: "raise-continuable with integer",
			code: `(with-exception-handler
				(lambda (e) (+ e 1))
				(lambda () (raise-continuable 99)))`,
			out: values.NewInteger(100),
		},
		{
			name: "raise-continuable with list",
			code: `(with-exception-handler
				(lambda (e) (car e))
				(lambda () (raise-continuable '(a b c))))`,
			out: values.NewSymbol("a"),
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
// error Tests (R7RS §6.11)
// =============================================================================

func TestError(t *testing.T) {
	// error creates an error object and raises it as non-continuable
	// We use call/cc to escape from the handler
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "error creates error object - check with error-object?",
			code: `(call/cc
				(lambda (escape)
					(with-exception-handler
						(lambda (e) (escape (error-object? e)))
						(lambda () (error "test message")))))`,
			out: values.TrueValue,
		},
		{
			name: "error object has correct message",
			code: `(call/cc
				(lambda (escape)
					(with-exception-handler
						(lambda (e) (escape (error-object-message e)))
						(lambda () (error "hello world")))))`,
			out: values.NewString("hello world"),
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

func TestErrorWithIrritants(t *testing.T) {
	// Test error with irritants using call/cc to escape
	result, err := runSchemeCode(t, `
		(call/cc
			(lambda (escape)
				(with-exception-handler
					(lambda (e) (escape (error-object-irritants e)))
					(lambda () (error "msg" 1 2 3)))))
	`)
	qt.Assert(t, err, qt.IsNil)
	expected := values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	qt.Assert(t, result, values.SchemeEquals, expected)
}

func TestErrorErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "error with non-string message",
			code: `(error 42)`,
		},
		{
			name: "error with symbol message",
			code: `(error 'not-a-string)`,
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
// error-object? Tests (R7RS §6.11)
// =============================================================================

func TestErrorObjectQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "error-object? on error object via call/cc",
			code: `(call/cc
				(lambda (escape)
					(with-exception-handler
						(lambda (e) (escape (error-object? e)))
						(lambda () (error "test")))))`,
			out: values.TrueValue,
		},
		{
			name: "error-object? on integer",
			code: `(error-object? 42)`,
			out:  values.FalseValue,
		},
		{
			name: "error-object? on string",
			code: `(error-object? "hello")`,
			out:  values.FalseValue,
		},
		{
			name: "error-object? on symbol",
			code: `(error-object? 'foo)`,
			out:  values.FalseValue,
		},
		{
			name: "error-object? on list",
			code: `(error-object? '(1 2 3))`,
			out:  values.FalseValue,
		},
		{
			name: "error-object? on empty list",
			code: `(error-object? '())`,
			out:  values.FalseValue,
		},
		{
			name: "error-object? on boolean",
			code: `(error-object? #t)`,
			out:  values.FalseValue,
		},
		{
			name: "error-object? on procedure",
			code: `(error-object? (lambda () 1))`,
			out:  values.FalseValue,
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
// error-object-message Tests (R7RS §6.11)
// =============================================================================

func TestErrorObjectMessage(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "error-object-message extracts message",
			code: `(call/cc
				(lambda (escape)
					(with-exception-handler
						(lambda (e) (escape (error-object-message e)))
						(lambda () (error "my message")))))`,
			out: values.NewString("my message"),
		},
		{
			name: "error-object-message with empty message",
			code: `(call/cc
				(lambda (escape)
					(with-exception-handler
						(lambda (e) (escape (error-object-message e)))
						(lambda () (error "")))))`,
			out: values.NewString(""),
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

func TestErrorObjectMessageErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "error-object-message on integer",
			code: `(error-object-message 42)`,
		},
		{
			name: "error-object-message on string",
			code: `(error-object-message "hello")`,
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
// error-object-irritants Tests (R7RS §6.11)
// =============================================================================

func TestErrorObjectIrritants(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name: "error-object-irritants with multiple irritants",
			code: `(call/cc
				(lambda (escape)
					(with-exception-handler
						(lambda (e) (escape (error-object-irritants e)))
						(lambda () (error "msg" 'a 'b 'c)))))`,
			expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
		},
		{
			name: "error-object-irritants with no irritants",
			code: `(call/cc
				(lambda (escape)
					(with-exception-handler
						(lambda (e) (escape (error-object-irritants e)))
						(lambda () (error "msg")))))`,
			expected: values.EmptyList,
		},
		{
			name: "error-object-irritants with single irritant",
			code: `(call/cc
				(lambda (escape)
					(with-exception-handler
						(lambda (e) (escape (error-object-irritants e)))
						(lambda () (error "msg" 42)))))`,
			expected: values.List(values.NewInteger(42)),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestErrorObjectIrritantsErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "error-object-irritants on integer",
			code: `(error-object-irritants 42)`,
		},
		{
			name: "error-object-irritants on string",
			code: `(error-object-irritants "hello")`,
		},
		{
			name: "error-object-irritants on list",
			code: `(error-object-irritants '(1 2 3))`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
