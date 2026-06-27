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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestWithExceptionHandlerNonContinuable(t *testing.T) {
	// Non-continuable exceptions (from raise) require the handler to
	// not return. If it returns, an error is raised.
	// We test that raise is non-continuable by verifying the error.
	_, err := testhelpers.RunSchemeCode(t, `
		(with-exception-handler
			(lambda (e) 'tried-to-return)
			(lambda () (raise 'error)))
	`)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestWithExceptionHandlerErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "thunk is not a procedure",
			Code: `(with-exception-handler (lambda (e) e) 42)`,
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
// raise Tests (R7RS §6.11)
// =============================================================================

func TestRaise(t *testing.T) {
	// raise creates a non-continuable exception
	// The handler cannot return from a non-continuable exception
	_, err := testhelpers.RunSchemeCode(t, `
		(with-exception-handler
			(lambda (e) e)
			(lambda () (raise 'error)))
	`)
	// Should error because handler tried to return from non-continuable
	qt.Assert(t, err, qt.IsNotNil)
}

func TestRaiseWithCallCC(t *testing.T) {
	// Use call/cc to escape from non-continuable exception handler
	result, err := testhelpers.RunSchemeCode(t, `
		(call/cc
			(lambda (escape)
				(with-exception-handler
					(lambda (e) (escape e))
					(lambda () (raise 'my-error)))))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewSymbol("my-error"))
}

// =============================================================================
// raise-continuable Tests (R7RS §6.11)
// =============================================================================

// TestRaiseContinuableMultipleValues verifies R7RS §6.11: "the values returned by the
// handler become the values returned by raise-continuable" (plural). A handler
// returning (values 1 2 3) must not collapse to a single value.
func TestRaiseContinuableMultipleValues(t *testing.T) {
	result, err := testhelpers.RunSchemeCode(t, `
		(call-with-values
			(lambda ()
				(with-exception-handler
					(lambda (e) (values 1 2 3))
					(lambda () (raise-continuable 'x))))
			list)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "(1 2 3)")
}

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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestErrorWithIrritants(t *testing.T) {
	// Test error with irritants using call/cc to escape
	result, err := testhelpers.RunSchemeCode(t, `
		(call/cc
			(lambda (escape)
				(with-exception-handler
					(lambda (e) (escape (error-object-irritants e)))
					(lambda () (error "msg" 1 2 3)))))
	`)
	qt.Assert(t, err, qt.IsNil)
	expected := values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	qt.Assert(t, result, valuestest.SchemeEquals, expected)
}

func TestErrorErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "error with non-string message",
			Code: `(error 42)`,
		},
		{
			Name: "error with symbol message",
			Code: `(error 'not-a-string)`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestErrorTypeCheckIsCatchableCondition pins the CODING_STYLE "Error Message
// Pattern": error's own message-type check is user-facing (a Scheme guard can catch
// it), so it raises a NativeError whose message survives and whose offending value
// rides as an irritant — not a lossy %T Go-type string. Each case self-checks and
// returns #t.
func TestErrorTypeCheckIsCatchableCondition(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "non-string message is a catchable error-object",
			code: `(guard (e ((error-object? e) #t) (#t #f))
				(error 42))`,
			out: values.TrueValue,
		},
		{
			name: "offending value rides as an irritant",
			code: `(guard (e ((error-object? e)
			           (and (equal? (error-object-message e)
			                        "error: message must be a string")
			                (equal? (error-object-irritants e) '(42)))))
				(error 42))`,
			out: values.TrueValue,
		},
		{
			name: "symbol message carried as irritant",
			code: `(guard (e ((error-object? e)
			           (equal? (error-object-irritants e) '(not-a-string))))
				(error 'not-a-string))`,
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestErrorObjectMessageErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "error-object-message on integer",
			Code: `(error-object-message 42)`,
		},
		{
			Name: "error-object-message on string",
			Code: `(error-object-message "hello")`,
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

func TestErrorObjectIrritantsErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "error-object-irritants on integer",
			Code: `(error-object-irritants 42)`,
		},
		{
			Name: "error-object-irritants on string",
			Code: `(error-object-irritants "hello")`,
		},
		{
			Name: "error-object-irritants on list",
			Code: `(error-object-irritants '(1 2 3))`,
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
// R7RS Conformance Tests (R7RS §6.11)
// =============================================================================

// TestRaiseNonContinuableR7RS tests R7RS §6.11 non-continuable exception semantics
// R7RS: "Invoke the current exception handler on obj. The handler is called with
// the same dynamic environment as the call to raise, except that the current
// exception handler is the one that was in place when the handler was installed."
func TestRaiseNonContinuableR7RS(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "non-continuable exception handler must not return",
			// We use call/cc to properly escape from the non-continuable handler
			code: `(call/cc
				(lambda (escape)
					(with-exception-handler
						(lambda (e) (escape 'caught))
						(lambda () (raise 'error)))))`,
			out: values.NewSymbol("caught"),
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

// TestRaiseContinuableR7RS tests R7RS §6.11 continuable exception semantics
// R7RS: "If the handler returns, the returned values become the values returned by
// the call to raise-continuable."
func TestRaiseContinuableR7RS(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "continuable exception handler return value becomes result (tail position)",
			code: `(with-exception-handler
				(lambda (e) (+ e 100))
				(lambda () (raise-continuable 5)))`,
			out: values.NewInteger(105), // handler returns 105, raise-continuable in tail position
		},
		{
			name: "continuable handler can transform exception value",
			code: `(with-exception-handler
				(lambda (e) (string-append e "!"))
				(lambda () (raise-continuable "hello")))`,
			out: values.NewString("hello!"),
		},
		{
			name: "continuable handler with list processing",
			code: `(with-exception-handler
				(lambda (e) (length e))
				(lambda () (raise-continuable '(a b c d e))))`,
			out: values.NewInteger(5),
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

// TestRaiseContinuableResumption tests R7RS §6.11 resumption semantics
// R7RS: Handler's return value becomes the value of raise-continuable,
// and execution continues from that point.
func TestRaiseContinuableResumption(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "handler return continues at call site - addition",
			code: `(with-exception-handler
				(lambda (e) (+ e 100))
				(lambda () (+ (raise-continuable 5) 1)))`,
			out: values.NewInteger(106), // handler returns 105, then +1 = 106
		},
		{
			name: "handler return continues at call site - multiplication",
			code: `(with-exception-handler
				(lambda (e) (* e 2))
				(lambda () (* (raise-continuable 7) 3)))`,
			out: values.NewInteger(42), // handler returns 14, then *3 = 42
		},
		{
			name: "multiple expressions after raise-continuable",
			code: `(with-exception-handler
				(lambda (e) 'recovered)
				(lambda ()
					(let ((x (raise-continuable 'warning)))
						(list 'after x))))`,
			out: values.List(values.NewSymbol("after"), values.NewSymbol("recovered")),
		},
		{
			name: "raise-continuable in let binding",
			code: `(with-exception-handler
				(lambda (e) (* e 2))
				(lambda ()
					(let ((x (raise-continuable 5)))
						(+ x 3))))`,
			out: values.NewInteger(13), // (* 5 2) = 10, + 3 = 13
		},
		{
			name: "raise-continuable in conditional test",
			code: `(with-exception-handler
				(lambda (e) #t)
				(lambda ()
					(if (raise-continuable #f) 'yes 'no)))`,
			out: values.NewSymbol("yes"),
		},
		{
			name: "nested continuable exceptions",
			code: `(with-exception-handler
				(lambda (e) (+ e 10))
				(lambda ()
					(+ (raise-continuable 1)
					   (raise-continuable 2))))`,
			out: values.NewInteger(23), // (+ 11 12)
		},
		{
			name: "raise-continuable result used in function call",
			code: `(with-exception-handler
				(lambda (e) (list e e))
				(lambda ()
					(length (raise-continuable 'x))))`,
			out: values.NewInteger(2), // (list 'x 'x) = (x x), length = 2
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

// TestExceptionHandlerChain tests R7RS §6.11 handler chain semantics
// R7RS: "When an exception handler is invoked, the current exception handler
// is the one that was in place when the handler was installed."
func TestExceptionHandlerChain(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "handler can re-raise to outer handler with call/cc",
			code: `(call/cc
				(lambda (escape)
					(with-exception-handler
						(lambda (e) (escape (list 'outer e)))
						(lambda ()
							(with-exception-handler
								(lambda (e) (raise (list 'from-inner e)))
								(lambda () (raise-continuable 'original)))))))`,
			out: values.List(values.NewSymbol("outer"),
				values.List(values.NewSymbol("from-inner"), values.NewSymbol("original"))),
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

// TestErrorR7RS tests R7RS §6.11 error procedure semantics
// R7RS: "Raises an exception as if by calling raise on a newly allocated
// implementation-defined object which encapsulates the information provided
// by message, as well as any objs, known as the irritants."
func TestErrorR7RS(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "error creates error object with message and irritants",
			code: `(call/cc
				(lambda (escape)
					(with-exception-handler
						(lambda (e)
							(escape (list (error-object? e)
							              (error-object-message e)
							              (error-object-irritants e))))
						(lambda () (error "test" 'a 'b)))))`,
			out: values.List(values.TrueValue,
				values.NewString("test"),
				values.List(values.NewSymbol("a"), values.NewSymbol("b"))),
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
// guard Tests (R7RS §4.2.7)
// =============================================================================

// TestGuard tests the guard syntax for exception handling
func TestGuard(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "guard with else clause catches exception",
			code: `(guard (exn (else 'caught)) (raise 'test-error))`,
			out:  values.NewSymbol("caught"),
		},
		{
			name: "guard with else clause returns exception value",
			code: `(guard (exn (else exn)) (raise 'my-error))`,
			out:  values.NewSymbol("my-error"),
		},
		{
			name: "guard with else clause and expression",
			code: `(guard (exn (else (list 'caught exn))) (raise 42))`,
			out:  values.List(values.NewSymbol("caught"), values.NewInteger(42)),
		},
		{
			name: "guard with matching test clause",
			code: `(guard (exn ((eq? exn 'specific) 'matched)) (raise 'specific))`,
			out:  values.NewSymbol("matched"),
		},
		{
			name: "guard with test clause using exception value",
			code: `(guard (exn ((number? exn) (+ exn 100))) (raise 42))`,
			out:  values.NewInteger(142),
		},
		{
			name: "guard with multiple clauses - first matches",
			code: `(guard (exn
				((number? exn) 'was-number)
				((string? exn) 'was-string)
				(else 'other))
				(raise 123))`,
			out: values.NewSymbol("was-number"),
		},
		{
			name: "guard with multiple clauses - second matches",
			code: `(guard (exn
				((number? exn) 'was-number)
				((string? exn) 'was-string)
				(else 'other))
				(raise "hello"))`,
			out: values.NewSymbol("was-string"),
		},
		{
			name: "guard with multiple clauses - else matches",
			code: `(guard (exn
				((number? exn) 'was-number)
				((string? exn) 'was-string)
				(else 'other))
				(raise 'symbol))`,
			out: values.NewSymbol("other"),
		},
		{
			name: "guard normal execution - no exception",
			code: `(guard (exn (else 'error)) (+ 1 2))`,
			out:  values.NewInteger(3),
		},
		{
			name: "guard normal execution - complex body",
			code: `(guard (exn (else 'error))
				(let ((x 10) (y 20))
					(* x y)))`,
			out: values.NewInteger(200),
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

// TestGuardArrowClause tests guard with => clause (R7RS §4.2.7)
//
// Per R7RS §4.2.7, (test => expr) evaluates expr and passes the result
// of test to it. The test must return a useful truthy value (not just #t),
// since that value is what gets passed to the procedure.
func TestGuardArrowClause(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "guard with => clause - assq result passed to cdr",
			code: `(guard (exn ((assq 'a exn) => cdr)) (raise '((a . 42))))`,
			out:  values.NewInteger(42),
		},
		{
			name: "guard with => clause - custom procedure",
			code: `(guard (exn ((assq 'val exn) => (lambda (p) (* (cdr p) 2)))) (raise '((val . 21))))`,
			out:  values.NewInteger(42),
		},
		{
			name: "guard with => clause - fallthrough to second clause",
			code: `(guard (exn ((assq 'a exn) => cdr) ((assq 'b exn))) (raise '((b . 23))))`,
			out:  values.NewCons(values.NewSymbol("b"), values.NewInteger(23)),
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

// TestGuardReraise tests that guard re-raises when no clause matches (R7RS §4.2.7)
func TestGuardReraise(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "guard re-raises to outer handler when no clause matches",
			code: `(call/cc
				(lambda (escape)
					(with-exception-handler
						(lambda (e) (escape (list 'outer e)))
						(lambda ()
							(guard (exn ((number? exn) 'was-number))
								(raise 'symbol-error))))))`,
			out: values.List(values.NewSymbol("outer"), values.NewSymbol("symbol-error")),
		},
		{
			name: "guard re-raises preserves exception value",
			code: `(call/cc
				(lambda (escape)
					(with-exception-handler
						(lambda (e) (escape e))
						(lambda ()
							(guard (exn ((string? exn) 'was-string))
								(raise 42))))))`,
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

// TestGuardWithError tests guard with error objects (R7RS §4.2.7, §6.11)
func TestGuardWithError(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "guard catches error object",
			code: `(guard (exn ((error-object? exn) (error-object-message exn)))
				(error "test message"))`,
			out: values.NewString("test message"),
		},
		{
			name: "guard accesses error irritants",
			code: `(guard (exn ((error-object? exn) (error-object-irritants exn)))
				(error "msg" 'a 'b 'c))`,
			out: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
		},
		{
			name: "guard with error-object? test",
			code: `(guard (exn
				((error-object? exn) 'was-error)
				(else 'was-other))
				(error "oops"))`,
			out: values.NewSymbol("was-error"),
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

// TestGuardNested tests nested guard expressions (R7RS §4.2.7)
// Note: Some nested guard patterns may have limitations due to continuation semantics.
func TestGuardNested(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "guard inside let - exception caught",
			code: `(let ((x 10))
				(guard (exn (else (+ x exn)))
					(raise 5)))`,
			out: values.NewInteger(15),
		},
		{
			name: "guard with computation before raise",
			code: `(guard (exn (else 'caught))
				(let ((x (+ 1 2)))
					(if (= x 3)
						(raise 'expected)
						'unexpected)))`,
			out: values.NewSymbol("caught"),
		},
		{
			name: "guard in procedure",
			code: `(let ((safe-div (lambda (a b)
				(guard (exn (else 0))
					(/ a b)))))
				(safe-div 10 2))`,
			out: values.NewInteger(5),
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
// Exception in Handler Tests (R7RS §6.11)
// =============================================================================

// TestExceptionInHandler tests R7RS §6.11 semantics when the exception handler
// itself raises an exception. R7RS: "the current exception handler is the one
// that was in place when the handler being called was installed."
func TestExceptionInHandler(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "inner handler raises, outer handler catches",
			code: `(call/cc
				(lambda (escape)
					(with-exception-handler
						(lambda (e) (escape (list 'outer e)))
						(lambda ()
							(with-exception-handler
								(lambda (e) (raise (list 'reraised e)))
								(lambda () (raise 'original)))))))`,
			out: values.List(values.NewSymbol("outer"),
				values.List(values.NewSymbol("reraised"), values.NewSymbol("original"))),
		},
		{
			name: "handler raises different exception type",
			code: `(call/cc
				(lambda (escape)
					(with-exception-handler
						(lambda (e) (escape (error-object-message e)))
						(lambda ()
							(with-exception-handler
								(lambda (e) (error "handler failed" e))
								(lambda () (raise 'bad)))))))`,
			out: values.NewString("handler failed"),
		},
		{
			name: "handler raises continuable to outer handler",
			code: `(with-exception-handler
				(lambda (e) (list 'outer-handled e))
				(lambda ()
					(with-exception-handler
						(lambda (e) (raise-continuable (list 'wrapped e)))
						(lambda () (raise-continuable 'start)))))`,
			out: values.List(values.NewSymbol("outer-handled"),
				values.List(values.NewSymbol("wrapped"), values.NewSymbol("start"))),
		},
		{
			name: "three layers - innermost raises to middle, middle raises to outer",
			code: `(call/cc
				(lambda (escape)
					(with-exception-handler
						(lambda (e) (escape (list 'L1 e)))
						(lambda ()
							(with-exception-handler
								(lambda (e) (raise (list 'L2 e)))
								(lambda ()
									(with-exception-handler
										(lambda (e) (raise (list 'L3 e)))
										(lambda () (raise 'origin)))))))))`,
			out: values.List(values.NewSymbol("L1"),
				values.List(values.NewSymbol("L2"),
					values.List(values.NewSymbol("L3"), values.NewSymbol("origin")))),
		},
		{
			name: "handler error object propagates to outer",
			code: `(call/cc
				(lambda (escape)
					(with-exception-handler
						(lambda (e)
							(escape (list (error-object? e)
							              (error-object-message e))))
						(lambda ()
							(with-exception-handler
								(lambda (e) (error "inner handler broke"))
								(lambda () (raise 'trigger)))))))`,
			out: values.List(values.TrueValue, values.NewString("inner handler broke")),
		},
		{
			name: "guard handler raises to outer guard",
			code: `(guard (outer-exn
					((string? outer-exn) (string-append "outer: " outer-exn)))
				(guard (inner-exn
						((number? inner-exn) 'was-number))
					(raise "not-a-number")))`,
			out: values.NewString("outer: not-a-number"),
		},
		{
			name: "guard body raises error, handler clause re-raises transformed value",
			code: `(call/cc
				(lambda (escape)
					(with-exception-handler
						(lambda (e) (escape e))
						(lambda ()
							(guard (exn
								((number? exn) (raise (* exn 10))))
								(raise 5))))))`,
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
// Deeply Nested Guard Tests (R7RS §4.2.7)
// =============================================================================

// TestGuardDeeplyNested tests deeply nested guard forms with exceptions
// propagating through multiple levels.
func TestGuardDeeplyNested(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "two nested guards - inner catches",
			code: `(guard (outer (else 'outer-caught))
				(guard (inner ((number? inner) (+ inner 100)))
					(raise 42)))`,
			out: values.NewInteger(142),
		},
		{
			name: "two nested guards - inner misses, outer catches",
			code: `(guard (outer ((symbol? outer) (list 'outer-caught outer)))
				(guard (inner ((number? inner) 'was-number))
					(raise 'not-a-number)))`,
			out: values.List(values.NewSymbol("outer-caught"), values.NewSymbol("not-a-number")),
		},
		{
			name: "three nested guards - innermost catches",
			code: `(guard (L1 (else 'L1))
				(guard (L2 (else 'L2))
					(guard (L3 ((number? L3) (* L3 2)))
						(raise 7))))`,
			out: values.NewInteger(14),
		},
		{
			name: "three nested guards - middle catches",
			code: `(guard (L1 (else 'L1))
				(guard (L2 ((symbol? L2) (list 'L2 L2)))
					(guard (L3 ((number? L3) 'was-number))
						(raise 'oops))))`,
			out: values.List(values.NewSymbol("L2"), values.NewSymbol("oops")),
		},
		{
			name: "three nested guards - outermost catches",
			code: `(guard (L1 ((list? L1) (length L1)))
				(guard (L2 ((number? L2) 'was-number))
					(guard (L3 ((string? L3) 'was-string))
						(raise '(a b c)))))`,
			out: values.NewInteger(3),
		},
		{
			name: "four nested guards - deepest catches",
			code: `(guard (L1 (else 'L1))
				(guard (L2 (else 'L2))
					(guard (L3 (else 'L3))
						(guard (L4 ((eq? L4 'target) 'hit))
							(raise 'target)))))`,
			out: values.NewSymbol("hit"),
		},
		{
			name: "four nested guards - outermost catches",
			code: `(guard (L1 ((char? L1) (list 'L1-caught L1)))
				(guard (L2 ((number? L2) 'L2))
					(guard (L3 ((string? L3) 'L3))
						(guard (L4 ((symbol? L4) 'L4))
							(raise #\x)))))`,
			out: values.List(values.NewSymbol("L1-caught"), values.NewCharacter('x')),
		},
		{
			name: "nested guards with body computation at each level",
			code: `(guard (L1 ((number? L1) (+ L1 1000)))
				(let ((a 10))
					(guard (L2 ((string? L2) 'was-string))
						(let ((b (* a 2)))
							(guard (L3 ((symbol? L3) 'was-symbol))
								(let ((c (+ b 5)))
									(raise c)))))))`,
			out: values.NewInteger(1025), // c = 25, + 1000 = 1025
		},
		{
			name: "nested guards - normal return bypasses all guards",
			code: `(guard (L1 (else 'L1))
				(guard (L2 (else 'L2))
					(guard (L3 (else 'L3))
						(+ 10 20 30))))`,
			out: values.NewInteger(60),
		},
		{
			name: "nested guard with => clause at inner level",
			code: `(guard (outer (else 'outer))
				(guard (inner ((assq 'n inner) => (lambda (p) (* (cdr p) (cdr p)))))
					(raise '((n . 9)))))`,
			out: values.NewInteger(81),
		},
		{
			name: "nested guard with error objects propagating",
			code: `(guard (outer
					((error-object? outer)
					 (string-append "caught: " (error-object-message outer))))
				(guard (inner ((number? inner) 'was-number))
					(error "deep failure" 'x 'y)))`,
			out: values.NewString("caught: deep failure"),
		},
		{
			name: "guard inside loop-like recursion",
			code: `(let loop ((n 3) (acc '()))
				(if (= n 0)
					acc
					(loop (- n 1)
						(cons
							(guard (exn ((number? exn) exn))
								(raise n))
							acc))))`,
			out: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
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

// TestGuardBodyMultipleValues verifies that guard propagates multiple values
// from its body to the caller when no exception is raised.
//
// The R7RS §7.3 reference implementation uses (let ((result (begin e1 e2 ...))))
// which loses multiple values: let expects exactly one value per binding, so
// (values 1 2) in the body causes a wrong-argument-count error. This test
// pins the correct behavior after the fix.
func TestGuardBodyMultipleValues(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "two values propagate when no exception raised",
			code: `(call-with-values
						(lambda () (guard (e (#f)) (values 1 2)))
						list)`,
			out: values.List(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			name: "three values propagate when no exception raised",
			code: `(call-with-values
						(lambda () (guard (e (#f)) (values 10 20 30)))
						list)`,
			out: values.List(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
		},
		{
			name: "single value still works after fix",
			code: `(guard (e (#f)) 42)`,
			out:  values.NewInteger(42),
		},
		{
			name: "multi-expression body last expr produces two values",
			code: `(call-with-values
						(lambda ()
							(guard (e (#f))
								(+ 1 0)
								(values 'a 'b)))
						list)`,
			out: values.List(values.NewSymbol("a"), values.NewSymbol("b")),
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

// TestFileErrorPredicate tests the file-error? predicate (R7RS §6.11)
func TestFileErrorPredicate(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "file-error? on open-input-file with nonexistent file",
			code: `(file-error? (guard (exn (else exn)) (open-input-file " no such file ")))`,
			out:  values.TrueValue,
		},
		{
			name: "file-error? on generic error returns #f",
			code: `(file-error? (guard (exn (else exn)) (error "generic")))`,
			out:  values.FalseValue,
		},
		{
			name: "file-error? on non-error returns #f",
			code: `(file-error? 42)`,
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

// TestReadErrorPredicate tests the read-error? predicate (R7RS §6.11)
func TestReadErrorPredicate(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "read-error? on malformed input",
			code: `(read-error? (guard (exn (else exn)) (read (open-input-string "#\\badname"))))`,
			out:  values.TrueValue,
		},
		{
			name: "read-error? on generic error returns #f",
			code: `(read-error? (guard (exn (else exn)) (error "generic")))`,
			out:  values.FalseValue,
		},
		{
			name: "read-error? on non-error returns #f",
			code: `(read-error? "hello")`,
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
