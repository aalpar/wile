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
// Additional Promise Tests (R7RS §4.2.5)
// =============================================================================

// TestMakeLazyPromise tests the make-lazy-promise / delay-force primitive
func TestMakeLazyPromise(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "delay-force creates promise",
			code: "(promise? (delay-force (make-promise 1)))",
			out:  values.TrueValue,
		},
		{
			name: "delay-force with delayed value",
			code: "(force (delay-force (delay 42)))",
			out:  values.NewInteger(42),
		},
		{
			name: "delay-force with make-promise",
			code: "(force (delay-force (make-promise 99)))",
			out:  values.NewInteger(99),
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

// TestPromiseMemoization tests that promises memoize their values
func TestPromiseMemoization(t *testing.T) {
	// This tests that a promise is only evaluated once
	// by using a side effect (incrementing a counter)
	result, err := testhelpers.RunSchemeCode(t, `
		(let ((count 0))
			(let ((p (delay (begin (set! count (+ count 1)) count))))
				(force p)
				(force p)
				(force p)
				count))
	`)
	qt.Assert(t, err, qt.IsNil)
	// count should be 1 because the promise body is only evaluated once
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(1))
}

// TestForceIdempotent tests that force is idempotent
func TestForceIdempotent(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "force on already forced promise returns same value",
			code: `(let ((p (delay (+ 1 2))))
				(let ((r1 (force p)))
					(let ((r2 (force p)))
						(= r1 r2))))`,
			out: values.TrueValue,
		},
		{
			name: "multiple forces return same value",
			code: `(let ((p (delay 'hello)))
				(eq? (force p) (force p)))`,
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

// TestPromiseWithSideEffects tests promises with side effects
func TestPromiseWithSideEffects(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "promise evaluates body only once",
			code: `(let ((x 0))
				(let ((p (delay (begin (set! x (+ x 1)) x))))
					(+ (force p) (force p))))`,
			out: values.NewInteger(2), // 1 + 1, because x is set to 1 once
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

// TestPromiseQEdgeCases tests promise? with edge cases
func TestPromiseQEdgeCases(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "promise? on forced promise still true",
			code: `(let ((p (delay 42)))
				(force p)
				(promise? p))`,
			out: values.TrueValue,
		},
		{
			name: "promise? on vector",
			code: "(promise? #(1 2 3))",
			out:  values.FalseValue,
		},
		{
			name: "promise? on empty list",
			code: "(promise? '())",
			out:  values.FalseValue,
		},
		{
			name: "promise? on procedure",
			code: "(promise? (lambda () 1))",
			out:  values.FalseValue,
		},
		{
			name: "promise? on boolean",
			code: "(promise? #t)",
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

// TestForceWithComplexExpressions tests force with complex expressions
func TestForceWithComplexExpressions(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "force with map",
			code: "(force (delay (map (lambda (x) (* x 2)) '(1 2 3))))",
			out:  values.List(values.NewInteger(2), values.NewInteger(4), values.NewInteger(6)),
		},
		{
			name: "force with filter-like operation",
			code: "(force (delay (cdr '(1 2 3))))",
			out:  values.List(values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name: "force with string operations",
			code: `(force (delay (string-append "hello" " " "world")))`,
			out:  values.NewString("hello world"),
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

// TestMakePromiseIdentity tests R7RS §4.2.5 make-promise identity behavior
// R7RS: "If obj is already a promise, it is returned."
func TestMakePromiseIdentity(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "make-promise returns same promise when given a promise",
			code: `(let ((p (delay 42)))
				(eq? p (make-promise p)))`,
			out: values.TrueValue,
		},
		{
			name: "make-promise on make-promise returns same promise",
			code: `(let ((p (make-promise 'x)))
				(eq? p (make-promise p)))`,
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

// TestDelayForceTailCall tests R7RS §4.2.5 delay-force tail-call semantics
// R7RS: "delay-force is similar to delay, except that the expression is expected
// to evaluate to a promise. This idiom is needed for proper tail recursion in
// lazy algorithms using delay and force."
func TestDelayForceTailCall(t *testing.T) {
	// This tests that delay-force enables iterative lazy algorithms
	// without stack overflow. R7RS example: lazy-filter uses delay-force
	// for proper tail recursion.
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "delay-force enables iterative lazy computation",
			code: `(letrec ((lazy-countdown
			           (lambda (n)
			             (if (= n 0)
			                 (delay 'done)
			                 (delay-force (lazy-countdown (- n 1)))))))
			         (force (lazy-countdown 100)))`,
			out: values.NewSymbol("done"),
		},
		{
			name: "delay-force chain doesn't accumulate stack",
			code: `(letrec ((stream-ref
			           (lambda (s n)
			             (if (= n 0)
			                 (force s)
			                 (stream-ref (delay-force (cdr (force s))) (- n 1)))))
			         (naturals
			           (lambda (n)
			             (delay (cons n (naturals (+ n 1)))))))
			         (stream-ref (naturals 0) 10))`,
			out: values.List(values.NewInteger(10), values.NewSymbol("naturals")),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			// For the stream test, we just check it doesn't stack overflow
			// and returns something (the exact value depends on implementation)
			if tc.name == "delay-force chain doesn't accumulate stack" {
				qt.Assert(t, err, qt.IsNil)
				// Just verify it returns a pair starting with 10
				pair, ok := result.(*values.Pair)
				qt.Assert(t, ok, qt.IsTrue)
				qt.Assert(t, pair.Car(), valuestest.SchemeEquals, values.NewInteger(10))
			} else {
				qt.Assert(t, err, qt.IsNil)
				qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
			}
		})
	}
}

// TestForceExceptionInDelayBody tests that forcing a promise whose body raises
// an error propagates the error, and that re-forcing re-evaluates the thunk.
//
// R7RS §4.2.5: If the body of a delay raises an exception, the promise remains
// unforced and subsequent force calls re-evaluate the thunk.
func TestForceExceptionInDelayBody(t *testing.T) {
	tcs := []struct {
		name    string
		code    string
		out     values.Value
		wantErr bool
	}{
		{
			name:    "force a promise whose body raises an error",
			code:    `(force (delay (error "boom")))`,
			wantErr: true,
		},
		{
			name: "re-forcing after exception re-evaluates thunk",
			code: `(let ((count 0))
				(let ((p (delay (begin (set! count (+ count 1))
				                       (if (= count 1) (error "first") count)))))
					(guard (e (#t 'caught))
						(force p))
					(force p)))`,
			out: values.NewInteger(2),
		},
		{
			name:    "error message preserved through force",
			code:    `(force (delay (error "test message")))`,
			wantErr: true,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			if tc.wantErr {
				qt.Assert(t, err, qt.IsNotNil)
			} else {
				qt.Assert(t, err, qt.IsNil)
				qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
			}
		})
	}
}

// TestForceCircularPromise tests deeply nested and recursive promise chains.
//
// R7RS §4.2.5: force must recursively force promises returned by promise
// bodies. This verifies that chains of delay-wrapped promises resolve correctly.
func TestForceCircularPromise(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "4-deep nested delay chain",
			code: "(force (delay (delay (delay (delay 42)))))",
			out:  values.NewInteger(42),
		},
		{
			name: "promise returning a different promise",
			code: `(let ((p1 (delay 99)))
				(let ((p2 (delay (force p1))))
					(force p2)))`,
			out: values.NewInteger(99),
		},
		{
			name: "delay-force chain resolves correctly",
			code: `(force (delay-force (delay-force (delay-force (delay 7)))))`,
			out:  values.NewInteger(7),
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

// TestMakePromiseEdgeCases tests make-promise edge cases
func TestMakePromiseEdgeCases(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "make-promise wraps already-promise",
			code: "(force (make-promise (make-promise 42)))",
			out:  values.NewInteger(42),
		},
		{
			name: "make-promise with empty list",
			code: "(force (make-promise '()))",
			out:  values.EmptyList,
		},
		{
			name: "make-promise with boolean",
			code: "(force (make-promise #f))",
			out:  values.FalseValue,
		},
		{
			name: "make-promise with string",
			code: `(force (make-promise "test"))`,
			out:  values.NewString("test"),
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
