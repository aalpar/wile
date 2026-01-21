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
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// TestPromiseMemoization tests that promises memoize their values
func TestPromiseMemoization(t *testing.T) {
	// This tests that a promise is only evaluated once
	// by using a side effect (incrementing a counter)
	result, err := runSchemeCode(t, `
		(let ((count 0))
			(let ((p (delay (begin (set! count (+ count 1)) count))))
				(force p)
				(force p)
				(force p)
				count))
	`)
	qt.Assert(t, err, qt.IsNil)
	// count should be 1 because the promise body is only evaluated once
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(1))
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
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}
