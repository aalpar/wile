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

// ============================================================================
// Phase 8: Equality & Control Flow Tests
// ============================================================================
//
// This file contains comprehensive tests for:
// - eq? (identity comparison)
// - eqv? (equivalent values)
// - equal? (deep structural comparison)
// - apply (function application)
// - map (mapping over lists)
// - for-each (side-effect iteration)
// - call-with-values (multiple values)
// - values (return multiple values)
// - dynamic-wind (cleanup handlers)
// - not (boolean negation)

// ----------------------------------------------------------------------------
// eq? Tests (R7RS §6.1 - Identity comparison)
// ----------------------------------------------------------------------------

func TestEqQComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Symbols - same symbol name should be eq?
		{name: "same symbol", code: `(eq? 'foo 'foo)`, expected: values.TrueValue},
		{name: "different symbols", code: `(eq? 'foo 'bar)`, expected: values.FalseValue},

		// Booleans - singletons
		{name: "true eq? true", code: `(eq? #t #t)`, expected: values.TrueValue},
		{name: "false eq? false", code: `(eq? #f #f)`, expected: values.TrueValue},
		{name: "true not eq? false", code: `(eq? #t #f)`, expected: values.FalseValue},

		// Empty list - singleton
		{name: "empty list eq? empty list", code: `(eq? '() '())`, expected: values.TrueValue},

		// Small integers - implementation may cache these
		{name: "same small integer", code: `(eq? 5 5)`, expected: values.TrueValue},
		{name: "zero eq? zero", code: `(eq? 0 0)`, expected: values.TrueValue},

		// Characters - same character should be eq?
		{name: "same character", code: `(eq? #\a #\a)`, expected: values.TrueValue},
		{name: "different characters", code: `(eq? #\a #\b)`, expected: values.FalseValue},

		// Pairs - literals may be shared per R7RS §4.1.2
		// "The implementation may share storage between constants where appropriate."
		// This implementation interns literal lists, so they ARE eq?
		{name: "literal pairs same contents (interned)", code: `(eq? '(1 2) '(1 2))`, expected: values.TrueValue},
		{name: "different pairs different contents", code: `(eq? '(1) '(2))`, expected: values.FalseValue},

		// Strings - literals may be shared per R7RS §4.1.2
		// This implementation interns literal strings, so they ARE eq?
		{name: "literal strings same contents (interned)", code: `(eq? "hello" "hello")`, expected: values.TrueValue},

		// Vectors - literals are interned like pairs and strings
		{name: "literal vectors same contents (interned)", code: `(eq? #(1 2 3) #(1 2 3))`, expected: values.TrueValue},

		// Procedures - same procedure should be eq?
		{name: "same primitive", code: `(eq? + +)`, expected: values.TrueValue},
		{name: "different primitives", code: `(eq? + -)`, expected: values.FalseValue},

		// Cross-type comparisons
		{name: "integer vs symbol", code: `(eq? 1 'one)`, expected: values.FalseValue},
		{name: "string vs symbol", code: `(eq? "foo" 'foo)`, expected: values.FalseValue},
		{name: "empty list vs false", code: `(eq? '() #f)`, expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestEqQWithLetBinding(t *testing.T) {
	// Test that eq? works correctly with let bindings (same object)
	tcs := []schemeCodeTestCase{
		{
			name:     "same pair via let",
			code:     `(let ((x '(1 2 3))) (eq? x x))`,
			expected: values.TrueValue,
		},
		{
			name:     "same string via let",
			code:     `(let ((s "hello")) (eq? s s))`,
			expected: values.TrueValue,
		},
		{
			name:     "same vector via let",
			code:     `(let ((v #(1 2 3))) (eq? v v))`,
			expected: values.TrueValue,
		},
		{
			name:     "same lambda via let",
			code:     `(let ((f (lambda (x) x))) (eq? f f))`,
			expected: values.TrueValue,
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

// ----------------------------------------------------------------------------
// eqv? Tests (R7RS §6.1 - Equivalence predicate)
// ----------------------------------------------------------------------------

func TestEqvQComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// All eq? cases should also be eqv?
		{name: "same symbol", code: `(eqv? 'foo 'foo)`, expected: values.TrueValue},
		{name: "different symbols", code: `(eqv? 'foo 'bar)`, expected: values.FalseValue},
		{name: "true eqv? true", code: `(eqv? #t #t)`, expected: values.TrueValue},
		{name: "false eqv? false", code: `(eqv? #f #f)`, expected: values.TrueValue},
		{name: "empty list", code: `(eqv? '() '())`, expected: values.TrueValue},

		// Numbers - eqv? compares by value AND exactness
		{name: "same integers", code: `(eqv? 42 42)`, expected: values.TrueValue},
		{name: "different integers", code: `(eqv? 42 43)`, expected: values.FalseValue},
		{name: "negative integers", code: `(eqv? -5 -5)`, expected: values.TrueValue},
		{name: "same floats", code: `(eqv? 3.14 3.14)`, expected: values.TrueValue},
		{name: "different floats", code: `(eqv? 3.14 2.71)`, expected: values.FalseValue},
		{name: "zero floats", code: `(eqv? 0.0 0.0)`, expected: values.TrueValue},

		// Exact vs inexact - different exactness means not eqv?
		{name: "integer vs float same value", code: `(eqv? 42 42.0)`, expected: values.FalseValue},
		{name: "integer vs float zero", code: `(eqv? 0 0.0)`, expected: values.FalseValue},

		// Characters
		{name: "same characters", code: `(eqv? #\a #\a)`, expected: values.TrueValue},
		{name: "different characters", code: `(eqv? #\a #\b)`, expected: values.FalseValue},
		{name: "unicode characters", code: `(eqv? #\λ #\λ)`, expected: values.TrueValue},

		// Rationals
		{name: "same rationals", code: `(eqv? 1/2 1/2)`, expected: values.TrueValue},
		{name: "equivalent rationals", code: `(eqv? 2/4 1/2)`, expected: values.TrueValue},
		{name: "different rationals", code: `(eqv? 1/2 1/3)`, expected: values.FalseValue},

		// Complex numbers
		{name: "same complex", code: `(eqv? 1+2i 1+2i)`, expected: values.TrueValue},
		{name: "different complex", code: `(eqv? 1+2i 1+3i)`, expected: values.FalseValue},

		// BigInteger
		{name: "same big integer", code: `(eqv? #z123456789012345678901234567890 #z123456789012345678901234567890)`, expected: values.TrueValue},
		{name: "different big integers", code: `(eqv? #z123456789012345678901234567890 #z123456789012345678901234567891)`, expected: values.FalseValue},

		// Pairs - literals are interned, so they ARE eqv?
		{name: "literal pairs interned", code: `(eqv? '(1 2) '(1 2))`, expected: values.TrueValue},

		// Strings - literals are interned, so they ARE eqv?
		{name: "literal strings interned", code: `(eqv? "hello" "hello")`, expected: values.TrueValue},

		// Procedures
		{name: "same procedure", code: `(eqv? + +)`, expected: values.TrueValue},
		{name: "different procedures", code: `(eqv? + -)`, expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestEqvQSpecialValues(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Infinity
		{name: "positive infinity", code: `(eqv? +inf.0 +inf.0)`, expected: values.TrueValue},
		{name: "negative infinity", code: `(eqv? -inf.0 -inf.0)`, expected: values.TrueValue},
		{name: "pos inf vs neg inf", code: `(eqv? +inf.0 -inf.0)`, expected: values.FalseValue},

		// NaN - per R7RS, NaN is not eqv? to itself
		{name: "nan vs nan", code: `(eqv? +nan.0 +nan.0)`, expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// ----------------------------------------------------------------------------
// equal? Tests (R7RS §6.1 - Deep structural comparison)
// ----------------------------------------------------------------------------

func TestEqualQComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// All eqv? cases should also be equal?
		{name: "same integers", code: `(equal? 42 42)`, expected: values.TrueValue},
		{name: "same symbols", code: `(equal? 'foo 'foo)`, expected: values.TrueValue},
		{name: "same booleans", code: `(equal? #t #t)`, expected: values.TrueValue},
		{name: "same characters", code: `(equal? #\a #\a)`, expected: values.TrueValue},

		// Strings - equal? compares by content
		{name: "same content strings", code: `(equal? "hello" "hello")`, expected: values.TrueValue},
		{name: "different strings", code: `(equal? "hello" "world")`, expected: values.FalseValue},
		{name: "empty strings", code: `(equal? "" "")`, expected: values.TrueValue},
		{name: "unicode strings", code: `(equal? "λ" "λ")`, expected: values.TrueValue},

		// Lists - equal? does deep comparison
		{name: "same content lists", code: `(equal? '(1 2 3) '(1 2 3))`, expected: values.TrueValue},
		{name: "different lists", code: `(equal? '(1 2 3) '(1 2 4))`, expected: values.FalseValue},
		{name: "empty lists", code: `(equal? '() '())`, expected: values.TrueValue},
		{name: "nested lists", code: `(equal? '((1 2) (3 4)) '((1 2) (3 4)))`, expected: values.TrueValue},
		{name: "different nested lists", code: `(equal? '((1 2) (3 4)) '((1 2) (3 5)))`, expected: values.FalseValue},
		{name: "deeply nested", code: `(equal? '(((a))) '(((a))))`, expected: values.TrueValue},

		// Vectors - equal? compares element by element
		{name: "same content vectors", code: `(equal? #(1 2 3) #(1 2 3))`, expected: values.TrueValue},
		{name: "different vectors", code: `(equal? #(1 2 3) #(1 2 4))`, expected: values.FalseValue},
		{name: "empty vectors", code: `(equal? #() #())`, expected: values.TrueValue},
		{name: "vectors with symbols", code: `(equal? #(a b c) #(a b c))`, expected: values.TrueValue},

		// Mixed structures
		{name: "list with strings", code: `(equal? '("a" "b") '("a" "b"))`, expected: values.TrueValue},
		{name: "list with vector", code: `(equal? '(1 #(2 3)) '(1 #(2 3)))`, expected: values.TrueValue},

		// Pairs (improper lists)
		{name: "same improper list", code: `(equal? '(1 . 2) '(1 . 2))`, expected: values.TrueValue},
		{name: "different improper list", code: `(equal? '(1 . 2) '(1 . 3))`, expected: values.FalseValue},

		// Cross-type comparisons
		{name: "list vs vector", code: `(equal? '(1 2 3) #(1 2 3))`, expected: values.FalseValue},
		{name: "string vs symbol", code: `(equal? "foo" 'foo)`, expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// ----------------------------------------------------------------------------
// apply Tests (R7RS §6.4 - Function application)
// ----------------------------------------------------------------------------

func TestApplyComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic apply
		{name: "apply + to list", code: `(apply + '(1 2 3))`, expected: values.NewInteger(6)},
		{name: "apply * to list", code: `(apply * '(2 3 4))`, expected: values.NewInteger(24)},
		{name: "apply - to list", code: `(apply - '(10 3 2))`, expected: values.NewInteger(5)},

		// Apply with prefix arguments
		{name: "apply with one prefix", code: `(apply + 1 '(2 3))`, expected: values.NewInteger(6)},
		{name: "apply with two prefix", code: `(apply + 1 2 '(3 4))`, expected: values.NewInteger(10)},
		{name: "apply with many prefix", code: `(apply + 1 2 3 4 '(5))`, expected: values.NewInteger(15)},

		// Empty list
		{name: "apply + to empty list", code: `(apply + '())`, expected: values.NewInteger(0)},
		{name: "apply * to empty list", code: `(apply * '())`, expected: values.NewInteger(1)},
		{name: "apply list to empty list", code: `(apply list '())`, expected: values.EmptyList},

		// Apply with lambda
		{name: "apply lambda", code: `(apply (lambda (x y) (+ x y)) '(3 4))`, expected: values.NewInteger(7)},
		{name: "apply variadic lambda", code: `(apply (lambda args args) '(1 2 3))`, expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},

		// Apply with cons
		{name: "apply cons", code: `(apply cons '(1 2))`, expected: values.NewCons(values.NewInteger(1), values.NewInteger(2))},
		{name: "apply car", code: `(apply car '((1 2 3)))`, expected: values.NewInteger(1)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestApplyErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "apply non-procedure", code: `(apply 5 '(1 2))`},
		{name: "apply without list", code: `(apply + 1 2 3)`},
		{name: "apply with improper list", code: `(apply + '(1 . 2))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// ----------------------------------------------------------------------------
// map Tests (R7RS §6.4 - Mapping over lists)
// ----------------------------------------------------------------------------

func TestMapComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Single list
		{name: "map double", code: `(map (lambda (x) (* x 2)) '(1 2 3))`, expected: values.List(values.NewInteger(2), values.NewInteger(4), values.NewInteger(6))},
		{name: "map identity", code: `(map (lambda (x) x) '(a b c))`, expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))},
		{name: "map empty list", code: `(map (lambda (x) x) '())`, expected: values.EmptyList},

		// Multiple lists
		{name: "map + two lists", code: `(map + '(1 2 3) '(10 20 30))`, expected: values.List(values.NewInteger(11), values.NewInteger(22), values.NewInteger(33))},
		{name: "map - two lists", code: `(map - '(10 20 30) '(1 2 3))`, expected: values.List(values.NewInteger(9), values.NewInteger(18), values.NewInteger(27))},
		{name: "map three lists", code: `(map + '(1 2) '(10 20) '(100 200))`, expected: values.List(values.NewInteger(111), values.NewInteger(222))},

		// Map with list constructor
		{name: "map list", code: `(map list '(a b) '(1 2))`, expected: values.List(values.List(values.NewSymbol("a"), values.NewInteger(1)), values.List(values.NewSymbol("b"), values.NewInteger(2)))},

		// Map with cons
		{name: "map cons", code: `(map cons '(a b c) '(1 2 3))`, expected: values.List(values.NewCons(values.NewSymbol("a"), values.NewInteger(1)), values.NewCons(values.NewSymbol("b"), values.NewInteger(2)), values.NewCons(values.NewSymbol("c"), values.NewInteger(3)))},

		// Unequal lengths - stops at shortest
		{name: "unequal lengths", code: `(map + '(1 2 3) '(10 20))`, expected: values.List(values.NewInteger(11), values.NewInteger(22))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestMapErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "map non-procedure", code: `(map 5 '(1 2 3))`},
		{name: "map with non-list", code: `(map + 5)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// ----------------------------------------------------------------------------
// for-each Tests (R7RS §6.4 - Side-effect iteration)
// ----------------------------------------------------------------------------

func TestForEachComprehensive(t *testing.T) {
	// for-each returns unspecified value, we test via side effects
	tcs := []schemeCodeTestCase{
		// Verify side effects happen in order
		{
			name: "for-each side effects order",
			code: `(let ((result '()))
				(for-each (lambda (x) (set! result (cons x result))) '(1 2 3))
				result)`,
			expected: values.List(values.NewInteger(3), values.NewInteger(2), values.NewInteger(1)),
		},
		{
			name: "for-each multiple lists",
			code: `(let ((result '()))
				(for-each (lambda (x y) (set! result (cons (+ x y) result))) '(1 2 3) '(10 20 30))
				result)`,
			expected: values.List(values.NewInteger(33), values.NewInteger(22), values.NewInteger(11)),
		},
		{
			name: "for-each empty list",
			code: `(let ((called #f))
				(for-each (lambda (x) (set! called #t)) '())
				called)`,
			expected: values.FalseValue,
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

func TestForEachErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "for-each non-procedure", code: `(for-each 5 '(1 2 3))`},
		{name: "for-each with non-list", code: `(for-each (lambda (x) x) 5)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// ----------------------------------------------------------------------------
// call-with-values Tests (R7RS §6.4 - Multiple values)
// ----------------------------------------------------------------------------

func TestCallWithValuesComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Single value
		{name: "single value", code: `(call-with-values (lambda () 42) (lambda (x) x))`, expected: values.NewInteger(42)},

		// Multiple values
		{name: "two values", code: `(call-with-values (lambda () (values 1 2)) (lambda (a b) (+ a b)))`, expected: values.NewInteger(3)},
		{name: "three values", code: `(call-with-values (lambda () (values 1 2 3)) (lambda (a b c) (* a b c)))`, expected: values.NewInteger(6)},
		{name: "five values", code: `(call-with-values (lambda () (values 1 2 3 4 5)) (lambda (a b c d e) (+ a b c d e)))`, expected: values.NewInteger(15)},

		// Zero values
		{name: "zero values", code: `(call-with-values (lambda () (values)) (lambda () 'done))`, expected: values.NewSymbol("done")},

		// Consumer uses list
		{name: "consumer builds list", code: `(call-with-values (lambda () (values 'a 'b 'c)) list)`, expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))},

		// Values from arithmetic
		{name: "floor/ values", code: `(call-with-values (lambda () (floor/ 13 4)) (lambda (q r) (+ (* q 10) r)))`, expected: values.NewInteger(31)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestCallWithValuesErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "producer not procedure", code: `(call-with-values 5 (lambda (x) x))`},
		{name: "consumer not procedure", code: `(call-with-values (lambda () 1) 5)`},
		{name: "arity mismatch", code: `(call-with-values (lambda () (values 1 2)) (lambda (x) x))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// ----------------------------------------------------------------------------
// values Tests (R7RS §6.4 - Return multiple values)
// ----------------------------------------------------------------------------

func TestValuesComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Single value (direct return)
		{name: "single value", code: `(values 42)`, expected: values.NewInteger(42)},

		// Multiple values with call-with-values to capture
		{name: "two values via cwv", code: `(call-with-values (lambda () (values 1 2)) +)`, expected: values.NewInteger(3)},
		{name: "three values via cwv", code: `(call-with-values (lambda () (values 'a 'b 'c)) list)`, expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))},

		// Zero values
		{name: "zero values via cwv", code: `(call-with-values (lambda () (values)) (lambda () 'empty))`, expected: values.NewSymbol("empty")},

		// Values of different types
		{name: "mixed types", code: `(call-with-values (lambda () (values 1 "hello" 'sym)) list)`, expected: values.List(values.NewInteger(1), values.NewString("hello"), values.NewSymbol("sym"))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// ----------------------------------------------------------------------------
// dynamic-wind Tests (R7RS §6.4 - Cleanup handlers)
// ----------------------------------------------------------------------------

func TestDynamicWindComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic - returns thunk value
		{name: "returns thunk value", code: `(dynamic-wind (lambda () #f) (lambda () 42) (lambda () #f))`, expected: values.NewInteger(42)},

		// Execution order
		{
			name: "before runs first",
			code: `(let ((log '()))
				(dynamic-wind
					(lambda () (set! log (cons 'before log)))
					(lambda () (set! log (cons 'during log)) 'result)
					(lambda () (set! log (cons 'after log))))
				(reverse log))`,
			expected: values.List(values.NewSymbol("before"), values.NewSymbol("during"), values.NewSymbol("after")),
		},

		// After runs even on error (caught)
		{
			name: "after runs on normal exit",
			code: `(let ((after-ran #f))
				(dynamic-wind
					(lambda () #f)
					(lambda () 42)
					(lambda () (set! after-ran #t)))
				after-ran)`,
			expected: values.TrueValue,
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

func TestDynamicWindWithContinuations(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// After runs on continuation escape
		{
			name: "after runs on escape",
			code: `(let ((after-ran #f))
				(call/cc (lambda (k)
					(dynamic-wind
						(lambda () #f)
						(lambda () (k 'escaped))
						(lambda () (set! after-ran #t)))))
				after-ran)`,
			expected: values.TrueValue,
		},

		// Escape value is correct
		{
			name: "escape returns correct value",
			code: `(call/cc (lambda (k)
				(dynamic-wind
					(lambda () #f)
					(lambda () (k 77))
					(lambda () #f))))`,
			expected: values.NewInteger(77),
		},

		// Before/after state mutation visible
		{
			name: "before sets state",
			code: `(let ((v (make-vector 1 0)))
				(dynamic-wind
					(lambda () (vector-set! v 0 1))
					(lambda () (vector-ref v 0))
					(lambda () (vector-set! v 0 2))))`,
			expected: values.NewInteger(1),
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

func TestDynamicWindErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "before not procedure", code: `(dynamic-wind 5 (lambda () 1) (lambda () 2))`},
		{name: "thunk not procedure", code: `(dynamic-wind (lambda () 1) 5 (lambda () 2))`},
		{name: "after not procedure", code: `(dynamic-wind (lambda () 1) (lambda () 2) 5)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// ----------------------------------------------------------------------------
// not Tests (R7RS §6.3 - Boolean negation)
// ----------------------------------------------------------------------------

func TestNotComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Only #f is false
		{name: "not false is true", code: `(not #f)`, expected: values.TrueValue},

		// Everything else is true (returns #f)
		{name: "not true", code: `(not #t)`, expected: values.FalseValue},
		{name: "not zero", code: `(not 0)`, expected: values.FalseValue},
		{name: "not one", code: `(not 1)`, expected: values.FalseValue},
		{name: "not negative", code: `(not -1)`, expected: values.FalseValue},
		{name: "not empty list", code: `(not '())`, expected: values.FalseValue},
		{name: "not non-empty list", code: `(not '(1 2 3))`, expected: values.FalseValue},
		{name: "not empty string", code: `(not "")`, expected: values.FalseValue},
		{name: "not non-empty string", code: `(not "hello")`, expected: values.FalseValue},
		{name: "not symbol", code: `(not 'foo)`, expected: values.FalseValue},
		{name: "not vector", code: `(not #(1 2 3))`, expected: values.FalseValue},
		{name: "not empty vector", code: `(not #())`, expected: values.FalseValue},
		{name: "not procedure", code: `(not +)`, expected: values.FalseValue},
		{name: "not lambda", code: `(not (lambda (x) x))`, expected: values.FalseValue},
		{name: "not character", code: `(not #\a)`, expected: values.FalseValue},
		{name: "not float", code: `(not 3.14)`, expected: values.FalseValue},
		{name: "not rational", code: `(not 1/2)`, expected: values.FalseValue},
		{name: "not complex", code: `(not 1+2i)`, expected: values.FalseValue},

		// Double negation
		{name: "not not false", code: `(not (not #f))`, expected: values.FalseValue},
		{name: "not not true", code: `(not (not #t))`, expected: values.TrueValue},
		{name: "not not number", code: `(not (not 42))`, expected: values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// ----------------------------------------------------------------------------
// Combined Tests - Integration scenarios
// ----------------------------------------------------------------------------

func TestEqualityHierarchy(t *testing.T) {
	// Test that eq? implies eqv? implies equal?
	tcs := []schemeCodeTestCase{
		// If eq?, then eqv? and equal?
		{name: "symbol eq implies eqv", code: `(and (eq? 'foo 'foo) (eqv? 'foo 'foo))`, expected: values.TrueValue},
		{name: "symbol eq implies equal", code: `(and (eq? 'foo 'foo) (equal? 'foo 'foo))`, expected: values.TrueValue},

		// If eqv? but not eq? (numbers)
		{name: "numbers eqv but check equal", code: `(and (eqv? 42 42) (equal? 42 42))`, expected: values.TrueValue},

		// Since literals are interned, they ARE eqv? (so these tests verify that)
		{name: "literal pairs are eqv (interned)", code: `(and (equal? '(1 2) '(1 2)) (eqv? '(1 2) '(1 2)))`, expected: values.TrueValue},
		{name: "literal strings are eqv (interned)", code: `(and (equal? "hello" "hello") (eqv? "hello" "hello"))`, expected: values.TrueValue},

		// Test with non-interned objects (created at runtime)
		{name: "runtime pairs equal but not eqv", code: `(let ((a (list 1 2)) (b (list 1 2))) (and (equal? a b) (not (eqv? a b))))`, expected: values.TrueValue},
		// Note: Short strings (<=64 chars) are interned, so they ARE eqv? for short content.
		// This tests that equal? works for strings regardless of interning.
		{name: "strings created at runtime are equal", code: `(let ((a (string #\h #\i)) (b (string #\h #\i))) (equal? a b))`, expected: values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestControlFlowCombinations(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// map with apply
		{
			name:     "map with apply",
			code:     `(map (lambda (args) (apply + args)) '((1 2) (3 4) (5 6)))`,
			expected: values.List(values.NewInteger(3), values.NewInteger(7), values.NewInteger(11)),
		},

		// call-with-values with map
		{
			name:     "call-with-values from division in map",
			code:     `(map (lambda (n) (call-with-values (lambda () (floor/ n 3)) list)) '(10 11 12))`,
			expected: values.List(values.List(values.NewInteger(3), values.NewInteger(1)), values.List(values.NewInteger(3), values.NewInteger(2)), values.List(values.NewInteger(4), values.NewInteger(0))),
		},

		// dynamic-wind with map
		{
			name: "dynamic-wind inside map",
			code: `(let ((count 0))
				(map (lambda (x)
					(dynamic-wind
						(lambda () (set! count (+ count 1)))
						(lambda () x)
						(lambda () #f)))
					'(a b c))
				count)`,
			expected: values.NewInteger(3),
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
