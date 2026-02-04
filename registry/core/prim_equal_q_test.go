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
	"time"

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// equal? Tests (R7RS §6.1 - Deep structural comparison)

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

// R7RS §6.1: "The equal? procedure must terminate even if its arguments
// are circular data structures."

func TestEqualQCircular_SelfReferentialList(t *testing.T) {
	result, err := runSchemeCodeWithTimeout(t, `
		(let ((x (list 1 2 3)))
		  (set-cdr! (cdr (cdr x)) x)
		  (equal? x x))
	`, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestEqualQCircular_TwoIdenticalCircularLists(t *testing.T) {
	result, err := runSchemeCodeWithTimeout(t, `
		(let ((a (list 1 2))
		      (b (list 1 2)))
		  (set-cdr! (cdr a) a)
		  (set-cdr! (cdr b) b)
		  (equal? a b))
	`, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestEqualQCircular_DifferentCircularLists(t *testing.T) {
	result, err := runSchemeCodeWithTimeout(t, `
		(let ((a (list 1))
		      (b (list 2)))
		  (set-cdr! a a)
		  (set-cdr! b b)
		  (equal? a b))
	`, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

func TestEqualQCircular_CircularVector(t *testing.T) {
	result, err := runSchemeCodeWithTimeout(t, `
		(let ((v (vector 0)))
		  (vector-set! v 0 v)
		  (equal? v v))
	`, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestEqualQCircular_PairContainingCircularVector(t *testing.T) {
	result, err := runSchemeCodeWithTimeout(t, `
		(let ((v (vector 0)))
		  (vector-set! v 0 v)
		  (let ((a (list v))
		        (b (list v)))
		    (equal? a b)))
	`, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}
