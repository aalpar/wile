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
	"time"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// equal? Tests (R7RS §6.1 - Deep structural comparison)

func TestEqualQComprehensive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// All eqv? cases should also be equal?
		{Name: "same integers", Code: `(equal? 42 42)`, Expected: values.TrueValue},
		{Name: "same symbols", Code: `(equal? 'foo 'foo)`, Expected: values.TrueValue},
		{Name: "same booleans", Code: `(equal? #t #t)`, Expected: values.TrueValue},
		{Name: "same characters", Code: `(equal? #\a #\a)`, Expected: values.TrueValue},

		// Strings - equal? compares by content
		{Name: "same content strings", Code: `(equal? "hello" "hello")`, Expected: values.TrueValue},
		{Name: "different strings", Code: `(equal? "hello" "world")`, Expected: values.FalseValue},
		{Name: "empty strings", Code: `(equal? "" "")`, Expected: values.TrueValue},
		{Name: "unicode strings", Code: `(equal? "λ" "λ")`, Expected: values.TrueValue},

		// Lists - equal? does deep comparison
		{Name: "same content lists", Code: `(equal? '(1 2 3) '(1 2 3))`, Expected: values.TrueValue},
		{Name: "different lists", Code: `(equal? '(1 2 3) '(1 2 4))`, Expected: values.FalseValue},
		{Name: "empty lists", Code: `(equal? '() '())`, Expected: values.TrueValue},
		{Name: "nested lists", Code: `(equal? '((1 2) (3 4)) '((1 2) (3 4)))`, Expected: values.TrueValue},
		{Name: "different nested lists", Code: `(equal? '((1 2) (3 4)) '((1 2) (3 5)))`, Expected: values.FalseValue},
		{Name: "deeply nested", Code: `(equal? '(((a))) '(((a))))`, Expected: values.TrueValue},

		// Vectors - equal? compares element by element
		{Name: "same content vectors", Code: `(equal? #(1 2 3) #(1 2 3))`, Expected: values.TrueValue},
		{Name: "different vectors", Code: `(equal? #(1 2 3) #(1 2 4))`, Expected: values.FalseValue},
		{Name: "empty vectors", Code: `(equal? #() #())`, Expected: values.TrueValue},
		{Name: "vectors with symbols", Code: `(equal? #(a b c) #(a b c))`, Expected: values.TrueValue},

		// Mixed structures
		{Name: "list with strings", Code: `(equal? '("a" "b") '("a" "b"))`, Expected: values.TrueValue},
		{Name: "list with vector", Code: `(equal? '(1 #(2 3)) '(1 #(2 3)))`, Expected: values.TrueValue},

		// Pairs (improper lists)
		{Name: "same improper list", Code: `(equal? '(1 . 2) '(1 . 2))`, Expected: values.TrueValue},
		{Name: "different improper list", Code: `(equal? '(1 . 2) '(1 . 3))`, Expected: values.FalseValue},

		// Cross-type comparisons
		{Name: "list vs vector", Code: `(equal? '(1 2 3) #(1 2 3))`, Expected: values.FalseValue},
		{Name: "string vs symbol", Code: `(equal? "foo" 'foo)`, Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestEqualityHierarchy(t *testing.T) {
	// Test that eq? implies eqv? implies equal?
	tcs := []testhelpers.SchemeCodeTestCase{
		// If eq?, then eqv? and equal?
		{Name: "symbol eq implies eqv", Code: `(and (eq? 'foo 'foo) (eqv? 'foo 'foo))`, Expected: values.TrueValue},
		{Name: "symbol eq implies equal", Code: `(and (eq? 'foo 'foo) (equal? 'foo 'foo))`, Expected: values.TrueValue},

		// If eqv? but not eq? (numbers)
		{Name: "numbers eqv but check equal", Code: `(and (eqv? 42 42) (equal? 42 42))`, Expected: values.TrueValue},

		// Since literals are interned, they ARE eqv? (so these tests verify that)
		{Name: "literal pairs are eqv (interned)", Code: `(and (equal? '(1 2) '(1 2)) (eqv? '(1 2) '(1 2)))`, Expected: values.TrueValue},
		{Name: "literal strings are eqv (interned)", Code: `(and (equal? "hello" "hello") (eqv? "hello" "hello"))`, Expected: values.TrueValue},

		// Test with non-interned objects (created at runtime)
		{Name: "runtime pairs equal but not eqv", Code: `(let ((a (list 1 2)) (b (list 1 2))) (and (equal? a b) (not (eqv? a b))))`, Expected: values.TrueValue},
		// Note: Short strings (<=64 chars) are interned, so they ARE eqv? for short content.
		// This tests that equal? works for strings regardless of interning.
		{Name: "strings created at runtime are equal", Code: `(let ((a (string #\h #\i)) (b (string #\h #\i))) (equal? a b))`, Expected: values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// R7RS §6.1: "The equal? procedure must terminate even if its arguments
// are circular data structures."

func TestEqualQCircular_SelfReferentialList(t *testing.T) {
	result, err := testhelpers.RunSchemeCodeWithTimeout(t, `
		(let ((x (list 1 2 3)))
		  (set-cdr! (cdr (cdr x)) x)
		  (equal? x x))
	`, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestEqualQCircular_TwoIdenticalCircularLists(t *testing.T) {
	result, err := testhelpers.RunSchemeCodeWithTimeout(t, `
		(let ((a (list 1 2))
		      (b (list 1 2)))
		  (set-cdr! (cdr a) a)
		  (set-cdr! (cdr b) b)
		  (equal? a b))
	`, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestEqualQCircular_DifferentCircularLists(t *testing.T) {
	result, err := testhelpers.RunSchemeCodeWithTimeout(t, `
		(let ((a (list 1))
		      (b (list 2)))
		  (set-cdr! a a)
		  (set-cdr! b b)
		  (equal? a b))
	`, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.FalseValue)
}

func TestEqualQCircular_CircularVector(t *testing.T) {
	result, err := testhelpers.RunSchemeCodeWithTimeout(t, `
		(let ((v (vector 0)))
		  (vector-set! v 0 v)
		  (equal? v v))
	`, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestEqualQCircular_PairContainingCircularVector(t *testing.T) {
	result, err := testhelpers.RunSchemeCodeWithTimeout(t, `
		(let ((v (vector 0)))
		  (vector-set! v 0 v)
		  (let ((a (list v))
		        (b (list v)))
		    (equal? a b)))
	`, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}
