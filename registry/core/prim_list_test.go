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

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// ============================================================================
// car, cdr, cons
// ============================================================================

func TestCar(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "car of quoted list", Code: `(car '(1 2 3))`, Expected: values.NewInteger(1)},
		{Name: "car of pair", Code: `(car (cons 'a 'b))`, Expected: values.NewSymbol("a")},
		{Name: "car of single element list", Code: `(car '(42))`, Expected: values.NewInteger(42)},
		{Name: "car of nested list", Code: `(car '((1 2) (3 4)))`, Expected: values.List(values.NewInteger(1), values.NewInteger(2))},
		{Name: "car of list with mixed types", Code: `(car '("hello" 1 #t))`, Expected: values.NewString("hello")},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCar_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"car of empty list", `(car '())`},
		{"car of integer", `(car 42)`},
		{"car of string", `(car "hello")`},
		{"car of symbol", `(car 'foo)`},
		{"car of boolean", `(car #t)`},
		{"car of vector", `(car #(1 2 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestCdr(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "cdr of quoted list", Code: `(cdr '(1 2 3))`, Expected: values.List(values.NewInteger(2), values.NewInteger(3))},
		{Name: "cdr of pair", Code: `(cdr (cons 'a 'b))`, Expected: values.NewSymbol("b")},
		{Name: "cdr of single element list", Code: `(cdr '(42))`, Expected: values.EmptyList},
		{Name: "cdr of two element list", Code: `(cdr '(1 2))`, Expected: values.List(values.NewInteger(2))},
		{Name: "cdr of nested list", Code: `(cdr '((1 2) (3 4)))`, Expected: values.List(values.List(values.NewInteger(3), values.NewInteger(4)))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCdr_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"cdr of empty list", `(cdr '())`},
		{"cdr of integer", `(cdr 42)`},
		{"cdr of string", `(cdr "hello")`},
		{"cdr of symbol", `(cdr 'foo)`},
		{"cdr of boolean", `(cdr #t)`},
		{"cdr of vector", `(cdr #(1 2 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestCons(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Building proper lists
		{Name: "cons two values", Code: `(cons 1 2)`, Expected: values.NewCons(values.NewInteger(1), values.NewInteger(2))},
		{Name: "cons with empty list", Code: `(cons 1 '())`, Expected: values.List(values.NewInteger(1))},
		{Name: "cons onto list", Code: `(cons 1 '(2 3))`, Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},

		// Building improper lists
		{Name: "cons symbols", Code: `(cons 'a 'b)`, Expected: values.NewCons(values.NewSymbol("a"), values.NewSymbol("b"))},

		// Nested cons
		{Name: "nested cons", Code: `(cons (cons 1 2) (cons 3 4))`, Expected: values.NewCons(values.NewCons(values.NewInteger(1), values.NewInteger(2)),
			values.NewCons(values.NewInteger(3), values.NewInteger(4)))},

		// Various types
		{Name: "cons string onto list", Code: `(cons "hello" '())`, Expected: values.List(values.NewString("hello"))},
		{Name: "cons list onto list", Code: `(cons '(1 2) '(3 4))`, Expected: values.NewCons(values.List(values.NewInteger(1), values.NewInteger(2)),
			values.List(values.NewInteger(3), values.NewInteger(4)))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestList(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "list with three elements", Code: `(list 1 2 3)`, Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{Name: "list with no elements", Code: `(list)`, Expected: values.EmptyList},
		{Name: "list with one element", Code: `(list 'a)`, Expected: values.List(values.NewSymbol("a"))},
		{Name: "list with mixed types", Code: `(list 1 "two" #t)`, Expected: values.List(values.NewInteger(1), values.NewString("two"), values.TrueValue)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// ============================================================================
// car, cdr, cons — Improper List Handling
// ============================================================================

func TestCar_ImproperList(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "car of dotted pair", Code: `(car '(1 . 2))`, Expected: values.NewInteger(1)},
		{Name: "car of longer improper list", Code: `(car '(a b . c))`, Expected: values.NewSymbol("a")},
		{Name: "car of nested improper pair", Code: `(car '((1 . 2) 3))`, Expected: values.NewCons(values.NewInteger(1), values.NewInteger(2))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCdr_ImproperList(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "cdr of dotted pair", Code: `(cdr '(1 . 2))`, Expected: values.NewInteger(2)},
		{Name: "cdr of longer improper list", Code: `(cdr '(a b . c))`, Expected: values.NewCons(values.NewSymbol("b"), values.NewSymbol("c"))},
		{Name: "cdr of two-element dotted", Code: `(cdr '(x . y))`, Expected: values.NewSymbol("y")},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCons_ImproperList(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "cons onto improper list", Code: `(cons 'a '(b . c))`, Expected: values.NewCons(values.NewSymbol("a"),
			values.NewCons(values.NewSymbol("b"), values.NewSymbol("c")))},
		{Name: "cons number onto dotted pair", Code: `(cons 1 (cons 2 3))`, Expected: values.NewCons(values.NewInteger(1),
			values.NewCons(values.NewInteger(2), values.NewInteger(3)))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// ============================================================================
// set-car!, set-cdr! — Improper List Handling
// ============================================================================

func TestSetCarBang_ImproperList(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "set-car! on dotted pair", Code: `(let ((p (cons 1 2))) (set-car! p 10) p)`, Expected: values.NewCons(values.NewInteger(10), values.NewInteger(2))},
		{Name: "set-car! preserves improper cdr", Code: `(let ((p (cons 1 2))) (set-car! p 'x) (cdr p))`, Expected: values.NewInteger(2)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestSetCdrBang_ImproperList(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "set-cdr! on dotted pair changes tail", Code: `(let ((p (cons 1 2))) (set-cdr! p 3) p)`, Expected: values.NewCons(values.NewInteger(1), values.NewInteger(3))},
		{Name: "set-cdr! converts improper to proper", Code: `(let ((p (cons 1 2))) (set-cdr! p '()) p)`, Expected: values.List(values.NewInteger(1))},
		{Name: "set-cdr! converts improper to different improper", Code: `(let ((p (cons 1 2))) (set-cdr! p (cons 3 4)) p)`, Expected: values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(3), values.NewInteger(4)))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// ============================================================================
// list-ref — Improper List Handling
// ============================================================================

func TestListRef_ImproperList(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "list-ref improper at index 0", Code: `(list-ref '(a b . c) 0)`, Expected: values.NewSymbol("a")},
		{Name: "list-ref improper at index 1", Code: `(list-ref '(a b . c) 1)`, Expected: values.NewSymbol("b")},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestListRef_ImproperListErrors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"list-ref index hits improper tail", `(list-ref '(a b . c) 2)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ============================================================================
// list-tail — Improper List Handling
// ============================================================================

func TestListTail_ImproperList(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "list-tail improper k=0", Code: `(list-tail '(a b . c) 0)`, Expected: values.NewCons(values.NewSymbol("a"),
			values.NewCons(values.NewSymbol("b"), values.NewSymbol("c")))},
		{Name: "list-tail improper k=1", Code: `(list-tail '(a b . c) 1)`, Expected: values.NewCons(values.NewSymbol("b"), values.NewSymbol("c"))},
		{Name: "list-tail improper k=2 returns atom tail", Code: `(list-tail '(a b . c) 2)`, Expected: values.NewSymbol("c")},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestListTail_ImproperListErrors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"list-tail past improper tail", `(list-tail '(a b . c) 3)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ============================================================================
// list-set! — Improper List Handling
// ============================================================================

func TestListSetBang_ImproperList(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "list-set! improper at index 0", Code: `(let ((lst (cons 'a (cons 'b 'c)))) (list-set! lst 0 'x) (car lst))`, Expected: values.NewSymbol("x")},
		{Name: "list-set! improper at index 1", Code: `(let ((lst (cons 'a (cons 'b 'c)))) (list-set! lst 1 'x) (cadr lst))`, Expected: values.NewSymbol("x")},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestListSetBang_ImproperListErrors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"list-set! index hits improper tail", `(list-set! '(a b . c) 2 'x)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ============================================================================
// memq, memv, member — Improper List Handling
// ============================================================================

func TestMemq_ImproperList(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "memq finds element in improper list", Code: `(memq 'b '(a b . c))`, Expected: values.NewCons(values.NewSymbol("b"), values.NewSymbol("c"))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestMemq_ImproperListErrors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"memq not found hits improper tail", `(memq 'z '(a b . c))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestMemv_ImproperList(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "memv finds integer in improper list", Code: `(memv 2 '(1 2 . 3))`, Expected: values.NewCons(values.NewInteger(2), values.NewInteger(3))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestMemv_ImproperListErrors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"memv not found hits improper tail", `(memv 9 '(1 2 . 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestMember_ImproperList(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "member finds element in improper list", Code: `(member "b" '("a" "b" . "c"))`, Expected: values.NewCons(values.NewString("b"), values.NewString("c"))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestMember_ImproperListErrors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"member not found hits improper tail", `(member "z" '("a" "b" . "c"))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ============================================================================
// assq, assv, assoc — Improper Alist Handling
// ============================================================================

func TestAssq_ImproperAlist(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "assq finds entry before improper tail", Code: `(assq 'a '((a 1) (b 2) . c))`, Expected: values.List(values.NewSymbol("a"), values.NewInteger(1))},
		{Name: "assq finds second entry before improper tail", Code: `(assq 'b '((a 1) (b 2) . c))`, Expected: values.List(values.NewSymbol("b"), values.NewInteger(2))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestAssq_ImproperAlistErrors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"assq not found hits improper tail", `(assq 'z '((a 1) (b 2) . c))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestAssv_ImproperAlist(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "assv finds entry before improper tail", Code: `(assv 1 '((1 a) (2 b) . c))`, Expected: values.List(values.NewInteger(1), values.NewSymbol("a"))},
		{Name: "assv finds second entry before improper tail", Code: `(assv 2 '((1 a) (2 b) . c))`, Expected: values.List(values.NewInteger(2), values.NewSymbol("b"))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestAssv_ImproperAlistErrors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"assv not found hits improper tail", `(assv 9 '((1 a) (2 b) . c))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestAssoc_ImproperAlist(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "assoc finds entry before improper tail", Code: `(assoc "a" '(("a" 1) ("b" 2) . c))`, Expected: values.List(values.NewString("a"), values.NewInteger(1))},
		{Name: "assoc finds second entry before improper tail", Code: `(assoc "b" '(("a" 1) ("b" 2) . c))`, Expected: values.List(values.NewString("b"), values.NewInteger(2))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestAssoc_ImproperAlistErrors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"assoc not found hits improper tail", `(assoc "z" '(("a" 1) ("b" 2) . c))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ============================================================================
// memq, memv, member, assq, assv, assoc — Circular List Handling
// ============================================================================

func TestMemq_CircularList(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"first element", `(let ((x (list 'a 'b 'c))) (set-cdr! (cddr x) x) (eq? (car (memq 'a x)) 'a))`},
		{"middle element", `(let ((x (list 'a 'b 'c))) (set-cdr! (cddr x) x) (eq? (car (memq 'b x)) 'b))`},
		{"last before cycle", `(let ((x (list 'a 'b 'c))) (set-cdr! (cddr x) x) (eq? (car (memq 'c x)) 'c))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

func TestMemv_CircularList(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"first element", `(let ((x (list 1 2 3))) (set-cdr! (cddr x) x) (eqv? (car (memv 1 x)) 1))`},
		{"middle element", `(let ((x (list 1 2 3))) (set-cdr! (cddr x) x) (eqv? (car (memv 2 x)) 2))`},
		{"last before cycle", `(let ((x (list 1 2 3))) (set-cdr! (cddr x) x) (eqv? (car (memv 3 x)) 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

func TestMember_CircularList(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"first element", `(let ((x (list "a" "b" "c"))) (set-cdr! (cddr x) x) (equal? (car (member "a" x)) "a"))`},
		{"middle element", `(let ((x (list "a" "b" "c"))) (set-cdr! (cddr x) x) (equal? (car (member "b" x)) "b"))`},
		{"last before cycle", `(let ((x (list "a" "b" "c"))) (set-cdr! (cddr x) x) (equal? (car (member "c" x)) "c"))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

func TestAssq_CircularList(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"first element", `(let ((x (list '(a 1) '(b 2) '(c 3)))) (set-cdr! (cddr x) x) (equal? (assq 'a x) '(a 1)))`},
		{"middle element", `(let ((x (list '(a 1) '(b 2) '(c 3)))) (set-cdr! (cddr x) x) (equal? (assq 'b x) '(b 2)))`},
		{"last before cycle", `(let ((x (list '(a 1) '(b 2) '(c 3)))) (set-cdr! (cddr x) x) (equal? (assq 'c x) '(c 3)))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

func TestAssv_CircularList(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"first element", `(let ((x (list '(1 a) '(2 b) '(3 c)))) (set-cdr! (cddr x) x) (equal? (assv 1 x) '(1 a)))`},
		{"middle element", `(let ((x (list '(1 a) '(2 b) '(3 c)))) (set-cdr! (cddr x) x) (equal? (assv 2 x) '(2 b)))`},
		{"last before cycle", `(let ((x (list '(1 a) '(2 b) '(3 c)))) (set-cdr! (cddr x) x) (equal? (assv 3 x) '(3 c)))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

func TestAssoc_CircularList(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"first element", `(let ((x (list '("a" 1) '("b" 2) '("c" 3)))) (set-cdr! (cddr x) x) (equal? (assoc "a" x) '("a" 1)))`},
		{"middle element", `(let ((x (list '("a" 1) '("b" 2) '("c" 3)))) (set-cdr! (cddr x) x) (equal? (assoc "b" x) '("b" 2)))`},
		{"last before cycle", `(let ((x (list '("a" 1) '("b" 2) '("c" 3)))) (set-cdr! (cddr x) x) (equal? (assoc "c" x) '("c" 3)))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

// ============================================================================
// memq, memv, member — Circular List: Lasso-Shaped
// ============================================================================
// Lasso-shaped: (1 2 3 4) with tail pointing back to second element.
// Shape: 1 -> 2 -> 3 -> 4 -> 2 -> 3 -> 4 -> ...
// Element 1 is in the tail (before cycle), 2/3/4 are in the cycle.

func TestMemq_CircularListLasso(t *testing.T) {
	// Build: (define x '(a b c d)) (set-cdr! (cdddr x) (cdr x))
	// Shape: a -> b -> c -> d -> b -> c -> d -> ...
	tcs := []struct {
		name string
		code string
	}{
		{"tail element before cycle", `(let ((x (list 'a 'b 'c 'd))) (set-cdr! (cdddr x) (cdr x)) (eq? (car (memq 'a x)) 'a))`},
		{"first element in cycle", `(let ((x (list 'a 'b 'c 'd))) (set-cdr! (cdddr x) (cdr x)) (eq? (car (memq 'b x)) 'b))`},
		{"middle element in cycle", `(let ((x (list 'a 'b 'c 'd))) (set-cdr! (cdddr x) (cdr x)) (eq? (car (memq 'c x)) 'c))`},
		{"last element in cycle", `(let ((x (list 'a 'b 'c 'd))) (set-cdr! (cdddr x) (cdr x)) (eq? (car (memq 'd x)) 'd))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

func TestMemv_CircularListLasso(t *testing.T) {
	// Shape: 10 -> 20 -> 30 -> 40 -> 20 -> 30 -> 40 -> ...
	tcs := []struct {
		name string
		code string
	}{
		{"tail element before cycle", `(let ((x (list 10 20 30 40))) (set-cdr! (cdddr x) (cdr x)) (eqv? (car (memv 10 x)) 10))`},
		{"first element in cycle", `(let ((x (list 10 20 30 40))) (set-cdr! (cdddr x) (cdr x)) (eqv? (car (memv 20 x)) 20))`},
		{"middle element in cycle", `(let ((x (list 10 20 30 40))) (set-cdr! (cdddr x) (cdr x)) (eqv? (car (memv 30 x)) 30))`},
		{"last element in cycle", `(let ((x (list 10 20 30 40))) (set-cdr! (cdddr x) (cdr x)) (eqv? (car (memv 40 x)) 40))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

func TestMember_CircularListLasso(t *testing.T) {
	// Shape: "w" -> "x" -> "y" -> "z" -> "x" -> "y" -> "z" -> ...
	tcs := []struct {
		name string
		code string
	}{
		{"tail element before cycle", `(let ((x (list "w" "x" "y" "z"))) (set-cdr! (cdddr x) (cdr x)) (equal? (car (member "w" x)) "w"))`},
		{"first element in cycle", `(let ((x (list "w" "x" "y" "z"))) (set-cdr! (cdddr x) (cdr x)) (equal? (car (member "x" x)) "x"))`},
		{"middle element in cycle", `(let ((x (list "w" "x" "y" "z"))) (set-cdr! (cdddr x) (cdr x)) (equal? (car (member "y" x)) "y"))`},
		{"last element in cycle", `(let ((x (list "w" "x" "y" "z"))) (set-cdr! (cdddr x) (cdr x)) (equal? (car (member "z" x)) "z"))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

// ============================================================================
// memq, memv, member — Circular List: Single & Two-Element Cycles
// ============================================================================

func TestMemq_CircularListSmall(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"single-element cycle", `(let ((x (list 'a))) (set-cdr! x x) (eq? (car (memq 'a x)) 'a))`},
		{"two-element cycle first", `(let ((x (list 'a 'b))) (set-cdr! (cdr x) x) (eq? (car (memq 'a x)) 'a))`},
		{"two-element cycle second", `(let ((x (list 'a 'b))) (set-cdr! (cdr x) x) (eq? (car (memq 'b x)) 'b))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

func TestMemv_CircularListSmall(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"single-element cycle", `(let ((x (list 1))) (set-cdr! x x) (eqv? (car (memv 1 x)) 1))`},
		{"two-element cycle first", `(let ((x (list 1 2))) (set-cdr! (cdr x) x) (eqv? (car (memv 1 x)) 1))`},
		{"two-element cycle second", `(let ((x (list 1 2))) (set-cdr! (cdr x) x) (eqv? (car (memv 2 x)) 2))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

func TestMember_CircularListSmall(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"single-element cycle", `(let ((x (list "a"))) (set-cdr! x x) (equal? (car (member "a" x)) "a"))`},
		{"two-element cycle first", `(let ((x (list "a" "b"))) (set-cdr! (cdr x) x) (equal? (car (member "a" x)) "a"))`},
		{"two-element cycle second", `(let ((x (list "a" "b"))) (set-cdr! (cdr x) x) (equal? (car (member "b" x)) "b"))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

// ============================================================================
// assq, assv, assoc — Circular Alist: Lasso-Shaped
// ============================================================================
// Lasso-shaped alist: ((a 1) (b 2) (c 3) (d 4)) with tail -> second element.

func TestAssq_CircularAlistLasso(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"tail entry before cycle", `(let ((x (list '(a 1) '(b 2) '(c 3) '(d 4)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assq 'a x) '(a 1)))`},
		{"first entry in cycle", `(let ((x (list '(a 1) '(b 2) '(c 3) '(d 4)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assq 'b x) '(b 2)))`},
		{"middle entry in cycle", `(let ((x (list '(a 1) '(b 2) '(c 3) '(d 4)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assq 'c x) '(c 3)))`},
		{"last entry in cycle", `(let ((x (list '(a 1) '(b 2) '(c 3) '(d 4)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assq 'd x) '(d 4)))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

func TestAssv_CircularAlistLasso(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"tail entry before cycle", `(let ((x (list '(1 a) '(2 b) '(3 c) '(4 d)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assv 1 x) '(1 a)))`},
		{"first entry in cycle", `(let ((x (list '(1 a) '(2 b) '(3 c) '(4 d)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assv 2 x) '(2 b)))`},
		{"middle entry in cycle", `(let ((x (list '(1 a) '(2 b) '(3 c) '(4 d)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assv 3 x) '(3 c)))`},
		{"last entry in cycle", `(let ((x (list '(1 a) '(2 b) '(3 c) '(4 d)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assv 4 x) '(4 d)))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

func TestAssoc_CircularAlistLasso(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"tail entry before cycle", `(let ((x (list '("a" 1) '("b" 2) '("c" 3) '("d" 4)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assoc "a" x) '("a" 1)))`},
		{"first entry in cycle", `(let ((x (list '("a" 1) '("b" 2) '("c" 3) '("d" 4)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assoc "b" x) '("b" 2)))`},
		{"middle entry in cycle", `(let ((x (list '("a" 1) '("b" 2) '("c" 3) '("d" 4)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assoc "c" x) '("c" 3)))`},
		{"last entry in cycle", `(let ((x (list '("a" 1) '("b" 2) '("c" 3) '("d" 4)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assoc "d" x) '("d" 4)))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

// ============================================================================
// assq, assv, assoc — Circular Alist: Single & Two-Element Cycles
// ============================================================================

func TestAssq_CircularAlistSmall(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"single-entry cycle", `(let ((x (list '(a 1)))) (set-cdr! x x) (equal? (assq 'a x) '(a 1)))`},
		{"two-entry cycle first", `(let ((x (list '(a 1) '(b 2)))) (set-cdr! (cdr x) x) (equal? (assq 'a x) '(a 1)))`},
		{"two-entry cycle second", `(let ((x (list '(a 1) '(b 2)))) (set-cdr! (cdr x) x) (equal? (assq 'b x) '(b 2)))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

func TestAssv_CircularAlistSmall(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"single-entry cycle", `(let ((x (list '(1 a)))) (set-cdr! x x) (equal? (assv 1 x) '(1 a)))`},
		{"two-entry cycle first", `(let ((x (list '(1 a) '(2 b)))) (set-cdr! (cdr x) x) (equal? (assv 1 x) '(1 a)))`},
		{"two-entry cycle second", `(let ((x (list '(1 a) '(2 b)))) (set-cdr! (cdr x) x) (equal? (assv 2 x) '(2 b)))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

func TestAssoc_CircularAlistSmall(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"single-entry cycle", `(let ((x (list '("a" 1)))) (set-cdr! x x) (equal? (assoc "a" x) '("a" 1)))`},
		{"two-entry cycle first", `(let ((x (list '("a" 1) '("b" 2)))) (set-cdr! (cdr x) x) (equal? (assoc "a" x) '("a" 1)))`},
		{"two-entry cycle second", `(let ((x (list '("a" 1) '("b" 2)))) (set-cdr! (cdr x) x) (equal? (assoc "b" x) '("b" 2)))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

// ============================================================================
// member, assoc — Circular List with Custom Compare
// ============================================================================

func TestMember_CircularListCustomCompare(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{
			"custom compare in full cycle",
			// Circular list (10 20 30 10 20 30 ...), find with custom comparator
			`(let ((x (list 10 20 30)))
				(set-cdr! (cddr x) x)
				(eqv? (car (member 25 x (lambda (a b) (> b 15)))) 20))`,
		},
		{
			"custom compare in lasso cycle",
			// Lasso: 1 -> 2 -> 3 -> 4 -> 2 -> ..., find element in cycle with custom comparator
			`(let ((x (list 1 2 3 4)))
				(set-cdr! (cdddr x) (cdr x))
				(eqv? (car (member 0 x (lambda (a b) (= b 4)))) 4))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

func TestAssoc_CircularAlistCustomCompare(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{
			"custom compare in full cycle",
			`(let ((x (list '(1 a) '(2 b) '(3 c))))
				(set-cdr! (cddr x) x)
				(equal? (assoc 0 x (lambda (a b) (= b 3))) '(3 c)))`,
		},
		{
			"custom compare in lasso cycle",
			`(let ((x (list '(1 a) '(2 b) '(3 c) '(4 d))))
				(set-cdr! (cdddr x) (cdr x))
				(equal? (assoc 0 x (lambda (a b) (= b 4))) '(4 d)))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
		})
	}
}

// ============================================================================
// append — Improper Non-Last Argument
// ============================================================================

func TestAppend_ImproperListErrors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"append improper non-last arg", `(append '(1 . 2) '(3))`},
		{"append improper first of three", `(append '(a . b) '(c) '(d))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ============================================================================
// CxR — Improper List Handling
// ============================================================================

func TestCxR_ImproperList(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "cadr of improper list", Code: `(cadr '(a b . c))`, Expected: values.NewSymbol("b")},
		{Name: "cddr of improper list returns atom", Code: `(cddr '(a b . c))`, Expected: values.NewSymbol("c")},
		{Name: "cdar of nested improper", Code: `(cdar '((1 . 2) 3))`, Expected: values.NewInteger(2)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCxR_ImproperListErrors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"caddr of short improper list", `(caddr '(a b . c))`},
		{"caaar of non-nested improper", `(caaar '(1 . 2))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ============================================================================
// List Predicates: null?, pair?, list?
// ============================================================================

func TestNullQ(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// True cases - only empty list returns #t
		{Name: "null? of empty list", Code: `(null? '())`, Expected: values.TrueValue},

		// False cases - everything else returns #f
		{Name: "null? of non-empty list", Code: `(null? '(1 2 3))`, Expected: values.FalseValue},
		{Name: "null? of single element list", Code: `(null? '(1))`, Expected: values.FalseValue},
		{Name: "null? of pair", Code: `(null? (cons 1 2))`, Expected: values.FalseValue},
		{Name: "null? of integer", Code: `(null? 42)`, Expected: values.FalseValue},
		{Name: "null? of string", Code: `(null? "hello")`, Expected: values.FalseValue},
		{Name: "null? of symbol", Code: `(null? 'foo)`, Expected: values.FalseValue},
		{Name: "null? of boolean true", Code: `(null? #t)`, Expected: values.FalseValue},
		{Name: "null? of boolean false", Code: `(null? #f)`, Expected: values.FalseValue},
		{Name: "null? of vector", Code: `(null? #(1 2 3))`, Expected: values.FalseValue},
		{Name: "null? of character", Code: `(null? #\a)`, Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestPairQ(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// True cases - pairs and non-empty lists
		{Name: "pair? of cons cell", Code: `(pair? (cons 1 2))`, Expected: values.TrueValue},
		{Name: "pair? of non-empty list", Code: `(pair? '(1 2 3))`, Expected: values.TrueValue},
		{Name: "pair? of single element list", Code: `(pair? '(1))`, Expected: values.TrueValue},
		{Name: "pair? of nested list", Code: `(pair? '((1 2) (3 4)))`, Expected: values.TrueValue},
		{Name: "pair? of improper list", Code: `(pair? '(1 2 . 3))`, Expected: values.TrueValue},

		// False cases - empty list is NOT a pair
		{Name: "pair? of empty list", Code: `(pair? '())`, Expected: values.FalseValue},
		{Name: "pair? of integer", Code: `(pair? 42)`, Expected: values.FalseValue},
		{Name: "pair? of string", Code: `(pair? "hello")`, Expected: values.FalseValue},
		{Name: "pair? of symbol", Code: `(pair? 'foo)`, Expected: values.FalseValue},
		{Name: "pair? of boolean", Code: `(pair? #t)`, Expected: values.FalseValue},
		{Name: "pair? of vector", Code: `(pair? #(1 2 3))`, Expected: values.FalseValue},
		{Name: "pair? of character", Code: `(pair? #\a)`, Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestListQ(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// True cases - proper lists
		{Name: "list? of empty list", Code: `(list? '())`, Expected: values.TrueValue},
		{Name: "list? of single element list", Code: `(list? '(1))`, Expected: values.TrueValue},
		{Name: "list? of multiple element list", Code: `(list? '(1 2 3))`, Expected: values.TrueValue},
		{Name: "list? of nested list", Code: `(list? '((1 2) (3 4)))`, Expected: values.TrueValue},
		{Name: "list? of list with mixed types", Code: `(list? '(1 "two" #t))`, Expected: values.TrueValue},

		// False cases - improper lists and non-lists
		{Name: "list? of improper list", Code: `(list? (cons 1 2))`, Expected: values.FalseValue},
		{Name: "list? of dotted list", Code: `(list? '(1 2 . 3))`, Expected: values.FalseValue},
		{Name: "list? of integer", Code: `(list? 42)`, Expected: values.FalseValue},
		{Name: "list? of string", Code: `(list? "hello")`, Expected: values.FalseValue},
		{Name: "list? of symbol", Code: `(list? 'foo)`, Expected: values.FalseValue},
		{Name: "list? of boolean", Code: `(list? #t)`, Expected: values.FalseValue},
		{Name: "list? of vector", Code: `(list? #(1 2 3))`, Expected: values.FalseValue},
		{Name: "list? of character", Code: `(list? #\a)`, Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// ============================================================================
// Mutation: set-car!, set-cdr!, list-set!
// ============================================================================

func TestSetCarBang(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "set-car! changes first element", Code: `(let ((p (cons 1 2))) (set-car! p 10) (car p))`, Expected: values.NewInteger(10)},
		{Name: "set-car! on list", Code: `(let ((lst (list 1 2 3))) (set-car! lst 10) lst)`, Expected: values.List(values.NewInteger(10), values.NewInteger(2), values.NewInteger(3))},
		{Name: "set-car! with different type", Code: `(let ((p (cons 1 2))) (set-car! p "hello") (car p))`, Expected: values.NewString("hello")},
		{Name: "set-car! preserves cdr", Code: `(let ((p (cons 1 2))) (set-car! p 10) (cdr p))`, Expected: values.NewInteger(2)},
		{Name: "set-car! on nested list", Code: `(let ((lst '((1 2) (3 4)))) (set-car! lst '(10 20)) (car lst))`, Expected: values.List(values.NewInteger(10), values.NewInteger(20))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestSetCarBang_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"set-car! on empty list", `(set-car! '() 1)`},
		{"set-car! on non-pair", `(set-car! 42 1)`},
		{"set-car! on string", `(set-car! "hello" 1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestSetCdrBang(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "set-cdr! changes cdr", Code: `(let ((p (cons 1 2))) (set-cdr! p 20) (cdr p))`, Expected: values.NewInteger(20)},
		{Name: "set-cdr! on list shortens it", Code: `(let ((lst (list 1 2 3))) (set-cdr! lst '()) lst)`, Expected: values.List(values.NewInteger(1))},
		{Name: "set-cdr! extends list", Code: `(let ((lst (list 1))) (set-cdr! lst '(2 3)) lst)`, Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{Name: "set-cdr! preserves car", Code: `(let ((p (cons 1 2))) (set-cdr! p 20) (car p))`, Expected: values.NewInteger(1)},
		{Name: "set-cdr! creates improper list", Code: `(let ((lst (list 1 2))) (set-cdr! (cdr lst) 3) lst)`, Expected: values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.NewInteger(3)))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestSetCdrBang_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"set-cdr! on empty list", `(set-cdr! '() 1)`},
		{"set-cdr! on non-pair", `(set-cdr! 42 1)`},
		{"set-cdr! on string", `(set-cdr! "hello" 1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestListSetBang(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "list-set! at index 0", Code: `(let ((lst (list 1 2 3))) (list-set! lst 0 10) lst)`, Expected: values.List(values.NewInteger(10), values.NewInteger(2), values.NewInteger(3))},
		{Name: "list-set! at index 1", Code: `(let ((lst (list 1 2 3))) (list-set! lst 1 20) lst)`, Expected: values.List(values.NewInteger(1), values.NewInteger(20), values.NewInteger(3))},
		{Name: "list-set! at index 2", Code: `(let ((lst (list 1 2 3))) (list-set! lst 2 30) lst)`, Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(30))},
		{Name: "list-set! with string value", Code: `(let ((lst (list 1 2 3))) (list-set! lst 1 "hello") lst)`, Expected: values.List(values.NewInteger(1), values.NewString("hello"), values.NewInteger(3))},
		{Name: "list-set! with list value", Code: `(let ((lst (list 1 2 3))) (list-set! lst 0 '(a b)) (car lst))`, Expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestListSetBang_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"list-set! out of bounds positive", `(list-set! (list 1 2 3) 5 10)`},
		{"list-set! negative index", `(list-set! (list 1 2 3) -1 10)`},
		{"list-set! on empty list", `(list-set! '() 0 10)`},
		{"list-set! on non-list", `(list-set! 42 0 10)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ============================================================================
// List Construction: make-list, append
// ============================================================================

func TestMakeList(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "make-list with fill", Code: `(make-list 3 'a)`, Expected: values.List(values.NewSymbol("a"), values.NewSymbol("a"), values.NewSymbol("a"))},
		{Name: "make-list with integer fill", Code: `(make-list 4 0)`, Expected: values.List(values.NewInteger(0), values.NewInteger(0), values.NewInteger(0), values.NewInteger(0))},
		{Name: "make-list single element", Code: `(make-list 1 'x)`, Expected: values.List(values.NewSymbol("x"))},
		{Name: "make-list zero length", Code: `(make-list 0 'a)`, Expected: values.EmptyList},
		{Name: "make-list without fill", Code: `(length (make-list 5))`, Expected: values.NewInteger(5)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestMakeList_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"make-list negative length", `(make-list -1 'a)`},
		{"make-list non-integer length", `(make-list "three" 'a)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestAppend(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "append no arguments", Code: `(append)`, Expected: values.EmptyList},
		{Name: "append single empty list", Code: `(append '())`, Expected: values.EmptyList},
		{Name: "append single list", Code: `(append '(1 2))`, Expected: values.List(values.NewInteger(1), values.NewInteger(2))},
		{Name: "append two lists", Code: `(append '(1 2) '(3 4))`, Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4))},
		{Name: "append three lists", Code: `(append '(a) '(b) '(c))`, Expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))},
		{Name: "append with empty list in middle", Code: `(append '(1) '() '(2))`, Expected: values.List(values.NewInteger(1), values.NewInteger(2))},
		{Name: "append with non-list as last argument", Code: `(append '(1 2) 3)`, Expected: values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.NewInteger(3)))},
		{Name: "append empty lists only", Code: `(append '() '())`, Expected: values.EmptyList},
		{Name: "append nested lists", Code: `(append '((1 2)) '((3 4)))`, Expected: values.List(values.List(values.NewInteger(1), values.NewInteger(2)),
			values.List(values.NewInteger(3), values.NewInteger(4)))},
		{Name: "append four lists", Code: `(append '(1) '(2) '(3) '(4))`, Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4))},
		{Name: "append lists with different types", Code: `(append '(1 2) '("a" "b") '(#t #f))`, Expected: values.List(values.NewInteger(1), values.NewInteger(2),
			values.NewString("a"), values.NewString("b"),
			values.TrueValue, values.FalseValue)},
		{Name: "append single element to list", Code: `(append '(a b) 'c)`, Expected: values.NewCons(values.NewSymbol("a"),
			values.NewCons(values.NewSymbol("b"), values.NewSymbol("c")))},
		{Name: "append all empty", Code: `(append '() '() '())`, Expected: values.EmptyList},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// ============================================================================
// List Access: length, reverse, list-ref, list-tail
// ============================================================================

func TestLength(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "length of three element list", Code: `(length '(1 2 3))`, Expected: values.NewInteger(3)},
		{Name: "length of empty list", Code: `(length '())`, Expected: values.NewInteger(0)},
		{Name: "length of single element list", Code: `(length '(a))`, Expected: values.NewInteger(1)},
		{Name: "length of two element list", Code: `(length '(a b))`, Expected: values.NewInteger(2)},
		{Name: "length of five element list", Code: `(length '(1 2 3 4 5))`, Expected: values.NewInteger(5)},
		{Name: "length of nested list", Code: `(length '((1 2) (3 4) (5 6)))`, Expected: values.NewInteger(3)},
		{Name: "length of list with mixed types", Code: `(length '(1 "two" #t 'four))`, Expected: values.NewInteger(4)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestLength_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"length of improper list", `(length (cons 1 2))`},
		{"length of integer", `(length 42)`},
		{"length of string", `(length "hello")`},
		{"length of symbol", `(length 'foo)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestReverse(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "reverse list", Code: `(reverse '(1 2 3))`, Expected: values.List(values.NewInteger(3), values.NewInteger(2), values.NewInteger(1))},
		{Name: "reverse empty list", Code: `(reverse '())`, Expected: values.EmptyList},
		{Name: "reverse single element", Code: `(reverse '(a))`, Expected: values.List(values.NewSymbol("a"))},
		{Name: "reverse two elements", Code: `(reverse '(a b))`, Expected: values.List(values.NewSymbol("b"), values.NewSymbol("a"))},
		{Name: "reverse five elements", Code: `(reverse '(1 2 3 4 5))`, Expected: values.List(values.NewInteger(5), values.NewInteger(4), values.NewInteger(3), values.NewInteger(2), values.NewInteger(1))},
		{Name: "reverse nested lists", Code: `(reverse '((1 2) (3 4)))`, Expected: values.List(values.List(values.NewInteger(3), values.NewInteger(4)),
			values.List(values.NewInteger(1), values.NewInteger(2)))},
		{Name: "reverse preserves nested structure", Code: `(car (reverse '((1 2) (3 4))))`, Expected: values.List(values.NewInteger(3), values.NewInteger(4))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestReverse_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"reverse of improper list", `(reverse (cons 1 2))`},
		{"reverse of integer", `(reverse 42)`},
		{"reverse of string", `(reverse "hello")`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestListRef(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "list-ref first element", Code: `(list-ref '(a b c) 0)`, Expected: values.NewSymbol("a")},
		{Name: "list-ref middle element", Code: `(list-ref '(a b c) 1)`, Expected: values.NewSymbol("b")},
		{Name: "list-ref last element", Code: `(list-ref '(a b c) 2)`, Expected: values.NewSymbol("c")},
		{Name: "list-ref nested element", Code: `(list-ref '((a) (b) (c)) 1)`, Expected: values.List(values.NewSymbol("b"))},
		{Name: "list-ref in long list", Code: `(list-ref '(1 2 3 4 5 6 7 8 9 10) 9)`, Expected: values.NewInteger(10)},
		{Name: "list-ref first of many", Code: `(list-ref '(a b c d e) 0)`, Expected: values.NewSymbol("a")},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestListRef_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"list-ref on empty list", `(list-ref '() 0)`},
		{"list-ref out of bounds", `(list-ref '(a b c) 5)`},
		{"list-ref negative index", `(list-ref '(a b c) -1)`},
		{"list-ref on non-list", `(list-ref 42 0)`},
		{"list-ref with non-integer index", `(list-ref '(a b c) "one")`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestListTail(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "list-tail from beginning", Code: `(list-tail '(a b c) 0)`, Expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))},
		{Name: "list-tail skip one", Code: `(list-tail '(a b c) 1)`, Expected: values.List(values.NewSymbol("b"), values.NewSymbol("c"))},
		{Name: "list-tail skip all", Code: `(list-tail '(a b c) 3)`, Expected: values.EmptyList},
		{Name: "list-tail skip two", Code: `(list-tail '(a b c d e) 2)`, Expected: values.List(values.NewSymbol("c"), values.NewSymbol("d"), values.NewSymbol("e"))},
		{Name: "list-tail skip none", Code: `(list-tail '(1 2) 0)`, Expected: values.List(values.NewInteger(1), values.NewInteger(2))},
		{Name: "list-tail on empty list with k=0", Code: `(list-tail '() 0)`, Expected: values.EmptyList},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestListTail_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"list-tail k too large", `(list-tail '(a b c) 5)`},
		{"list-tail negative k", `(list-tail '(a b c) -1)`},
		{"list-tail with non-integer k", `(list-tail '(a b c) "two")`},
		{"list-tail on non-list with k>0", `(list-tail 42 1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ============================================================================
// Search: memq, memv, member
// ============================================================================

func TestMemq(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// memq uses eq? (pointer equality) - works for booleans (singletons) and symbols (interned)
		{Name: "memq finds boolean", Code: `(memq #t '(#f #t 1))`, Expected: values.List(values.TrueValue, values.NewInteger(1))},
		{Name: "memq returns #f when not found", Code: `(memq #t '(#f 1 2))`, Expected: values.FalseValue},
		{Name: "memq with empty list returns #f", Code: `(memq #t '())`, Expected: values.FalseValue},
		{Name: "memq finds symbol", Code: `(memq 'b '(a b c))`, Expected: values.List(values.NewSymbol("b"), values.NewSymbol("c"))},
		{Name: "memq symbol not found", Code: `(memq 'd '(a b c))`, Expected: values.FalseValue},
		{Name: "memq finds first occurrence", Code: `(memq #t '(#f #f #t #t))`, Expected: values.List(values.TrueValue, values.TrueValue)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestMemv(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// memv uses eqv? - compares numbers and characters by value
		{Name: "memv finds integer", Code: `(memv 2 '(1 2 3))`, Expected: values.List(values.NewInteger(2), values.NewInteger(3))},
		{Name: "memv returns #f when not found", Code: `(memv 4 '(1 2 3))`, Expected: values.FalseValue},
		{Name: "memv finds integer in list", Code: `(memv 3 '(1 2 3 4 5))`, Expected: values.List(values.NewInteger(3), values.NewInteger(4), values.NewInteger(5))},
		{Name: "memv integer not found", Code: `(memv 10 '(1 2 3))`, Expected: values.FalseValue},
		{Name: "memv finds character", Code: `(memv #\b '(#\a #\b #\c))`, Expected: values.List(values.NewCharacter('b'), values.NewCharacter('c'))},
		{Name: "memv finds symbol", Code: `(memv 'x '(a b x y z))`, Expected: values.List(values.NewSymbol("x"), values.NewSymbol("y"), values.NewSymbol("z"))},
		{Name: "memv finds first element", Code: `(memv 1 '(1 2 3))`, Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{Name: "memv finds last element", Code: `(memv 3 '(1 2 3))`, Expected: values.List(values.NewInteger(3))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestMember(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// member uses equal? - deep comparison
		{Name: "member finds list with equal?", Code: `(member '(2) '((1) (2) (3)))`, Expected: values.List(values.List(values.NewInteger(2)), values.List(values.NewInteger(3)))},
		{Name: "member finds string", Code: `(member "hello" '("world" "hello" "foo"))`, Expected: values.List(values.NewString("hello"), values.NewString("foo"))},
		{Name: "member returns #f when not found", Code: `(member '(4) '((1) (2) (3)))`, Expected: values.FalseValue},
		{Name: "member string not found", Code: `(member "bar" '("foo" "baz"))`, Expected: values.FalseValue},
		{Name: "member finds nested list", Code: `(member '(2 3) '((1 2) (2 3) (3 4)))`, Expected: values.List(values.List(values.NewInteger(2), values.NewInteger(3)),
			values.List(values.NewInteger(3), values.NewInteger(4)))},
		{Name: "member finds integer", Code: `(member 42 '(1 42 100))`, Expected: values.List(values.NewInteger(42), values.NewInteger(100))},
		{Name: "member in empty list", Code: `(member 'x '())`, Expected: values.FalseValue},
		{Name: "member finds character", Code: `(member #\b '(#\a #\b #\c))`, Expected: values.List(values.NewCharacter('b'), values.NewCharacter('c'))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestMemberWithCompare tests R7RS §6.4 optional compare procedure for member.
func TestMemberWithCompare(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Custom compare using = for numeric comparison
		{Name: "member with = finds exact number", Code: `(member 2.0 '(1 2 3) =)`, Expected: values.List(values.NewInteger(2), values.NewInteger(3))},
		{Name: "member with = not found", Code: `(member 5 '(1 2 3) =)`, Expected: values.FalseValue},

		// Custom compare using string=?
		{Name: "member with string=?", Code: `(member "B" '("a" "B" "c") string=?)`, Expected: values.List(values.NewString("B"), values.NewString("c"))},

		// Custom compare using string-ci=? for case-insensitive
		{Name: "member with string-ci=?", Code: `(member "b" '("A" "B" "C") string-ci=?)`, Expected: values.List(values.NewString("B"), values.NewString("C"))},
		{Name: "member with string-ci=? not found", Code: `(member "d" '("A" "B" "C") string-ci=?)`, Expected: values.FalseValue},

		// Custom compare with lambda - find number greater than obj
		{Name: "member with custom lambda", Code: `(member 2 '(1 2 3 4) (lambda (obj elem) (> elem obj)))`, Expected: values.List(values.NewInteger(3), values.NewInteger(4))},

		// Custom compare - always false returns #f
		{Name: "member with always-false compare", Code: `(member 'x '(a b c) (lambda (a b) #f))`, Expected: values.FalseValue},

		// Custom compare - always true returns first element
		{Name: "member with always-true compare", Code: `(member 'x '(a b c) (lambda (a b) #t))`, Expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))},

		// Empty list with custom compare
		{Name: "member with compare in empty list", Code: `(member 1 '() =)`, Expected: values.FalseValue},

		// Compare using eq? explicitly
		{Name: "member with eq?", Code: `(member 'b '(a b c) eq?)`, Expected: values.List(values.NewSymbol("b"), values.NewSymbol("c"))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// ============================================================================
// Association Lists: assq, assv, assoc
// ============================================================================

func TestAssq(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// assq uses eq? - works for booleans (singletons) and symbols (interned)
		{Name: "assq finds boolean key", Code: `(assq #t '((#f 1) (#t 2)))`, Expected: values.List(values.TrueValue, values.NewInteger(2))},
		{Name: "assq returns #f when not found", Code: `(assq #t '((#f 1)))`, Expected: values.FalseValue},
		{Name: "assq with empty list returns #f", Code: `(assq #t '())`, Expected: values.FalseValue},
		{Name: "assq finds symbol key", Code: `(assq 'b '((a 1) (b 2) (c 3)))`, Expected: values.List(values.NewSymbol("b"), values.NewInteger(2))},
		{Name: "assq symbol not found", Code: `(assq 'd '((a 1) (b 2) (c 3)))`, Expected: values.FalseValue},
		{Name: "assq finds #f key", Code: `(assq #f '((#t yes) (#f no)))`, Expected: values.List(values.FalseValue, values.NewSymbol("no"))},
		{Name: "assq returns first match", Code: `(assq 'a '((a 1) (a 2) (a 3)))`, Expected: values.List(values.NewSymbol("a"), values.NewInteger(1))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestAssv(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// assv uses eqv? - compares numbers and characters by value
		{Name: "assv finds integer key", Code: `(assv 2 '((1 a) (2 b) (3 c)))`, Expected: values.List(values.NewInteger(2), values.NewSymbol("b"))},
		{Name: "assv returns #f when not found", Code: `(assv 4 '((1 a) (2 b) (3 c)))`, Expected: values.FalseValue},
		{Name: "assv integer not found", Code: `(assv 5 '((1 one) (2 two)))`, Expected: values.FalseValue},
		{Name: "assv finds character key", Code: `(assv #\b '((#\a alpha) (#\b beta) (#\c gamma)))`, Expected: values.List(values.NewCharacter('b'), values.NewSymbol("beta"))},
		{Name: "assv multiple entries", Code: `(assv 0 '((0 zero) (1 one) (0 another-zero)))`, Expected: values.List(values.NewInteger(0), values.NewSymbol("zero"))},
		{Name: "assv finds symbol key", Code: `(assv 'x '((y why) (x ecks) (z zee)))`, Expected: values.List(values.NewSymbol("x"), values.NewSymbol("ecks"))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestAssoc(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// assoc uses equal? - deep comparison
		{Name: "assoc finds list key with equal?", Code: `(assoc '(1 2) '(((1 2) found) ((3 4) other)))`, Expected: values.List(values.List(values.NewInteger(1), values.NewInteger(2)), values.NewSymbol("found"))},
		{Name: "assoc returns #f when not found", Code: `(assoc '(5 6) '(((1 2) a) ((3 4) b)))`, Expected: values.FalseValue},
		{Name: "assoc with string key", Code: `(assoc "hello" '(("hello" found) ("world" other)))`, Expected: values.List(values.NewString("hello"), values.NewSymbol("found"))},
		{Name: "assoc finds string key", Code: `(assoc "hello" '(("world" 1) ("hello" 2)))`, Expected: values.List(values.NewString("hello"), values.NewInteger(2))},
		{Name: "assoc list not found", Code: `(assoc '(5 6) '(((1 2) a) ((3 4) b)))`, Expected: values.FalseValue},
		{Name: "assoc in empty alist", Code: `(assoc 'x '())`, Expected: values.FalseValue},
		{Name: "assoc finds integer key", Code: `(assoc 42 '((1 a) (42 b) (100 c)))`, Expected: values.List(values.NewInteger(42), values.NewSymbol("b"))},
		{Name: "assoc finds character key", Code: `(assoc #\y '((#\a alpha) (#\y why)))`, Expected: values.List(values.NewCharacter('y'), values.NewSymbol("why"))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestAssocWithCompare tests R7RS §6.4 optional compare procedure for assoc.
func TestAssocWithCompare(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Custom compare using = for numeric comparison
		{Name: "assoc with = finds exact number", Code: `(assoc 2.0 '((1 one) (2 two) (3 three)) =)`, Expected: values.List(values.NewInteger(2), values.NewSymbol("two"))},
		{Name: "assoc with = not found", Code: `(assoc 5 '((1 one) (2 two) (3 three)) =)`, Expected: values.FalseValue},

		// Custom compare using string=?
		{Name: "assoc with string=?", Code: `(assoc "B" '(("a" alpha) ("B" beta) ("c" gamma)) string=?)`, Expected: values.List(values.NewString("B"), values.NewSymbol("beta"))},

		// Custom compare using string-ci=? for case-insensitive
		{Name: "assoc with string-ci=?", Code: `(assoc "b" '(("A" alpha) ("B" beta) ("C" gamma)) string-ci=?)`, Expected: values.List(values.NewString("B"), values.NewSymbol("beta"))},
		{Name: "assoc with string-ci=? not found", Code: `(assoc "d" '(("A" alpha) ("B" beta)) string-ci=?)`, Expected: values.FalseValue},

		// Custom compare with lambda - find key where car > obj
		{Name: "assoc with custom lambda", Code: `(assoc 2 '((1 one) (2 two) (3 three) (4 four)) (lambda (obj key) (> key obj)))`, Expected: values.List(values.NewInteger(3), values.NewSymbol("three"))},

		// Custom compare - always false returns #f
		{Name: "assoc with always-false compare", Code: `(assoc 'x '((a 1) (b 2) (c 3)) (lambda (a b) #f))`, Expected: values.FalseValue},

		// Custom compare - always true returns first entry
		{Name: "assoc with always-true compare", Code: `(assoc 'x '((a 1) (b 2) (c 3)) (lambda (a b) #t))`, Expected: values.List(values.NewSymbol("a"), values.NewInteger(1))},

		// Empty alist with custom compare
		{Name: "assoc with compare in empty alist", Code: `(assoc 1 '() =)`, Expected: values.FalseValue},

		// Compare using eq? explicitly
		{Name: "assoc with eq?", Code: `(assoc 'b '((a 1) (b 2) (c 3)) eq?)`, Expected: values.List(values.NewSymbol("b"), values.NewInteger(2))},

		// More complex custom comparison - check if key starts with same letter
		{Name: "assoc with char comparison", Code: `(assoc #\b '((#\a alpha) (#\b beta) (#\c gamma)) char=?)`, Expected: values.List(values.NewCharacter('b'), values.NewSymbol("beta"))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// ============================================================================
// list-copy
// ============================================================================

func TestListCopy(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic cases
		{Name: "list-copy empty list", Code: `(list-copy '())`, Expected: values.EmptyList},
		{Name: "list-copy single element", Code: `(list-copy '(1))`, Expected: values.List(values.NewInteger(1))},
		{Name: "list-copy multi element", Code: `(list-copy '(1 2 3))`, Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{Name: "list-copy nested lists", Code: `(list-copy '((1 2) (3 4)))`, Expected: values.List(values.List(values.NewInteger(1), values.NewInteger(2)),
			values.List(values.NewInteger(3), values.NewInteger(4)))},

		// Improper list
		{Name: "list-copy improper list", Code: `(list-copy (cons 1 2))`, Expected: values.NewCons(values.NewInteger(1), values.NewInteger(2))},
		{Name: "list-copy longer improper list", Code: `(list-copy '(1 2 . 3))`, Expected: values.NewCons(values.NewInteger(1),
			values.NewCons(values.NewInteger(2), values.NewInteger(3)))},

		// Non-pair returns as-is per R7RS
		{Name: "list-copy integer", Code: `(list-copy 42)`, Expected: values.NewInteger(42)},
		{Name: "list-copy string", Code: `(list-copy "hello")`, Expected: values.NewString("hello")},
		{Name: "list-copy symbol", Code: `(list-copy 'foo)`, Expected: values.NewSymbol("foo")},
		{Name: "list-copy boolean", Code: `(list-copy #t)`, Expected: values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestListCopy_SpineIndependence(t *testing.T) {
	// Verify that mutating the copy's spine doesn't affect the original
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "spine independence via set-cdr!", Code: `
			(let ((orig (list 1 2 3)))
			  (let ((copy (list-copy orig)))
			    (set-cdr! copy '(99))
			    (cadr orig)))`, Expected: values.NewInteger(2)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestListCopy_ElementSharing(t *testing.T) {
	// Verify that car elements are shared (not deep copied)
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "element sharing via eq?", Code: `
			(let ((inner (list 1 2)))
			  (let ((orig (list inner 3)))
			    (let ((copy (list-copy orig)))
			      (eq? (car orig) (car copy)))))`, Expected: values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// ============================================================================
// Search error tests: memq, memv, member
// ============================================================================

func TestMemq_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"memq non-list second arg integer", `(memq 1 42)`},
		{"memq non-list second arg string", `(memq 'a "hello")`},
		{"memq non-list second arg vector", `(memq 1 #(1 2 3))`},
		{"memq non-list second arg boolean", `(memq 'a #t)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestMemv_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"memv non-list second arg integer", `(memv 1 42)`},
		{"memv non-list second arg string", `(memv 1 "hello")`},
		{"memv non-list second arg vector", `(memv 1 #(1 2 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestMember_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"member non-list second arg integer", `(member 1 42)`},
		{"member non-list second arg string", `(member 'a "hello")`},
		{"member non-list second arg vector", `(member 1 #(1 2 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ============================================================================
// Association list error tests: assq, assv, assoc
// ============================================================================

func TestAssq_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"assq non-list second arg", `(assq 'a 42)`},
		{"assq non-list second arg string", `(assq 'a "hello")`},
		{"assq malformed alist entry", `(assq 'a '(not-a-pair))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestAssv_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"assv non-list second arg", `(assv 1 42)`},
		{"assv non-list second arg string", `(assv 1 "hello")`},
		{"assv malformed alist entry", `(assv 1 '(not-a-pair))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestAssoc_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"assoc non-list second arg", `(assoc 'a 42)`},
		{"assoc non-list second arg string", `(assoc 'a "hello")`},
		{"assoc malformed alist entry", `(assoc 'a '(not-a-pair))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ============================================================================
// Append error tests
// ============================================================================

func TestAppend_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"append non-list first arg", `(append 42 '(1))`},
		{"append non-list middle arg", `(append '(1) 42 '(3))`},
		{"append non-list string arg", `(append "hello" '(1))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.code)
		})
	}
}

// TestRestArgBufferAliasing verifies that the rest-arg list returned by
// variadic primitives is properly copied and not corrupted by subsequent
// variadic calls. The rest-arg buffer (restArgBuf) is reused across calls;
// PrimList copies the spine to prevent aliasing.
func TestRestArgBufferAliasing(t *testing.T) {
	code := `
		(let ((first (list 1 2 3))
		      (second (list 4 5 6)))
		  (list first second))
	`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals,
		values.List(
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
			values.List(values.NewInteger(4), values.NewInteger(5), values.NewInteger(6)),
		))
}
