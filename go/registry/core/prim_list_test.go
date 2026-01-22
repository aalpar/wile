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

// ============================================================================
// car, cdr, cons
// ============================================================================

func TestCar(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"car of quoted list", `(car '(1 2 3))`, values.NewInteger(1)},
		{"car of pair", `(car (cons 'a 'b))`, values.NewSymbol("a")},
		{"car of single element list", `(car '(42))`, values.NewInteger(42)},
		{"car of nested list", `(car '((1 2) (3 4)))`, values.List(values.NewInteger(1), values.NewInteger(2))},
		{"car of list with mixed types", `(car '("hello" 1 #t))`, values.NewString("hello")},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestCar_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		// Note: (car '()) returns nil in this implementation rather than erroring
		{"car of integer", `(car 42)`},
		{"car of string", `(car "hello")`},
		{"car of symbol", `(car 'foo)`},
		{"car of boolean", `(car #t)`},
		{"car of vector", `(car #(1 2 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestCdr(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"cdr of quoted list", `(cdr '(1 2 3))`, values.List(values.NewInteger(2), values.NewInteger(3))},
		{"cdr of pair", `(cdr (cons 'a 'b))`, values.NewSymbol("b")},
		{"cdr of single element list", `(cdr '(42))`, values.EmptyList},
		{"cdr of two element list", `(cdr '(1 2))`, values.List(values.NewInteger(2))},
		{"cdr of nested list", `(cdr '((1 2) (3 4)))`, values.List(values.List(values.NewInteger(3), values.NewInteger(4)))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestCdr_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		// Note: (cdr '()) returns nil in this implementation rather than erroring
		{"cdr of integer", `(cdr 42)`},
		{"cdr of string", `(cdr "hello")`},
		{"cdr of symbol", `(cdr 'foo)`},
		{"cdr of boolean", `(cdr #t)`},
		{"cdr of vector", `(cdr #(1 2 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestCons(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Building proper lists
		{"cons two values", `(cons 1 2)`, values.NewCons(values.NewInteger(1), values.NewInteger(2))},
		{"cons with empty list", `(cons 1 '())`, values.List(values.NewInteger(1))},
		{"cons onto list", `(cons 1 '(2 3))`, values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},

		// Building improper lists
		{"cons symbols", `(cons 'a 'b)`, values.NewCons(values.NewSymbol("a"), values.NewSymbol("b"))},

		// Nested cons
		{"nested cons", `(cons (cons 1 2) (cons 3 4))`,
			values.NewCons(values.NewCons(values.NewInteger(1), values.NewInteger(2)),
				values.NewCons(values.NewInteger(3), values.NewInteger(4)))},

		// Various types
		{"cons string onto list", `(cons "hello" '())`, values.List(values.NewString("hello"))},
		{"cons list onto list", `(cons '(1 2) '(3 4))`,
			values.NewCons(values.List(values.NewInteger(1), values.NewInteger(2)),
				values.List(values.NewInteger(3), values.NewInteger(4)))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestList(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"list with three elements", `(list 1 2 3)`, values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{"list with no elements", `(list)`, values.EmptyList},
		{"list with one element", `(list 'a)`, values.List(values.NewSymbol("a"))},
		{"list with mixed types", `(list 1 "two" #t)`, values.List(values.NewInteger(1), values.NewString("two"), values.TrueValue)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// ============================================================================
// List Predicates: null?, pair?, list?
// ============================================================================

func TestNullQ(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// True cases - only empty list returns #t
		{"null? of empty list", `(null? '())`, values.TrueValue},

		// False cases - everything else returns #f
		{"null? of non-empty list", `(null? '(1 2 3))`, values.FalseValue},
		{"null? of single element list", `(null? '(1))`, values.FalseValue},
		{"null? of pair", `(null? (cons 1 2))`, values.FalseValue},
		{"null? of integer", `(null? 42)`, values.FalseValue},
		{"null? of string", `(null? "hello")`, values.FalseValue},
		{"null? of symbol", `(null? 'foo)`, values.FalseValue},
		{"null? of boolean true", `(null? #t)`, values.FalseValue},
		{"null? of boolean false", `(null? #f)`, values.FalseValue},
		{"null? of vector", `(null? #(1 2 3))`, values.FalseValue},
		{"null? of character", `(null? #\a)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestPairQ(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// True cases - pairs and non-empty lists
		{"pair? of cons cell", `(pair? (cons 1 2))`, values.TrueValue},
		{"pair? of non-empty list", `(pair? '(1 2 3))`, values.TrueValue},
		{"pair? of single element list", `(pair? '(1))`, values.TrueValue},
		{"pair? of nested list", `(pair? '((1 2) (3 4)))`, values.TrueValue},
		{"pair? of improper list", `(pair? '(1 2 . 3))`, values.TrueValue},

		// False cases - empty list is NOT a pair
		{"pair? of empty list", `(pair? '())`, values.FalseValue},
		{"pair? of integer", `(pair? 42)`, values.FalseValue},
		{"pair? of string", `(pair? "hello")`, values.FalseValue},
		{"pair? of symbol", `(pair? 'foo)`, values.FalseValue},
		{"pair? of boolean", `(pair? #t)`, values.FalseValue},
		{"pair? of vector", `(pair? #(1 2 3))`, values.FalseValue},
		{"pair? of character", `(pair? #\a)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestListQ(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// True cases - proper lists
		{"list? of empty list", `(list? '())`, values.TrueValue},
		{"list? of single element list", `(list? '(1))`, values.TrueValue},
		{"list? of multiple element list", `(list? '(1 2 3))`, values.TrueValue},
		{"list? of nested list", `(list? '((1 2) (3 4)))`, values.TrueValue},
		{"list? of list with mixed types", `(list? '(1 "two" #t))`, values.TrueValue},

		// False cases - improper lists and non-lists
		{"list? of improper list", `(list? (cons 1 2))`, values.FalseValue},
		{"list? of dotted list", `(list? '(1 2 . 3))`, values.FalseValue},
		{"list? of integer", `(list? 42)`, values.FalseValue},
		{"list? of string", `(list? "hello")`, values.FalseValue},
		{"list? of symbol", `(list? 'foo)`, values.FalseValue},
		{"list? of boolean", `(list? #t)`, values.FalseValue},
		{"list? of vector", `(list? #(1 2 3))`, values.FalseValue},
		{"list? of character", `(list? #\a)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// ============================================================================
// Mutation: set-car!, set-cdr!, list-set!
// ============================================================================

func TestSetCarBang(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"set-car! changes first element", `(let ((p (cons 1 2))) (set-car! p 10) (car p))`, values.NewInteger(10)},
		{"set-car! on list", `(let ((lst (list 1 2 3))) (set-car! lst 10) lst)`,
			values.List(values.NewInteger(10), values.NewInteger(2), values.NewInteger(3))},
		{"set-car! with different type", `(let ((p (cons 1 2))) (set-car! p "hello") (car p))`, values.NewString("hello")},
		{"set-car! preserves cdr", `(let ((p (cons 1 2))) (set-car! p 10) (cdr p))`, values.NewInteger(2)},
		{"set-car! on nested list", `(let ((lst '((1 2) (3 4)))) (set-car! lst '(10 20)) (car lst))`,
			values.List(values.NewInteger(10), values.NewInteger(20))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
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
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestSetCdrBang(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"set-cdr! changes cdr", `(let ((p (cons 1 2))) (set-cdr! p 20) (cdr p))`, values.NewInteger(20)},
		{"set-cdr! on list shortens it", `(let ((lst (list 1 2 3))) (set-cdr! lst '()) lst)`,
			values.List(values.NewInteger(1))},
		{"set-cdr! extends list", `(let ((lst (list 1))) (set-cdr! lst '(2 3)) lst)`,
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{"set-cdr! preserves car", `(let ((p (cons 1 2))) (set-cdr! p 20) (car p))`, values.NewInteger(1)},
		{"set-cdr! creates improper list", `(let ((lst (list 1 2))) (set-cdr! (cdr lst) 3) lst)`,
			values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.NewInteger(3)))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
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
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestListSetBang(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"list-set! at index 0", `(let ((lst (list 1 2 3))) (list-set! lst 0 10) lst)`,
			values.List(values.NewInteger(10), values.NewInteger(2), values.NewInteger(3))},
		{"list-set! at index 1", `(let ((lst (list 1 2 3))) (list-set! lst 1 20) lst)`,
			values.List(values.NewInteger(1), values.NewInteger(20), values.NewInteger(3))},
		{"list-set! at index 2", `(let ((lst (list 1 2 3))) (list-set! lst 2 30) lst)`,
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(30))},
		{"list-set! with string value", `(let ((lst (list 1 2 3))) (list-set! lst 1 "hello") lst)`,
			values.List(values.NewInteger(1), values.NewString("hello"), values.NewInteger(3))},
		{"list-set! with list value", `(let ((lst (list 1 2 3))) (list-set! lst 0 '(a b)) (car lst))`,
			values.List(values.NewSymbol("a"), values.NewSymbol("b"))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
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
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ============================================================================
// List Construction: make-list, append
// ============================================================================

func TestMakeList(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"make-list with fill", `(make-list 3 'a)`,
			values.List(values.NewSymbol("a"), values.NewSymbol("a"), values.NewSymbol("a"))},
		{"make-list with integer fill", `(make-list 4 0)`,
			values.List(values.NewInteger(0), values.NewInteger(0), values.NewInteger(0), values.NewInteger(0))},
		{"make-list single element", `(make-list 1 'x)`,
			values.List(values.NewSymbol("x"))},
		{"make-list zero length", `(make-list 0 'a)`, values.EmptyList},
		{"make-list without fill", `(length (make-list 5))`, values.NewInteger(5)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
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
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestAppend(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"append no arguments", `(append)`, values.EmptyList},
		{"append single empty list", `(append '())`, values.EmptyList},
		{"append single list", `(append '(1 2))`, values.List(values.NewInteger(1), values.NewInteger(2))},
		{"append two lists", `(append '(1 2) '(3 4))`,
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4))},
		{"append three lists", `(append '(a) '(b) '(c))`,
			values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))},
		{"append with empty list in middle", `(append '(1) '() '(2))`,
			values.List(values.NewInteger(1), values.NewInteger(2))},
		{"append with non-list as last argument", `(append '(1 2) 3)`,
			values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.NewInteger(3)))},
		{"append empty lists only", `(append '() '())`, values.EmptyList},
		{"append nested lists", `(append '((1 2)) '((3 4)))`,
			values.List(values.List(values.NewInteger(1), values.NewInteger(2)),
				values.List(values.NewInteger(3), values.NewInteger(4)))},
		{"append four lists", `(append '(1) '(2) '(3) '(4))`,
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4))},
		{"append lists with different types", `(append '(1 2) '("a" "b") '(#t #f))`,
			values.List(values.NewInteger(1), values.NewInteger(2),
				values.NewString("a"), values.NewString("b"),
				values.TrueValue, values.FalseValue)},
		{"append single element to list", `(append '(a b) 'c)`,
			values.NewCons(values.NewSymbol("a"),
				values.NewCons(values.NewSymbol("b"), values.NewSymbol("c")))},
		{"append all empty", `(append '() '() '())`, values.EmptyList},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// ============================================================================
// List Access: length, reverse, list-ref, list-tail
// ============================================================================

func TestLength(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"length of three element list", `(length '(1 2 3))`, values.NewInteger(3)},
		{"length of empty list", `(length '())`, values.NewInteger(0)},
		{"length of single element list", `(length '(a))`, values.NewInteger(1)},
		{"length of two element list", `(length '(a b))`, values.NewInteger(2)},
		{"length of five element list", `(length '(1 2 3 4 5))`, values.NewInteger(5)},
		{"length of nested list", `(length '((1 2) (3 4) (5 6)))`, values.NewInteger(3)},
		{"length of list with mixed types", `(length '(1 "two" #t 'four))`, values.NewInteger(4)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
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
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestReverse(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"reverse list", `(reverse '(1 2 3))`,
			values.List(values.NewInteger(3), values.NewInteger(2), values.NewInteger(1))},
		{"reverse empty list", `(reverse '())`, values.EmptyList},
		{"reverse single element", `(reverse '(a))`, values.List(values.NewSymbol("a"))},
		{"reverse two elements", `(reverse '(a b))`,
			values.List(values.NewSymbol("b"), values.NewSymbol("a"))},
		{"reverse five elements", `(reverse '(1 2 3 4 5))`,
			values.List(values.NewInteger(5), values.NewInteger(4), values.NewInteger(3), values.NewInteger(2), values.NewInteger(1))},
		{"reverse nested lists", `(reverse '((1 2) (3 4)))`,
			values.List(values.List(values.NewInteger(3), values.NewInteger(4)),
				values.List(values.NewInteger(1), values.NewInteger(2)))},
		{"reverse preserves nested structure", `(car (reverse '((1 2) (3 4))))`,
			values.List(values.NewInteger(3), values.NewInteger(4))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
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
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestListRef(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"list-ref first element", `(list-ref '(a b c) 0)`, values.NewSymbol("a")},
		{"list-ref middle element", `(list-ref '(a b c) 1)`, values.NewSymbol("b")},
		{"list-ref last element", `(list-ref '(a b c) 2)`, values.NewSymbol("c")},
		{"list-ref nested element", `(list-ref '((a) (b) (c)) 1)`, values.List(values.NewSymbol("b"))},
		{"list-ref in long list", `(list-ref '(1 2 3 4 5 6 7 8 9 10) 9)`, values.NewInteger(10)},
		{"list-ref first of many", `(list-ref '(a b c d e) 0)`, values.NewSymbol("a")},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestListRef_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"list-ref out of bounds", `(list-ref '(a b c) 5)`},
		{"list-ref negative index", `(list-ref '(a b c) -1)`},
		{"list-ref on non-list", `(list-ref 42 0)`},
		{"list-ref with non-integer index", `(list-ref '(a b c) "one")`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestListTail(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"list-tail from beginning", `(list-tail '(a b c) 0)`,
			values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))},
		{"list-tail skip one", `(list-tail '(a b c) 1)`,
			values.List(values.NewSymbol("b"), values.NewSymbol("c"))},
		{"list-tail skip all", `(list-tail '(a b c) 3)`, values.EmptyList},
		{"list-tail skip two", `(list-tail '(a b c d e) 2)`,
			values.List(values.NewSymbol("c"), values.NewSymbol("d"), values.NewSymbol("e"))},
		{"list-tail skip none", `(list-tail '(1 2) 0)`,
			values.List(values.NewInteger(1), values.NewInteger(2))},
		{"list-tail on empty list with k=0", `(list-tail '() 0)`, values.EmptyList},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
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
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ============================================================================
// Search: memq, memv, member
// ============================================================================

func TestMemq(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// memq uses eq? (pointer equality) - works for booleans (singletons) and symbols (interned)
		{"memq finds boolean", `(memq #t '(#f #t 1))`,
			values.List(values.TrueValue, values.NewInteger(1))},
		{"memq returns #f when not found", `(memq #t '(#f 1 2))`, values.FalseValue},
		{"memq with empty list returns #f", `(memq #t '())`, values.FalseValue},
		{"memq finds symbol", `(memq 'b '(a b c))`,
			values.List(values.NewSymbol("b"), values.NewSymbol("c"))},
		{"memq symbol not found", `(memq 'd '(a b c))`, values.FalseValue},
		{"memq finds first occurrence", `(memq #t '(#f #f #t #t))`,
			values.List(values.TrueValue, values.TrueValue)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestMemv(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// memv uses eqv? - compares numbers and characters by value
		{"memv finds integer", `(memv 2 '(1 2 3))`,
			values.List(values.NewInteger(2), values.NewInteger(3))},
		{"memv returns #f when not found", `(memv 4 '(1 2 3))`, values.FalseValue},
		{"memv finds integer in list", `(memv 3 '(1 2 3 4 5))`,
			values.List(values.NewInteger(3), values.NewInteger(4), values.NewInteger(5))},
		{"memv integer not found", `(memv 10 '(1 2 3))`, values.FalseValue},
		{"memv finds character", `(memv #\b '(#\a #\b #\c))`,
			values.List(values.NewCharacter('b'), values.NewCharacter('c'))},
		{"memv finds symbol", `(memv 'x '(a b x y z))`,
			values.List(values.NewSymbol("x"), values.NewSymbol("y"), values.NewSymbol("z"))},
		{"memv finds first element", `(memv 1 '(1 2 3))`,
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{"memv finds last element", `(memv 3 '(1 2 3))`,
			values.List(values.NewInteger(3))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestMember(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// member uses equal? - deep comparison
		{"member finds list with equal?", `(member '(2) '((1) (2) (3)))`,
			values.List(values.List(values.NewInteger(2)), values.List(values.NewInteger(3)))},
		{"member finds string", `(member "hello" '("world" "hello" "foo"))`,
			values.List(values.NewString("hello"), values.NewString("foo"))},
		{"member returns #f when not found", `(member '(4) '((1) (2) (3)))`, values.FalseValue},
		{"member string not found", `(member "bar" '("foo" "baz"))`, values.FalseValue},
		{"member finds nested list", `(member '(2 3) '((1 2) (2 3) (3 4)))`,
			values.List(values.List(values.NewInteger(2), values.NewInteger(3)),
				values.List(values.NewInteger(3), values.NewInteger(4)))},
		{"member finds integer", `(member 42 '(1 42 100))`,
			values.List(values.NewInteger(42), values.NewInteger(100))},
		{"member in empty list", `(member 'x '())`, values.FalseValue},
		{"member finds character", `(member #\b '(#\a #\b #\c))`,
			values.List(values.NewCharacter('b'), values.NewCharacter('c'))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// ============================================================================
// Association Lists: assq, assv, assoc
// ============================================================================

func TestAssq(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// assq uses eq? - works for booleans (singletons) and symbols (interned)
		{"assq finds boolean key", `(assq #t '((#f 1) (#t 2)))`,
			values.List(values.TrueValue, values.NewInteger(2))},
		{"assq returns #f when not found", `(assq #t '((#f 1)))`, values.FalseValue},
		{"assq with empty list returns #f", `(assq #t '())`, values.FalseValue},
		{"assq finds symbol key", `(assq 'b '((a 1) (b 2) (c 3)))`,
			values.List(values.NewSymbol("b"), values.NewInteger(2))},
		{"assq symbol not found", `(assq 'd '((a 1) (b 2) (c 3)))`, values.FalseValue},
		{"assq finds #f key", `(assq #f '((#t yes) (#f no)))`,
			values.List(values.FalseValue, values.NewSymbol("no"))},
		{"assq returns first match", `(assq 'a '((a 1) (a 2) (a 3)))`,
			values.List(values.NewSymbol("a"), values.NewInteger(1))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestAssv(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// assv uses eqv? - compares numbers and characters by value
		{"assv finds integer key", `(assv 2 '((1 a) (2 b) (3 c)))`,
			values.List(values.NewInteger(2), values.NewSymbol("b"))},
		{"assv returns #f when not found", `(assv 4 '((1 a) (2 b) (3 c)))`, values.FalseValue},
		{"assv integer not found", `(assv 5 '((1 one) (2 two)))`, values.FalseValue},
		{"assv finds character key", `(assv #\b '((#\a alpha) (#\b beta) (#\c gamma)))`,
			values.List(values.NewCharacter('b'), values.NewSymbol("beta"))},
		{"assv multiple entries", `(assv 0 '((0 zero) (1 one) (0 another-zero)))`,
			values.List(values.NewInteger(0), values.NewSymbol("zero"))},
		{"assv finds symbol key", `(assv 'x '((y why) (x ecks) (z zee)))`,
			values.List(values.NewSymbol("x"), values.NewSymbol("ecks"))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestAssoc(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// assoc uses equal? - deep comparison
		{"assoc finds list key with equal?", `(assoc '(1 2) '(((1 2) found) ((3 4) other)))`,
			values.List(values.List(values.NewInteger(1), values.NewInteger(2)), values.NewSymbol("found"))},
		{"assoc returns #f when not found", `(assoc '(5 6) '(((1 2) a) ((3 4) b)))`, values.FalseValue},
		{"assoc with string key", `(assoc "hello" '(("hello" found) ("world" other)))`,
			values.List(values.NewString("hello"), values.NewSymbol("found"))},
		{"assoc finds string key", `(assoc "hello" '(("world" 1) ("hello" 2)))`,
			values.List(values.NewString("hello"), values.NewInteger(2))},
		{"assoc list not found", `(assoc '(5 6) '(((1 2) a) ((3 4) b)))`, values.FalseValue},
		{"assoc in empty alist", `(assoc 'x '())`, values.FalseValue},
		{"assoc finds integer key", `(assoc 42 '((1 a) (42 b) (100 c)))`,
			values.List(values.NewInteger(42), values.NewSymbol("b"))},
		{"assoc finds character key", `(assoc #\y '((#\a alpha) (#\y why)))`,
			values.List(values.NewCharacter('y'), values.NewSymbol("why"))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}
