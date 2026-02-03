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

	"github.com/aalpar/wile/go/values"

	qt "github.com/frankban/quicktest"
)

// ---------------------------------------------------------------------------
// vector (constructor)
// ---------------------------------------------------------------------------

func TestVector(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"empty vector", `(vector)`, values.NewVector()},
		{"single element", `(vector 1)`, values.NewVector(values.NewInteger(1))},
		{"multiple elements", `(vector 1 2 3)`,
			values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{"mixed types", `(vector 1 "two" #t)`,
			values.NewVector(values.NewInteger(1), values.NewString("two"), values.TrueValue)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// ---------------------------------------------------------------------------
// make-vector
// ---------------------------------------------------------------------------

func TestMakeVectorExtended(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"zero length", `(make-vector 0)`, values.NewVector()},
		{"without fill length", `(vector-length (make-vector 5))`, values.NewInteger(5)},
		{"with fill all elements", `(let ((v (make-vector 3 42))) (vector->list v))`,
			values.List(values.NewInteger(42), values.NewInteger(42), values.NewInteger(42))},
		{"large vector", `(vector-length (make-vector 100))`, values.NewInteger(100)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestMakeVector_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{"non-integer k", `(make-vector "5")`},
		{"negative k", `(make-vector -1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector-length
// ---------------------------------------------------------------------------

func TestVectorLengthExtended(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"empty vector", `(vector-length '#())`, values.NewInteger(0)},
		{"single element", `(vector-length '#(42))`, values.NewInteger(1)},
		{"large vector", `(vector-length (make-vector 100))`, values.NewInteger(100)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestVectorLength_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{"string arg", `(vector-length "hello")`},
		{"integer arg", `(vector-length 42)`},
		{"list arg", `(vector-length '(1 2 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector-ref
// ---------------------------------------------------------------------------

func TestVectorRefExtended(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"first element", `(vector-ref '#(a b c) 0)`, values.NewSymbol("a")},
		{"last element", `(vector-ref '#(a b c) 2)`, values.NewSymbol("c")},
		{"single element vector", `(vector-ref '#(42) 0)`, values.NewInteger(42)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestVectorRef_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{"non-vector", `(vector-ref '(1 2) 0)`},
		{"non-integer index", `(vector-ref '#(1 2) "0")`},
		{"negative index", `(vector-ref '#(1 2) -1)`},
		{"index equals length", `(vector-ref '#(1 2) 2)`},
		{"empty vector", `(vector-ref '#() 0)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector-set!
// ---------------------------------------------------------------------------

func TestVectorSet(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"set first element", `(let ((v (vector 1 2 3))) (vector-set! v 0 99) (vector-ref v 0))`,
			values.NewInteger(99)},
		{"set last element", `(let ((v (vector 1 2 3))) (vector-set! v 2 99) (vector-ref v 2))`,
			values.NewInteger(99)},
		{"set with different type", `(let ((v (vector 1 2 3))) (vector-set! v 0 "hello") (vector-ref v 0))`,
			values.NewString("hello")},
		{"other elements unchanged", `(let ((v (vector 1 2 3))) (vector-set! v 1 99) (vector-ref v 0))`,
			values.NewInteger(1)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestVectorSet_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{"non-vector", `(vector-set! '(1 2) 0 99)`},
		{"non-integer index", `(vector-set! (vector 1 2) "0" 99)`},
		{"negative index", `(vector-set! (vector 1 2) -1 99)`},
		{"index equals length", `(vector-set! (vector 1 2) 2 99)`},
		{"empty vector", `(vector-set! (vector) 0 99)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector->list (error cases; happy path already well-covered)
// ---------------------------------------------------------------------------

func TestVectorToList_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{"non-vector arg", `(vector->list 42)`},
		{"non-integer start", `(vector->list '#(1 2 3) "0")`},
		{"non-integer end", `(vector->list '#(1 2 3) 0 "3")`},
		{"negative start", `(vector->list '#(1 2 3) -1)`},
		{"end exceeds length", `(vector->list '#(1 2 3) 0 5)`},
		{"start exceeds end", `(vector->list '#(1 2 3) 2 1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ---------------------------------------------------------------------------
// list->vector
// ---------------------------------------------------------------------------

func TestListToVectorExtended(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"empty list", `(list->vector '())`, values.NewVector()},
		{"single element", `(list->vector '(42))`, values.NewVector(values.NewInteger(42))},
		{"multiple elements", `(list->vector '(1 2 3))`,
			values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{"mixed types", `(list->vector (list 1 "two" #t))`,
			values.NewVector(values.NewInteger(1), values.NewString("two"), values.TrueValue)},
		{"verify element access", `(vector-ref (list->vector '(a b c)) 1)`, values.NewSymbol("b")},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestListToVector_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{"non-list arg", `(list->vector 42)`},
		{"improper list", `(list->vector (cons 1 2))`},
		{"longer improper list", `(list->vector '(1 2 . 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector-copy
// ---------------------------------------------------------------------------

func TestVectorCopy(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"full copy", `(vector-copy '#(1 2 3))`,
			values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{"empty vector", `(vector-copy '#())`, values.NewVector()},
		{"with start", `(vector-copy '#(1 2 3 4 5) 2)`,
			values.NewVector(values.NewInteger(3), values.NewInteger(4), values.NewInteger(5))},
		{"with start and end", `(vector-copy '#(1 2 3 4 5) 1 3)`,
			values.NewVector(values.NewInteger(2), values.NewInteger(3))},
		{"start equals end", `(vector-copy '#(1 2 3) 1 1)`, values.NewVector()},
		{"full range explicit", `(vector-copy '#(1 2 3) 0 3)`,
			values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestVectorCopy_Independence(t *testing.T) {
	// Mutating the copy must not affect the original.
	code := `(let ((orig (vector 1 2 3)))
               (let ((cp (vector-copy orig)))
                 (vector-set! cp 0 99)
                 (vector-ref orig 0)))`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(1))
}

func TestVectorCopy_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{"non-vector", `(vector-copy 42)`},
		{"non-integer start", `(vector-copy '#(1 2 3) "0")`},
		{"non-integer end", `(vector-copy '#(1 2 3) 0 "3")`},
		{"negative start", `(vector-copy '#(1 2 3) -1)`},
		{"end exceeds length", `(vector-copy '#(1 2 3) 0 5)`},
		{"start exceeds end", `(vector-copy '#(1 2 3) 2 1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector-copy!
// ---------------------------------------------------------------------------

func TestVectorCopyTo(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"basic copy", `(let ((dest (vector 0 0 0))) (vector-copy! dest 0 '#(1 2 3)) (vector->list dest))`,
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{"copy with offset", `(let ((dest (vector 0 0 0 0 0))) (vector-copy! dest 2 '#(7 8 9)) (vector->list dest))`,
			values.List(values.NewInteger(0), values.NewInteger(0), values.NewInteger(7), values.NewInteger(8), values.NewInteger(9))},
		{"copy with source start end", `(let ((dest (vector 0 0 0))) (vector-copy! dest 0 '#(1 2 3 4 5) 1 3) (vector->list dest))`,
			values.List(values.NewInteger(2), values.NewInteger(3), values.NewInteger(0))},
		{"copy single element", `(let ((dest (vector 0 0 0))) (vector-copy! dest 1 '#(99) 0 1) (vector->list dest))`,
			values.List(values.NewInteger(0), values.NewInteger(99), values.NewInteger(0))},
		{"zero-length copy", `(let ((dest (vector 1 2 3))) (vector-copy! dest 0 '#(9 9 9) 1 1) (vector->list dest))`,
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{"overlapping same vector", `(let ((v (vector 1 2 3 4 5))) (vector-copy! v 1 v 0 3) (vector->list v))`,
			values.List(values.NewInteger(1), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(5))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestVectorCopyTo_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{"non-vector dest", `(vector-copy! '(1 2 3) 0 '#(1))`},
		{"non-integer at", `(vector-copy! (vector 1 2 3) "0" '#(1))`},
		{"non-vector source", `(vector-copy! (vector 1 2 3) 0 '(1))`},
		{"at negative", `(vector-copy! (vector 1 2 3) -1 '#(1))`},
		{"dest overflow", `(vector-copy! (vector 1 2) 1 '#(9 9 9))`},
		{"source start exceeds end", `(vector-copy! (vector 1 2 3) 0 '#(1 2 3) 2 1)`},
		{"source end exceeds length", `(vector-copy! (vector 1 2 3) 0 '#(1 2) 0 5)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector-fill!
// ---------------------------------------------------------------------------

func TestVectorFill(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"fill entire vector", `(let ((v (vector 1 2 3))) (vector-fill! v 0) (vector->list v))`,
			values.List(values.NewInteger(0), values.NewInteger(0), values.NewInteger(0))},
		{"fill with start", `(let ((v (vector 1 2 3))) (vector-fill! v 99 1) (vector->list v))`,
			values.List(values.NewInteger(1), values.NewInteger(99), values.NewInteger(99))},
		{"fill with start and end", `(let ((v (vector 1 2 3 4 5))) (vector-fill! v 0 1 3) (vector->list v))`,
			values.List(values.NewInteger(1), values.NewInteger(0), values.NewInteger(0), values.NewInteger(4), values.NewInteger(5))},
		{"fill empty range", `(let ((v (vector 1 2 3))) (vector-fill! v 99 1 1) (vector->list v))`,
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{"fill with string value", `(let ((v (vector 1 2 3))) (vector-fill! v "x") (vector->list v))`,
			values.List(values.NewString("x"), values.NewString("x"), values.NewString("x"))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestVectorFill_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{"non-vector", `(vector-fill! '(1 2 3) 0)`},
		{"non-integer start", `(vector-fill! (vector 1 2 3) 0 "1")`},
		{"non-integer end", `(vector-fill! (vector 1 2 3) 0 0 "3")`},
		{"negative start", `(vector-fill! (vector 1 2 3) 0 -1)`},
		{"end exceeds length", `(vector-fill! (vector 1 2 3) 0 0 5)`},
		{"start exceeds end", `(vector-fill! (vector 1 2 3) 0 2 1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector-append
// ---------------------------------------------------------------------------

func TestVectorAppend(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"no args", `(vector-append)`, values.NewVector()},
		{"single vector", `(vector-append '#(1 2 3))`,
			values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{"two vectors", `(vector-append '#(1 2) '#(3 4))`,
			values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4))},
		{"three vectors", `(vector-append '#(1) '#(2) '#(3))`,
			values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{"empty vectors", `(vector-append '#() '#())`, values.NewVector()},
		{"mix empty and non-empty", `(vector-append '#() '#(1 2) '#() '#(3))`,
			values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestVectorAppend_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{"non-vector arg", `(vector-append 42)`},
		{"non-vector second arg", `(vector-append '#(1) "hello")`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector-map
// ---------------------------------------------------------------------------

func TestVectorMap(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"double elements", `(vector-map (lambda (x) (* x 2)) '#(1 2 3))`,
			values.NewVector(values.NewInteger(2), values.NewInteger(4), values.NewInteger(6))},
		{"two vectors", `(vector-map + '#(1 2 3) '#(10 20 30))`,
			values.NewVector(values.NewInteger(11), values.NewInteger(22), values.NewInteger(33))},
		{"empty vector", `(vector-map (lambda (x) x) '#())`, values.NewVector()},
		{"unequal lengths uses shortest", `(vector-map + '#(1 2 3) '#(10 20))`,
			values.NewVector(values.NewInteger(11), values.NewInteger(22))},

		// Single element
		{"single element", `(vector-map (lambda (x) (* x 10)) '#(5))`,
			values.NewVector(values.NewInteger(50))},

		// Three vectors
		{"three vectors", `(vector-map + '#(1 2) '#(10 20) '#(100 200))`,
			values.NewVector(values.NewInteger(111), values.NewInteger(222))},

		// Identity preserves order
		{"identity preserves order", `(vector-map (lambda (x) x) '#(a b c))`,
			values.NewVector(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestVectorMap_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{"non-procedure", `(vector-map 42 '#(1))`},
		{"non-vector", `(vector-map + '(1 2))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector-for-each
// ---------------------------------------------------------------------------

func TestVectorForEach(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"accumulate sum", `(let ((sum 0)) (vector-for-each (lambda (x) (set! sum (+ sum x))) '#(1 2 3)) sum)`,
			values.NewInteger(6)},
		{"two vectors", `(let ((sum 0)) (vector-for-each (lambda (a b) (set! sum (+ sum a b))) '#(1 2) '#(10 20)) sum)`,
			values.NewInteger(33)},
		{"empty vector no calls", `(let ((called #f)) (vector-for-each (lambda (x) (set! called #t)) '#()) called)`,
			values.FalseValue},

		// Order verification
		{"order verification", `(let ((result '())) (vector-for-each (lambda (x) (set! result (cons x result))) '#(1 2 3)) result)`,
			values.List(values.NewInteger(3), values.NewInteger(2), values.NewInteger(1))},

		// Unequal lengths - stops at shortest
		{"unequal lengths", `(let ((count 0)) (vector-for-each (lambda (a b) (set! count (+ count 1))) '#(1 2 3) '#(10 20)) count)`,
			values.NewInteger(2)},

		// Three vectors
		{"three vectors", `(let ((result '())) (vector-for-each (lambda (a b c) (set! result (cons (+ a b c) result))) '#(1 2) '#(10 20) '#(100 200)) result)`,
			values.List(values.NewInteger(222), values.NewInteger(111))},

		// Returns void
		{"returns void", `(vector-for-each (lambda (x) x) '#(1 2 3))`,
			values.Void},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestVectorForEach_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{"non-procedure", `(vector-for-each 42 '#(1))`},
		{"non-vector", `(vector-for-each + '(1 2))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector->string
// ---------------------------------------------------------------------------

func TestVectorToString(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"basic conversion", `(vector->string '#(#\h #\e #\l #\l #\o))`, values.NewString("hello")},
		{"empty vector", `(vector->string '#())`, values.NewString("")},
		{"single character", `(vector->string '#(#\A))`, values.NewString("A")},
		{"with start and end", `(vector->string '#(#\h #\e #\l #\l #\o) 1 3)`, values.NewString("el")},
		{"with start only", `(vector->string '#(#\a #\b #\c) 1)`, values.NewString("bc")},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestVectorToString_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{"non-vector", `(vector->string "hello")`},
		{"non-character element", `(vector->string '#(1 2 3))`},
		{"non-integer start", `(vector->string '#(#\a #\b) "0")`},
		{"out-of-bounds end", `(vector->string '#(#\a #\b) 0 5)`},
		{"start exceeds end", `(vector->string '#(#\a #\b #\c) 2 1)`},
		{"negative start", `(vector->string '#(#\a #\b) -1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

// ---------------------------------------------------------------------------
// string->vector
// ---------------------------------------------------------------------------

func TestStringToVector(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{"basic conversion", `(string->vector "hello")`,
			values.NewVector(values.NewCharacter('h'), values.NewCharacter('e'),
				values.NewCharacter('l'), values.NewCharacter('l'), values.NewCharacter('o'))},
		{"empty string", `(string->vector "")`, values.NewVector()},
		{"single character", `(string->vector "A")`, values.NewVector(values.NewCharacter('A'))},
		{"with start and end", `(string->vector "hello" 1 3)`,
			values.NewVector(values.NewCharacter('e'), values.NewCharacter('l'))},
		{"with start only", `(string->vector "abc" 1)`,
			values.NewVector(values.NewCharacter('b'), values.NewCharacter('c'))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestStringToVector_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{"non-string arg", `(string->vector 42)`},
		{"non-integer start", `(string->vector "abc" "0")`},
		{"out-of-bounds end", `(string->vector "abc" 0 5)`},
		{"start exceeds end", `(string->vector "abc" 2 1)`},
		{"negative start", `(string->vector "abc" -1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}
