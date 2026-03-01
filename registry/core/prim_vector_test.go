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

// ---------------------------------------------------------------------------
// vector (constructor)
// ---------------------------------------------------------------------------

func TestVector(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "empty vector", Code: `(vector)`, Expected: values.NewVector()},
		{Name: "single element", Code: `(vector 1)`, Expected: values.NewVector(values.NewInteger(1))},
		{Name: "multiple elements", Code: `(vector 1 2 3)`, Expected: values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{Name: "mixed types", Code: `(vector 1 "two" #t)`, Expected: values.NewVector(values.NewInteger(1), values.NewString("two"), values.TrueValue)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// ---------------------------------------------------------------------------
// make-vector
// ---------------------------------------------------------------------------

func TestMakeVectorExtended(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "zero length", Code: `(make-vector 0)`, Expected: values.NewVector()},
		{Name: "without fill length", Code: `(vector-length (make-vector 5))`, Expected: values.NewInteger(5)},
		{Name: "with fill all elements", Code: `(let ((v (make-vector 3 42))) (vector->list v))`, Expected: values.List(values.NewInteger(42), values.NewInteger(42), values.NewInteger(42))},
		{Name: "large vector", Code: `(vector-length (make-vector 100))`, Expected: values.NewInteger(100)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestMakeVector_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-integer k", Code: `(make-vector "5")`},
		{Name: "negative k", Code: `(make-vector -1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector-length
// ---------------------------------------------------------------------------

func TestVectorLengthExtended(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "empty vector", Code: `(vector-length '#())`, Expected: values.NewInteger(0)},
		{Name: "single element", Code: `(vector-length '#(42))`, Expected: values.NewInteger(1)},
		{Name: "large vector", Code: `(vector-length (make-vector 100))`, Expected: values.NewInteger(100)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestVectorLength_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "string arg", Code: `(vector-length "hello")`},
		{Name: "integer arg", Code: `(vector-length 42)`},
		{Name: "list arg", Code: `(vector-length '(1 2 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector-ref
// ---------------------------------------------------------------------------

func TestVectorRefExtended(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "first element", Code: `(vector-ref '#(a b c) 0)`, Expected: values.NewSymbol("a")},
		{Name: "last element", Code: `(vector-ref '#(a b c) 2)`, Expected: values.NewSymbol("c")},
		{Name: "single element vector", Code: `(vector-ref '#(42) 0)`, Expected: values.NewInteger(42)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestVectorRef_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-vector", Code: `(vector-ref '(1 2) 0)`},
		{Name: "non-integer index", Code: `(vector-ref '#(1 2) "0")`},
		{Name: "negative index", Code: `(vector-ref '#(1 2) -1)`},
		{Name: "index equals length", Code: `(vector-ref '#(1 2) 2)`},
		{Name: "empty vector", Code: `(vector-ref '#() 0)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector-set!
// ---------------------------------------------------------------------------

func TestVectorSet(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "set first element", Code: `(let ((v (vector 1 2 3))) (vector-set! v 0 99) (vector-ref v 0))`, Expected: values.NewInteger(99)},
		{Name: "set last element", Code: `(let ((v (vector 1 2 3))) (vector-set! v 2 99) (vector-ref v 2))`, Expected: values.NewInteger(99)},
		{Name: "set with different type", Code: `(let ((v (vector 1 2 3))) (vector-set! v 0 "hello") (vector-ref v 0))`, Expected: values.NewString("hello")},
		{Name: "other elements unchanged", Code: `(let ((v (vector 1 2 3))) (vector-set! v 1 99) (vector-ref v 0))`, Expected: values.NewInteger(1)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestVectorSet_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-vector", Code: `(vector-set! '(1 2) 0 99)`},
		{Name: "non-integer index", Code: `(vector-set! (vector 1 2) "0" 99)`},
		{Name: "negative index", Code: `(vector-set! (vector 1 2) -1 99)`},
		{Name: "index equals length", Code: `(vector-set! (vector 1 2) 2 99)`},
		{Name: "empty vector", Code: `(vector-set! (vector) 0 99)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector->list (error cases; happy path already well-covered)
// ---------------------------------------------------------------------------

func TestVectorToList_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-vector arg", Code: `(vector->list 42)`},
		{Name: "non-integer start", Code: `(vector->list '#(1 2 3) "0")`},
		{Name: "non-integer end", Code: `(vector->list '#(1 2 3) 0 "3")`},
		{Name: "negative start", Code: `(vector->list '#(1 2 3) -1)`},
		{Name: "end exceeds length", Code: `(vector->list '#(1 2 3) 0 5)`},
		{Name: "start exceeds end", Code: `(vector->list '#(1 2 3) 2 1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ---------------------------------------------------------------------------
// list->vector
// ---------------------------------------------------------------------------

func TestListToVectorExtended(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "empty list", Code: `(list->vector '())`, Expected: values.NewVector()},
		{Name: "single element", Code: `(list->vector '(42))`, Expected: values.NewVector(values.NewInteger(42))},
		{Name: "multiple elements", Code: `(list->vector '(1 2 3))`, Expected: values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{Name: "mixed types", Code: `(list->vector (list 1 "two" #t))`, Expected: values.NewVector(values.NewInteger(1), values.NewString("two"), values.TrueValue)},
		{Name: "verify element access", Code: `(vector-ref (list->vector '(a b c)) 1)`, Expected: values.NewSymbol("b")},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestListToVector_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-list arg", Code: `(list->vector 42)`},
		{Name: "improper list", Code: `(list->vector (cons 1 2))`},
		{Name: "longer improper list", Code: `(list->vector '(1 2 . 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector-copy
// ---------------------------------------------------------------------------

func TestVectorCopy(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "full copy", Code: `(vector-copy '#(1 2 3))`, Expected: values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{Name: "empty vector", Code: `(vector-copy '#())`, Expected: values.NewVector()},
		{Name: "with start", Code: `(vector-copy '#(1 2 3 4 5) 2)`, Expected: values.NewVector(values.NewInteger(3), values.NewInteger(4), values.NewInteger(5))},
		{Name: "with start and end", Code: `(vector-copy '#(1 2 3 4 5) 1 3)`, Expected: values.NewVector(values.NewInteger(2), values.NewInteger(3))},
		{Name: "start equals end", Code: `(vector-copy '#(1 2 3) 1 1)`, Expected: values.NewVector()},
		{Name: "full range explicit", Code: `(vector-copy '#(1 2 3) 0 3)`, Expected: values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestVectorCopy_Independence(t *testing.T) {
	// Mutating the copy must not affect the original.
	code := `(let ((orig (vector 1 2 3)))
               (let ((cp (vector-copy orig)))
                 (vector-set! cp 0 99)
                 (vector-ref orig 0)))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(1))
}

func TestVectorCopy_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-vector", Code: `(vector-copy 42)`},
		{Name: "non-integer start", Code: `(vector-copy '#(1 2 3) "0")`},
		{Name: "non-integer end", Code: `(vector-copy '#(1 2 3) 0 "3")`},
		{Name: "negative start", Code: `(vector-copy '#(1 2 3) -1)`},
		{Name: "end exceeds length", Code: `(vector-copy '#(1 2 3) 0 5)`},
		{Name: "start exceeds end", Code: `(vector-copy '#(1 2 3) 2 1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector-copy!
// ---------------------------------------------------------------------------

func TestVectorCopyTo(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "basic copy", Code: `(let ((dest (vector 0 0 0))) (vector-copy! dest 0 '#(1 2 3)) (vector->list dest))`, Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{Name: "copy with offset", Code: `(let ((dest (vector 0 0 0 0 0))) (vector-copy! dest 2 '#(7 8 9)) (vector->list dest))`, Expected: values.List(values.NewInteger(0), values.NewInteger(0), values.NewInteger(7), values.NewInteger(8), values.NewInteger(9))},
		{Name: "copy with source start end", Code: `(let ((dest (vector 0 0 0))) (vector-copy! dest 0 '#(1 2 3 4 5) 1 3) (vector->list dest))`, Expected: values.List(values.NewInteger(2), values.NewInteger(3), values.NewInteger(0))},
		{Name: "copy single element", Code: `(let ((dest (vector 0 0 0))) (vector-copy! dest 1 '#(99) 0 1) (vector->list dest))`, Expected: values.List(values.NewInteger(0), values.NewInteger(99), values.NewInteger(0))},
		{Name: "zero-length copy", Code: `(let ((dest (vector 1 2 3))) (vector-copy! dest 0 '#(9 9 9) 1 1) (vector->list dest))`, Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{Name: "overlapping same vector", Code: `(let ((v (vector 1 2 3 4 5))) (vector-copy! v 1 v 0 3) (vector->list v))`, Expected: values.List(values.NewInteger(1), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(5))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestVectorCopyTo_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-vector dest", Code: `(vector-copy! '(1 2 3) 0 '#(1))`},
		{Name: "non-integer at", Code: `(vector-copy! (vector 1 2 3) "0" '#(1))`},
		{Name: "non-vector source", Code: `(vector-copy! (vector 1 2 3) 0 '(1))`},
		{Name: "at negative", Code: `(vector-copy! (vector 1 2 3) -1 '#(1))`},
		{Name: "dest overflow", Code: `(vector-copy! (vector 1 2) 1 '#(9 9 9))`},
		{Name: "source start exceeds end", Code: `(vector-copy! (vector 1 2 3) 0 '#(1 2 3) 2 1)`},
		{Name: "source end exceeds length", Code: `(vector-copy! (vector 1 2 3) 0 '#(1 2) 0 5)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector-fill!
// ---------------------------------------------------------------------------

func TestVectorFill(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "fill entire vector", Code: `(let ((v (vector 1 2 3))) (vector-fill! v 0) (vector->list v))`, Expected: values.List(values.NewInteger(0), values.NewInteger(0), values.NewInteger(0))},
		{Name: "fill with start", Code: `(let ((v (vector 1 2 3))) (vector-fill! v 99 1) (vector->list v))`, Expected: values.List(values.NewInteger(1), values.NewInteger(99), values.NewInteger(99))},
		{Name: "fill with start and end", Code: `(let ((v (vector 1 2 3 4 5))) (vector-fill! v 0 1 3) (vector->list v))`, Expected: values.List(values.NewInteger(1), values.NewInteger(0), values.NewInteger(0), values.NewInteger(4), values.NewInteger(5))},
		{Name: "fill empty range", Code: `(let ((v (vector 1 2 3))) (vector-fill! v 99 1 1) (vector->list v))`, Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{Name: "fill with string value", Code: `(let ((v (vector 1 2 3))) (vector-fill! v "x") (vector->list v))`, Expected: values.List(values.NewString("x"), values.NewString("x"), values.NewString("x"))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestVectorFill_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-vector", Code: `(vector-fill! '(1 2 3) 0)`},
		{Name: "non-integer start", Code: `(vector-fill! (vector 1 2 3) 0 "1")`},
		{Name: "non-integer end", Code: `(vector-fill! (vector 1 2 3) 0 0 "3")`},
		{Name: "negative start", Code: `(vector-fill! (vector 1 2 3) 0 -1)`},
		{Name: "end exceeds length", Code: `(vector-fill! (vector 1 2 3) 0 0 5)`},
		{Name: "start exceeds end", Code: `(vector-fill! (vector 1 2 3) 0 2 1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector-append
// ---------------------------------------------------------------------------

func TestVectorAppend(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "no args", Code: `(vector-append)`, Expected: values.NewVector()},
		{Name: "single vector", Code: `(vector-append '#(1 2 3))`, Expected: values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{Name: "two vectors", Code: `(vector-append '#(1 2) '#(3 4))`, Expected: values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4))},
		{Name: "three vectors", Code: `(vector-append '#(1) '#(2) '#(3))`, Expected: values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{Name: "empty vectors", Code: `(vector-append '#() '#())`, Expected: values.NewVector()},
		{Name: "mix empty and non-empty", Code: `(vector-append '#() '#(1 2) '#() '#(3))`, Expected: values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestVectorAppend_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-vector arg", Code: `(vector-append 42)`},
		{Name: "non-vector second arg", Code: `(vector-append '#(1) "hello")`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector-map
// ---------------------------------------------------------------------------

func TestVectorMap(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "double elements", Code: `(vector-map (lambda (x) (* x 2)) '#(1 2 3))`, Expected: values.NewVector(values.NewInteger(2), values.NewInteger(4), values.NewInteger(6))},
		{Name: "two vectors", Code: `(vector-map + '#(1 2 3) '#(10 20 30))`, Expected: values.NewVector(values.NewInteger(11), values.NewInteger(22), values.NewInteger(33))},
		{Name: "empty vector", Code: `(vector-map (lambda (x) x) '#())`, Expected: values.NewVector()},
		{Name: "unequal lengths uses shortest", Code: `(vector-map + '#(1 2 3) '#(10 20))`, Expected: values.NewVector(values.NewInteger(11), values.NewInteger(22))},

		// Single element
		{Name: "single element", Code: `(vector-map (lambda (x) (* x 10)) '#(5))`, Expected: values.NewVector(values.NewInteger(50))},

		// Three vectors
		{Name: "three vectors", Code: `(vector-map + '#(1 2) '#(10 20) '#(100 200))`, Expected: values.NewVector(values.NewInteger(111), values.NewInteger(222))},

		// Identity preserves order
		{Name: "identity preserves order", Code: `(vector-map (lambda (x) x) '#(a b c))`, Expected: values.NewVector(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestVectorMap_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-procedure", Code: `(vector-map 42 '#(1))`},
		{Name: "non-vector", Code: `(vector-map + '(1 2))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector-for-each
// ---------------------------------------------------------------------------

func TestVectorForEach(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "accumulate sum", Code: `(let ((sum 0)) (vector-for-each (lambda (x) (set! sum (+ sum x))) '#(1 2 3)) sum)`, Expected: values.NewInteger(6)},
		{Name: "two vectors", Code: `(let ((sum 0)) (vector-for-each (lambda (a b) (set! sum (+ sum a b))) '#(1 2) '#(10 20)) sum)`, Expected: values.NewInteger(33)},
		{Name: "empty vector no calls", Code: `(let ((called #f)) (vector-for-each (lambda (x) (set! called #t)) '#()) called)`, Expected: values.FalseValue},

		// Order verification
		{Name: "order verification", Code: `(let ((result '())) (vector-for-each (lambda (x) (set! result (cons x result))) '#(1 2 3)) result)`, Expected: values.List(values.NewInteger(3), values.NewInteger(2), values.NewInteger(1))},

		// Unequal lengths - stops at shortest
		{Name: "unequal lengths", Code: `(let ((count 0)) (vector-for-each (lambda (a b) (set! count (+ count 1))) '#(1 2 3) '#(10 20)) count)`, Expected: values.NewInteger(2)},

		// Three vectors
		{Name: "three vectors", Code: `(let ((result '())) (vector-for-each (lambda (a b c) (set! result (cons (+ a b c) result))) '#(1 2) '#(10 20) '#(100 200)) result)`, Expected: values.List(values.NewInteger(222), values.NewInteger(111))},

		// Returns void
		{Name: "returns void", Code: `(vector-for-each (lambda (x) x) '#(1 2 3))`, Expected: values.Void},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestVectorForEach_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-procedure", Code: `(vector-for-each 42 '#(1))`},
		{Name: "non-vector", Code: `(vector-for-each + '(1 2))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ---------------------------------------------------------------------------
// vector->string
// ---------------------------------------------------------------------------

func TestVectorToString(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "basic conversion", Code: `(vector->string '#(#\h #\e #\l #\l #\o))`, Expected: values.NewString("hello")},
		{Name: "empty vector", Code: `(vector->string '#())`, Expected: values.NewString("")},
		{Name: "single character", Code: `(vector->string '#(#\A))`, Expected: values.NewString("A")},
		{Name: "with start and end", Code: `(vector->string '#(#\h #\e #\l #\l #\o) 1 3)`, Expected: values.NewString("el")},
		{Name: "with start only", Code: `(vector->string '#(#\a #\b #\c) 1)`, Expected: values.NewString("bc")},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestVectorToString_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-vector", Code: `(vector->string "hello")`},
		{Name: "non-character element", Code: `(vector->string '#(1 2 3))`},
		{Name: "non-integer start", Code: `(vector->string '#(#\a #\b) "0")`},
		{Name: "out-of-bounds end", Code: `(vector->string '#(#\a #\b) 0 5)`},
		{Name: "start exceeds end", Code: `(vector->string '#(#\a #\b #\c) 2 1)`},
		{Name: "negative start", Code: `(vector->string '#(#\a #\b) -1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ---------------------------------------------------------------------------
// string->vector
// ---------------------------------------------------------------------------

func TestStringToVector(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "basic conversion", Code: `(string->vector "hello")`, Expected: values.NewVector(values.NewCharacter('h'), values.NewCharacter('e'),
			values.NewCharacter('l'), values.NewCharacter('l'), values.NewCharacter('o'))},
		{Name: "empty string", Code: `(string->vector "")`, Expected: values.NewVector()},
		{Name: "single character", Code: `(string->vector "A")`, Expected: values.NewVector(values.NewCharacter('A'))},
		{Name: "with start and end", Code: `(string->vector "hello" 1 3)`, Expected: values.NewVector(values.NewCharacter('e'), values.NewCharacter('l'))},
		{Name: "with start only", Code: `(string->vector "abc" 1)`, Expected: values.NewVector(values.NewCharacter('b'), values.NewCharacter('c'))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestStringToVector_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-string arg", Code: `(string->vector 42)`},
		{Name: "non-integer start", Code: `(string->vector "abc" "0")`},
		{Name: "out-of-bounds end", Code: `(string->vector "abc" 0 5)`},
		{Name: "start exceeds end", Code: `(string->vector "abc" 2 1)`},
		{Name: "negative start", Code: `(string->vector "abc" -1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}
