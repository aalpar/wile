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

// map Tests (R7RS §6.4 - Mapping over lists)

func TestMapComprehensive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Single list
		{Name: "map double", Code: `(map (lambda (x) (* x 2)) '(1 2 3))`, Expected: values.List(values.NewInteger(2), values.NewInteger(4), values.NewInteger(6))},
		{Name: "map identity", Code: `(map (lambda (x) x) '(a b c))`, Expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))},
		{Name: "map empty list", Code: `(map (lambda (x) x) '())`, Expected: values.EmptyList},

		// Multiple lists
		{Name: "map + two lists", Code: `(map + '(1 2 3) '(10 20 30))`, Expected: values.List(values.NewInteger(11), values.NewInteger(22), values.NewInteger(33))},
		{Name: "map - two lists", Code: `(map - '(10 20 30) '(1 2 3))`, Expected: values.List(values.NewInteger(9), values.NewInteger(18), values.NewInteger(27))},
		{Name: "map three lists", Code: `(map + '(1 2) '(10 20) '(100 200))`, Expected: values.List(values.NewInteger(111), values.NewInteger(222))},

		// Map with list constructor
		{Name: "map list", Code: `(map list '(a b) '(1 2))`, Expected: values.List(values.List(values.NewSymbol("a"), values.NewInteger(1)), values.List(values.NewSymbol("b"), values.NewInteger(2)))},

		// Map with cons
		{Name: "map cons", Code: `(map cons '(a b c) '(1 2 3))`, Expected: values.List(values.NewCons(values.NewSymbol("a"), values.NewInteger(1)), values.NewCons(values.NewSymbol("b"), values.NewInteger(2)), values.NewCons(values.NewSymbol("c"), values.NewInteger(3)))},

		// Unequal lengths - stops at shortest
		{Name: "unequal lengths", Code: `(map + '(1 2 3) '(10 20))`, Expected: values.List(values.NewInteger(11), values.NewInteger(22))},

		// Single element list
		{Name: "single element list", Code: `(map (lambda (x) (* x 10)) '(5))`, Expected: values.List(values.NewInteger(50))},

		// Four lists
		{Name: "four lists", Code: `(map list '(a b) '(1 2) '(x y) '(#t #f))`,
			Expected: values.List(
				values.List(values.NewSymbol("a"), values.NewInteger(1), values.NewSymbol("x"), values.TrueValue),
				values.List(values.NewSymbol("b"), values.NewInteger(2), values.NewSymbol("y"), values.FalseValue))},

		// Order verified via accumulation
		{Name: "order verified via accumulation", Code: `(let ((order '())) (map (lambda (x) (set! order (cons x order)) x) '(1 2 3)) order)`,
			Expected: values.List(values.NewInteger(3), values.NewInteger(2), values.NewInteger(1))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestMapErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "map non-procedure", Code: `(map 5 '(1 2 3))`},
		{Name: "map with non-list", Code: `(map + 5)`},
		{Name: "error propagation", Code: `(map (lambda (x) (error "boom")) '(1))`},
		{Name: "improper list as single argument", Code: `(map + '(1 2 . 3))`},
		{Name: "improper list as second argument", Code: `(map + '(1 2) '(3 . 4))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// The call-depth property lives in pkg/wile (TestMapCallDepthCeiling): these helpers
// build a MachineContext directly, which leaves maxCallDepth at 0 = unlimited, so a
// depth assertion here would pass against ANY map shape and prove nothing.

// TestMapResultIsFreshAndMutable guards the accumulate-and-reverse construction:
// the result must be freshly allocated cells the caller owns, never a splice of the
// input's spine. R7RS §6.4 leaves map's sharing unspecified, so this is a Wile
// guarantee rather than a conformance point — and it is the property a future
// allocation optimization is most likely to trade away without noticing.
func TestMapResultIsFreshAndMutable(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "set-car! on result does not disturb input",
			Code:     `(let* ((in (list 1 2 3)) (out (map (lambda (x) x) in))) (set-car! out 99) in)`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			Name:     "set-cdr! on result does not disturb input",
			Code:     `(let* ((in (list 1 2 3)) (out (map (lambda (x) x) in))) (set-cdr! out '()) in)`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			Name:     "result is not eq? to input",
			Code:     `(let ((in (list 1 2 3))) (eq? in (map (lambda (x) x) in)))`,
			Expected: values.FalseValue,
		},
		{
			Name:     "no accumulator artifact leads the result",
			Code:     `(car (map (lambda (x) (* x 10)) '(5 6)))`,
			Expected: values.NewInteger(50),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestMapExceptionGuard(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "guard catches exception mid-iteration",
			Code: `(guard (e (#t 'caught))
				(map (lambda (x) (if (= x 3) (error "mid-map") x)) '(1 2 3 4 5)))`,
			Expected: values.NewSymbol("caught"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
