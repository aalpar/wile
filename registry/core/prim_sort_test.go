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

func TestSort(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "empty list", Code: `(sort < '())`, Expected: values.EmptyList},
		{Name: "single element", Code: `(sort < '(42))`, Expected: values.List(values.NewInteger(42))},
		{Name: "already sorted", Code: `(sort < '(1 2 3))`, Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{Name: "reverse sorted", Code: `(sort < '(3 2 1))`, Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{Name: "duplicates", Code: `(sort < '(3 1 4 1 5 9 2 6))`, Expected: values.List(
			values.NewInteger(1), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3),
			values.NewInteger(4), values.NewInteger(5), values.NewInteger(6), values.NewInteger(9))},
		{Name: "descending order", Code: `(sort > '(1 2 3))`, Expected: values.List(values.NewInteger(3), values.NewInteger(2), values.NewInteger(1))},
		{Name: "two elements", Code: `(sort < '(5 3))`, Expected: values.List(values.NewInteger(3), values.NewInteger(5))},
		{Name: "strings", Code: `(sort string<? '("banana" "apple" "cherry"))`, Expected: values.List(
			values.NewString("apple"), values.NewString("banana"), values.NewString("cherry"))},
		{Name: "custom comparator", Code: `(sort (lambda (a b) (> a b)) '(1 3 2))`, Expected: values.List(
			values.NewInteger(3), values.NewInteger(2), values.NewInteger(1))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestSort_NonDestructive(t *testing.T) {
	code := `(let ((xs '(3 1 2)))
               (sort < xs)
               xs)`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals,
		values.List(values.NewInteger(3), values.NewInteger(1), values.NewInteger(2)))
}

func TestSort_Stable(t *testing.T) {
	// Sort pairs by first element; equal firsts should preserve original order.
	code := `(sort (lambda (a b) (< (car a) (car b)))
                   '((1 . a) (2 . b) (1 . c) (2 . d)))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals,
		values.List(
			values.NewCons(values.NewInteger(1), values.NewSymbol("a")),
			values.NewCons(values.NewInteger(1), values.NewSymbol("c")),
			values.NewCons(values.NewInteger(2), values.NewSymbol("b")),
			values.NewCons(values.NewInteger(2), values.NewSymbol("d"))))
}

func TestSort_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "wrong arity zero args", Code: `(sort)`},
		{Name: "wrong arity one arg", Code: `(sort <)`},
		{Name: "not a list", Code: `(sort < 42)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
