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

func TestSort_SwappedArguments(t *testing.T) {
	// Calling sort list-first -- (sort lst less?) -- is a common habit carried
	// over from list-first Scheme dialects (e.g. Racket). wile is comparator-first.
	// The swap should produce an error that names sort and explains the argument
	// order, not a confusing "length: expected a list" error raised deep inside
	// the merge sort with no mention of sort or the swap.
	_, err := testhelpers.RunSchemeCode(t, `(sort '(3 1 2) <)`)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "sort:")
	qt.Assert(t, err.Error(), qt.Contains, "swapped")
}

func TestSort_GuardPrecision(t *testing.T) {
	// TODO(you): This test validates the *core design property* of the swap
	// guard -- precision. The guard must fire ONLY on the unambiguous swap (a
	// procedure sitting in the list slot). It must NOT relabel every sort
	// type-error as a swap, or the hint becomes noise.
	//
	// Reference: TestSort_SwappedArguments above pins the positive case.
	//
	// Assert the negative cases -- swap-shaped but not actually a swap:
	//   1. (sort '(1 2) '(3 4))  both args non-procedures. The predicate
	//      (and (not (procedure? less?)) (procedure? lst)) is false because
	//      the list slot isn't a procedure -> should still error, but the
	//      message should NOT contain "swapped".
	//   2. (sort < 42)           valid comparator, non-list second arg ->
	//      generic "not a list" error, NOT "swapped".
	//
	// The design question you are encoding: when exactly one argument is wrong
	// and it isn't the detectable swap, does the user get the precise generic
	// error, or a (possibly misleading) swap hint? Your assertion fixes that
	// answer -- and guards against a future "helpful" broadening that would
	// over-fire.
	t.Skip("TODO: assert guard precision -- see comment above")
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
