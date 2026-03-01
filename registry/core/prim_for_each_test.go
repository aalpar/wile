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

// for-each Tests (R7RS §6.4 - Side-effect iteration)

func TestForEachComprehensive(t *testing.T) {
	// for-each returns unspecified value, we test via side effects
	tcs := []testhelpers.SchemeCodeTestCase{
		// Verify side effects happen in order
		{
			Name: "for-each side effects order",
			Code: `(let ((result '()))
				(for-each (lambda (x) (set! result (cons x result))) '(1 2 3))
				result)`,
			Expected: values.List(values.NewInteger(3), values.NewInteger(2), values.NewInteger(1)),
		},
		{
			Name: "for-each multiple lists",
			Code: `(let ((result '()))
				(for-each (lambda (x y) (set! result (cons (+ x y) result))) '(1 2 3) '(10 20 30))
				result)`,
			Expected: values.List(values.NewInteger(33), values.NewInteger(22), values.NewInteger(11)),
		},
		{
			Name: "for-each empty list",
			Code: `(let ((called #f))
				(for-each (lambda (x) (set! called #t)) '())
				called)`,
			Expected: values.FalseValue,
		},
		// Single element
		{
			Name:     "for-each single element",
			Code:     `(let ((result 0)) (for-each (lambda (x) (set! result x)) '(42)) result)`,
			Expected: values.NewInteger(42),
		},
		// Three lists
		{
			Name:     "for-each three lists",
			Code:     `(let ((result '())) (for-each (lambda (a b c) (set! result (cons (+ a b c) result))) '(1 2) '(10 20) '(100 200)) result)`,
			Expected: values.List(values.NewInteger(222), values.NewInteger(111)),
		},
		// Returns void
		{
			Name:     "for-each returns void",
			Code:     `(for-each (lambda (x) x) '(1 2 3))`,
			Expected: values.Void,
		},
		// Unequal lengths - stops at shortest
		{
			Name:     "for-each unequal lengths",
			Code:     `(let ((count 0)) (for-each (lambda (a b) (set! count (+ count 1))) '(1 2 3) '(10 20)) count)`,
			Expected: values.NewInteger(2),
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

func TestForEachErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "for-each non-procedure", Code: `(for-each 5 '(1 2 3))`},
		{Name: "for-each with non-list", Code: `(for-each (lambda (x) x) 5)`},
		{Name: "error propagation", Code: `(for-each (lambda (x) (error "boom")) '(1))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
