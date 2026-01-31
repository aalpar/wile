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

// for-each Tests (R7RS §6.4 - Side-effect iteration)

func TestForEachComprehensive(t *testing.T) {
	// for-each returns unspecified value, we test via side effects
	tcs := []schemeCodeTestCase{
		// Verify side effects happen in order
		{
			name: "for-each side effects order",
			code: `(let ((result '()))
				(for-each (lambda (x) (set! result (cons x result))) '(1 2 3))
				result)`,
			expected: values.List(values.NewInteger(3), values.NewInteger(2), values.NewInteger(1)),
		},
		{
			name: "for-each multiple lists",
			code: `(let ((result '()))
				(for-each (lambda (x y) (set! result (cons (+ x y) result))) '(1 2 3) '(10 20 30))
				result)`,
			expected: values.List(values.NewInteger(33), values.NewInteger(22), values.NewInteger(11)),
		},
		{
			name: "for-each empty list",
			code: `(let ((called #f))
				(for-each (lambda (x) (set! called #t)) '())
				called)`,
			expected: values.FalseValue,
		},
		// Single element
		{
			name:     "for-each single element",
			code:     `(let ((result 0)) (for-each (lambda (x) (set! result x)) '(42)) result)`,
			expected: values.NewInteger(42),
		},
		// Three lists
		{
			name:     "for-each three lists",
			code:     `(let ((result '())) (for-each (lambda (a b c) (set! result (cons (+ a b c) result))) '(1 2) '(10 20) '(100 200)) result)`,
			expected: values.List(values.NewInteger(222), values.NewInteger(111)),
		},
		// Returns void
		{
			name:     "for-each returns void",
			code:     `(for-each (lambda (x) x) '(1 2 3))`,
			expected: values.Void,
		},
		// Unequal lengths - stops at shortest
		{
			name:     "for-each unequal lengths",
			code:     `(let ((count 0)) (for-each (lambda (a b) (set! count (+ count 1))) '(1 2 3) '(10 20)) count)`,
			expected: values.NewInteger(2),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestForEachErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "for-each non-procedure", code: `(for-each 5 '(1 2 3))`},
		{name: "for-each with non-list", code: `(for-each (lambda (x) x) 5)`},
		{name: "error propagation", code: `(for-each (lambda (x) (error "boom")) '(1))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
