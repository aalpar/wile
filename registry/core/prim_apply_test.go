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

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// apply Tests (R7RS §6.4 - Function application)

func TestApplyComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic apply
		{name: "apply + to list", code: `(apply + '(1 2 3))`, expected: values.NewInteger(6)},
		{name: "apply * to list", code: `(apply * '(2 3 4))`, expected: values.NewInteger(24)},
		{name: "apply - to list", code: `(apply - '(10 3 2))`, expected: values.NewInteger(5)},

		// Apply with prefix arguments
		{name: "apply with one prefix", code: `(apply + 1 '(2 3))`, expected: values.NewInteger(6)},
		{name: "apply with two prefix", code: `(apply + 1 2 '(3 4))`, expected: values.NewInteger(10)},
		{name: "apply with many prefix", code: `(apply + 1 2 3 4 '(5))`, expected: values.NewInteger(15)},

		// Empty list
		{name: "apply + to empty list", code: `(apply + '())`, expected: values.NewInteger(0)},
		{name: "apply * to empty list", code: `(apply * '())`, expected: values.NewInteger(1)},
		{name: "apply list to empty list", code: `(apply list '())`, expected: values.EmptyList},

		// Apply with lambda
		{name: "apply lambda", code: `(apply (lambda (x y) (+ x y)) '(3 4))`, expected: values.NewInteger(7)},
		{name: "apply variadic lambda", code: `(apply (lambda args args) '(1 2 3))`, expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},

		// Apply with cons
		{name: "apply cons", code: `(apply cons '(1 2))`, expected: values.NewCons(values.NewInteger(1), values.NewInteger(2))},
		{name: "apply car", code: `(apply car '((1 2 3)))`, expected: values.NewInteger(1)},

		// case-lambda dispatch
		{name: "case-lambda two args", code: `(apply (case-lambda ((x) x) ((x y) (+ x y))) '(3 4))`, expected: values.NewInteger(7)},
		{name: "case-lambda one arg", code: `(apply (case-lambda ((x) x) ((x y) (+ x y))) '(42))`, expected: values.NewInteger(42)},

		// Nested apply
		{name: "nested apply", code: `(apply apply (list + '(1 2 3)))`, expected: values.NewInteger(6)},

		// Build list with prefix args
		{name: "build list with prefix", code: `(apply list 1 2 '(3 4 5))`, expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4), values.NewInteger(5))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// TestApplyMultipleValues tests that apply correctly propagates multiple values.
// R7RS §6.4: apply should preserve multiple return values from the applied procedure.
func TestApplyMultipleValues(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// apply values should return multiple values
		{
			name:     "apply values with two args",
			code:     `(call-with-values (lambda () (apply values '(1 2))) list)`,
			expected: values.List(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			name:     "apply values with three args",
			code:     `(call-with-values (lambda () (apply values '(a b c))) list)`,
			expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
		},
		{
			name:     "apply values with zero args",
			code:     `(call-with-values (lambda () (apply values '())) (lambda () 'empty))`,
			expected: values.NewSymbol("empty"),
		},
		{
			name:     "apply values with one arg",
			code:     `(call-with-values (lambda () (apply values '(42))) (lambda (x) x))`,
			expected: values.NewInteger(42),
		},
		// apply a multi-value returning procedure
		{
			name:     "apply floor/ (multi-value)",
			code:     `(call-with-values (lambda () (apply floor/ '(17 5))) list)`,
			expected: values.List(values.NewInteger(3), values.NewInteger(2)),
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

func TestApplyErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "apply non-procedure", code: `(apply 5 '(1 2))`},
		{name: "apply without list", code: `(apply + 1 2 3)`},
		{name: "apply with improper list", code: `(apply + '(1 . 2))`},
		{name: "too many args", code: `(apply (lambda (x y) (+ x y)) '(1 2 3))`},
		{name: "too few args", code: `(apply (lambda (x y) (+ x y)) '(1))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
