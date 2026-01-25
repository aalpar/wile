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

	"wile/values"

	qt "github.com/frankban/quicktest"
)

// define-values Tests (R7RS §5.3.3)

func TestDefineValuesComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Zero values
		{
			name:     "zero values",
			code:     `(begin (define-values () (values)) 'ok)`,
			expected: values.NewSymbol("ok"),
		},

		// Single value
		{
			name:     "single value",
			code:     `(begin (define-values (x) (values 42)) x)`,
			expected: values.NewInteger(42),
		},

		// Two values
		{
			name:     "two values",
			code:     `(begin (define-values (a b) (values 1 2)) (list a b))`,
			expected: values.List(values.NewInteger(1), values.NewInteger(2)),
		},

		// Three values
		{
			name:     "three values",
			code:     `(begin (define-values (a b c) (values 1 2 3)) (list a b c))`,
			expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},

		// Four values
		{
			name:     "four values",
			code:     `(begin (define-values (w x y z) (values 'a 'b 'c 'd)) (list w x y z))`,
			expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"), values.NewSymbol("d")),
		},

		// With floor/
		{
			name:     "floor/ returns two values",
			code:     `(begin (define-values (quot rem) (floor/ 17 5)) (list quot rem))`,
			expected: values.List(values.NewInteger(3), values.NewInteger(2)),
		},

		// Mixed types
		{
			name:     "mixed types",
			code:     `(begin (define-values (num str sym) (values 42 "hello" 'world)) (list num str sym))`,
			expected: values.List(values.NewInteger(42), values.NewString("hello"), values.NewSymbol("world")),
		},

		// Rest pattern: bare identifier collects all values as a list (R7RS §5.3.3)
		{
			name:     "rest pattern collects all values",
			code:     `(begin (define-values x (values 1 2)) x)`,
			expected: values.List(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			name:     "rest pattern single value",
			code:     `(begin (define-values x (values 42)) x)`,
			expected: values.List(values.NewInteger(42)),
		},
		{
			name:     "rest pattern no values",
			code:     `(begin (define-values x (values)) x)`,
			expected: values.EmptyList,
		},
		{
			name:     "rest pattern many values",
			code:     `(begin (define-values x (values 'a 'b 'c 'd 'e)) x)`,
			expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"), values.NewSymbol("d"), values.NewSymbol("e")),
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
