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

// map Tests (R7RS §6.4 - Mapping over lists)

func TestMapComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Single list
		{name: "map double", code: `(map (lambda (x) (* x 2)) '(1 2 3))`, expected: values.List(values.NewInteger(2), values.NewInteger(4), values.NewInteger(6))},
		{name: "map identity", code: `(map (lambda (x) x) '(a b c))`, expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))},
		{name: "map empty list", code: `(map (lambda (x) x) '())`, expected: values.EmptyList},

		// Multiple lists
		{name: "map + two lists", code: `(map + '(1 2 3) '(10 20 30))`, expected: values.List(values.NewInteger(11), values.NewInteger(22), values.NewInteger(33))},
		{name: "map - two lists", code: `(map - '(10 20 30) '(1 2 3))`, expected: values.List(values.NewInteger(9), values.NewInteger(18), values.NewInteger(27))},
		{name: "map three lists", code: `(map + '(1 2) '(10 20) '(100 200))`, expected: values.List(values.NewInteger(111), values.NewInteger(222))},

		// Map with list constructor
		{name: "map list", code: `(map list '(a b) '(1 2))`, expected: values.List(values.List(values.NewSymbol("a"), values.NewInteger(1)), values.List(values.NewSymbol("b"), values.NewInteger(2)))},

		// Map with cons
		{name: "map cons", code: `(map cons '(a b c) '(1 2 3))`, expected: values.List(values.NewCons(values.NewSymbol("a"), values.NewInteger(1)), values.NewCons(values.NewSymbol("b"), values.NewInteger(2)), values.NewCons(values.NewSymbol("c"), values.NewInteger(3)))},

		// Unequal lengths - stops at shortest
		{name: "unequal lengths", code: `(map + '(1 2 3) '(10 20))`, expected: values.List(values.NewInteger(11), values.NewInteger(22))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestMapErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "map non-procedure", code: `(map 5 '(1 2 3))`},
		{name: "map with non-list", code: `(map + 5)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
