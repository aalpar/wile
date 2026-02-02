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

	"github.com/aalpar/wile/go/values"

	qt "github.com/frankban/quicktest"
)

// Combined Tests - Integration scenarios for control flow primitives

func TestControlFlowCombinations(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// map with apply
		{
			name:     "map with apply",
			code:     `(map (lambda (args) (apply + args)) '((1 2) (3 4) (5 6)))`,
			expected: values.List(values.NewInteger(3), values.NewInteger(7), values.NewInteger(11)),
		},

		// call-with-values with map
		{
			name:     "call-with-values from division in map",
			code:     `(map (lambda (n) (call-with-values (lambda () (floor/ n 3)) list)) '(10 11 12))`,
			expected: values.List(values.List(values.NewInteger(3), values.NewInteger(1)), values.List(values.NewInteger(3), values.NewInteger(2)), values.List(values.NewInteger(4), values.NewInteger(0))),
		},

		// dynamic-wind with map
		{
			name: "dynamic-wind inside map",
			code: `(let ((count 0))
				(map (lambda (x)
					(dynamic-wind
						(lambda () (set! count (+ count 1)))
						(lambda () x)
						(lambda () #f)))
					'(a b c))
				count)`,
			expected: values.NewInteger(3),
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
