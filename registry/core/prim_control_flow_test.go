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

// Combined Tests - Integration scenarios for control flow primitives

func TestControlFlowCombinations(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// map with apply
		{
			Name:     "map with apply",
			Code:     `(map (lambda (args) (apply + args)) '((1 2) (3 4) (5 6)))`,
			Expected: values.List(values.NewInteger(3), values.NewInteger(7), values.NewInteger(11)),
		},

		// call-with-values with map
		{
			Name:     "call-with-values from division in map",
			Code:     `(map (lambda (n) (call-with-values (lambda () (floor/ n 3)) list)) '(10 11 12))`,
			Expected: values.List(values.List(values.NewInteger(3), values.NewInteger(1)), values.List(values.NewInteger(3), values.NewInteger(2)), values.List(values.NewInteger(4), values.NewInteger(0))),
		},

		// dynamic-wind with map
		{
			Name: "dynamic-wind inside map",
			Code: `(let ((count 0))
				(map (lambda (x)
					(dynamic-wind
						(lambda () (set! count (+ count 1)))
						(lambda () x)
						(lambda () #f)))
					'(a b c))
				count)`,
			Expected: values.NewInteger(3),
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
