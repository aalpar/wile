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

package machine_test

import (
	"testing"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestMachineContextSubcontext(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// apply exercises sub-context creation
		{Name: "apply with list", Code: `(apply + '(1 2 3))`, Expected: values.NewInteger(6)},
		{Name: "apply with prefix args", Code: `(apply + 1 2 '(3))`, Expected: values.NewInteger(6)},
		{Name: "apply lambda", Code: `(apply (lambda (x y) (* x y)) '(3 4))`, Expected: values.NewInteger(12)},

		// call-with-values exercises sub-context
		{Name: "call-with-values basic", Code: `(call-with-values (lambda () (values 1 2)) +)`, Expected: values.NewInteger(3)},
		{Name: "call-with-values single", Code: `(call-with-values (lambda () 42) (lambda (x) x))`, Expected: values.NewInteger(42)},

		// map exercises sub-context per element
		{Name: "map single list", Code: `(map (lambda (x) (* x 2)) '(1 2 3))`, Expected: values.List(values.NewInteger(2), values.NewInteger(4), values.NewInteger(6))},
		{Name: "map two lists", Code: `(map + '(1 2 3) '(10 20 30))`, Expected: values.List(values.NewInteger(11), values.NewInteger(22), values.NewInteger(33))},

		// for-each exercises sub-context with side effects
		{
			Name: "for-each accumulates",
			Code: `(let ((sum 0))
                     (for-each (lambda (x) (set! sum (+ sum x))) '(1 2 3))
                     sum)`,
			Expected: values.NewInteger(6),
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

func TestMachineContextSubcontextErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "apply non-procedure", Code: `(apply 5 '(1 2))`},
		{Name: "apply non-list last arg", Code: `(apply + 1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
