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

func TestExactIntegerQ(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// True cases
		{Name: "exact-integer? on integer", Code: `(exact-integer? 42)`, Expected: values.TrueValue},
		{Name: "exact-integer? on negative integer", Code: `(exact-integer? -42)`, Expected: values.TrueValue},
		{Name: "exact-integer? on zero", Code: `(exact-integer? 0)`, Expected: values.TrueValue},
		{Name: "exact-integer? on biginteger", Code: `(exact-integer? #z123456789012345678901234567890)`, Expected: values.TrueValue},

		// False cases
		{Name: "exact-integer? on float", Code: `(exact-integer? 42.0)`, Expected: values.FalseValue},
		{Name: "exact-integer? on rational", Code: `(exact-integer? 3/4)`, Expected: values.FalseValue},
		{Name: "exact-integer? on integer rational", Code: `(exact-integer? 4/2)`, Expected: values.TrueValue}, // 4/2 reduces to Integer(2) at parse time
		// R7RS §6.2.6: 1+0i is exact, and (integer? 1+0i) is #t since
		// imag is exactly zero and real is integer. So exact-integer? is #t.
		{Name: "exact-integer? on exact complex", Code: `(exact-integer? 1+0i)`, Expected: values.TrueValue},
		{Name: "exact-integer? on inexact complex", Code: `(exact-integer? 1.0+0.0i)`, Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
