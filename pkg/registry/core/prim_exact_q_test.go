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

func TestExactQ(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Exact types
		{Name: "exact? on integer", Code: `(exact? 42)`, Expected: values.TrueValue},
		{Name: "exact? on negative integer", Code: `(exact? -42)`, Expected: values.TrueValue},
		{Name: "exact? on rational", Code: `(exact? 3/4)`, Expected: values.TrueValue},
		{Name: "exact? on biginteger", Code: `(exact? #z123456789012345678901234567890)`, Expected: values.TrueValue},

		// Exact complex (integer parts)
		{Name: "exact? on exact complex", Code: `(exact? 1+2i)`, Expected: values.TrueValue},

		// Inexact types
		{Name: "exact? on float", Code: `(exact? 3.14)`, Expected: values.FalseValue},
		{Name: "exact? on inexact complex", Code: `(exact? 1.0+2.0i)`, Expected: values.FalseValue},
		{Name: "exact? on +inf.0", Code: `(exact? +inf.0)`, Expected: values.FalseValue},
		{Name: "exact? on +nan.0", Code: `(exact? +nan.0)`, Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
