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

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestExactQ(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Exact types
		{name: "exact? on integer", code: `(exact? 42)`, expected: values.TrueValue},
		{name: "exact? on negative integer", code: `(exact? -42)`, expected: values.TrueValue},
		{name: "exact? on rational", code: `(exact? 3/4)`, expected: values.TrueValue},
		{name: "exact? on biginteger", code: `(exact? #z123456789012345678901234567890)`, expected: values.TrueValue},

		// Exact complex (integer parts)
		{name: "exact? on exact complex", code: `(exact? 1+2i)`, expected: values.TrueValue},

		// Inexact types
		{name: "exact? on float", code: `(exact? 3.14)`, expected: values.FalseValue},
		{name: "exact? on inexact complex", code: `(exact? 1.0+2.0i)`, expected: values.FalseValue},
		{name: "exact? on +inf.0", code: `(exact? +inf.0)`, expected: values.FalseValue},
		{name: "exact? on +nan.0", code: `(exact? +nan.0)`, expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}
