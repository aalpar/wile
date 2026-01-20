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

package primitives_test

import (
	"testing"

	"wile/values"

	qt "github.com/frankban/quicktest"
)

func TestExactIntegerQ(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// True cases
		{name: "exact-integer? on integer", code: `(exact-integer? 42)`, expected: values.TrueValue},
		{name: "exact-integer? on negative integer", code: `(exact-integer? -42)`, expected: values.TrueValue},
		{name: "exact-integer? on zero", code: `(exact-integer? 0)`, expected: values.TrueValue},
		{name: "exact-integer? on biginteger", code: `(exact-integer? #z123456789012345678901234567890)`, expected: values.TrueValue},

		// False cases
		{name: "exact-integer? on float", code: `(exact-integer? 42.0)`, expected: values.FalseValue},
		{name: "exact-integer? on rational", code: `(exact-integer? 3/4)`, expected: values.FalseValue},
		{name: "exact-integer? on integer rational", code: `(exact-integer? 4/2)`, expected: values.FalseValue},
		{name: "exact-integer? on complex", code: `(exact-integer? 1+0i)`, expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}
