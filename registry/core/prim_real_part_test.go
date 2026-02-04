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

func TestRealPartExtended(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Exact complex (integer parts parsed as exact BigComplex)
		{name: "real-part of complex", code: `(real-part 3+4i)`, expected: values.NewBigIntegerFromInt64(3)},
		{name: "real-part of complex negative", code: `(real-part -3+4i)`, expected: values.NewBigIntegerFromInt64(-3)},
		{name: "real-part of pure imaginary", code: `(real-part 0+4i)`, expected: values.NewBigIntegerFromInt64(0)},

		// Inexact complex (float parts)
		{name: "real-part of inexact complex", code: `(real-part 3.0+4.0i)`, expected: values.NewFloat(3.0)},
		{name: "real-part of inexact complex negative", code: `(real-part -3.0+4.0i)`, expected: values.NewFloat(-3.0)},

		// Real numbers (imaginary part is 0)
		{name: "real-part of integer", code: `(real-part 5)`, expected: values.NewFloat(5.0)},
		{name: "real-part of float", code: `(real-part 5.5)`, expected: values.NewFloat(5.5)},
		{name: "real-part of rational", code: `(real-part 3/4)`, expected: values.NewFloat(0.75)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestRealPartErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "real-part of string", code: `(real-part "hello")`},
		{name: "real-part of symbol", code: `(real-part 'foo)`},
		{name: "real-part of list", code: `(real-part '(1 2 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
