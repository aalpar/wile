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

func TestImagPartExtended(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Exact complex (integer parts parsed as exact BigComplex)
		{name: "imag-part of complex", code: `(imag-part 3+4i)`, expected: values.NewBigIntegerFromInt64(4)},
		{name: "imag-part of complex negative", code: `(imag-part 3-4i)`, expected: values.NewBigIntegerFromInt64(-4)},
		{name: "imag-part of pure real complex", code: `(imag-part 3+0i)`, expected: values.NewBigIntegerFromInt64(0)},

		// Inexact complex (float parts)
		{name: "imag-part of inexact complex", code: `(imag-part 3.0+4.0i)`, expected: values.NewFloat(4.0)},
		{name: "imag-part of inexact complex negative", code: `(imag-part 3.0-4.0i)`, expected: values.NewFloat(-4.0)},

		// Real numbers: imag-part is 0 with exactness matching input per R7RS §6.2.6
		{name: "imag-part of integer", code: `(imag-part 5)`, expected: values.NewInteger(0)},
		{name: "imag-part of float", code: `(imag-part 5.5)`, expected: values.NewFloat(0.0)},
		{name: "imag-part of rational", code: `(imag-part 3/4)`, expected: values.NewInteger(0)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

func TestImagPartErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "imag-part of string", code: `(imag-part "hello")`},
		{name: "imag-part of symbol", code: `(imag-part 'foo)`},
		{name: "imag-part of list", code: `(imag-part '(1 2 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
