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

func TestImagPartExtended(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Exact complex (integer parts parsed as exact BigComplex)
		{Name: "imag-part of complex", Code: `(imag-part 3+4i)`, Expected: values.NewBigIntegerFromInt64(4)},
		{Name: "imag-part of complex negative", Code: `(imag-part 3-4i)`, Expected: values.NewBigIntegerFromInt64(-4)},
		{Name: "imag-part of pure real complex", Code: `(imag-part 3+0i)`, Expected: values.NewBigIntegerFromInt64(0)},

		// Inexact complex (float parts)
		{Name: "imag-part of inexact complex", Code: `(imag-part 3.0+4.0i)`, Expected: values.NewFloat(4.0)},
		{Name: "imag-part of inexact complex negative", Code: `(imag-part 3.0-4.0i)`, Expected: values.NewFloat(-4.0)},

		// Real numbers: imag-part is 0 with exactness matching input per R7RS §6.2.6
		{Name: "imag-part of integer", Code: `(imag-part 5)`, Expected: values.NewInteger(0)},
		{Name: "imag-part of float", Code: `(imag-part 5.5)`, Expected: values.NewFloat(0.0)},
		{Name: "imag-part of rational", Code: `(imag-part 3/4)`, Expected: values.NewInteger(0)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestImagPartErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "imag-part of string", Code: `(imag-part "hello")`},
		{Name: "imag-part of symbol", Code: `(imag-part 'foo)`},
		{Name: "imag-part of list", Code: `(imag-part '(1 2 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
