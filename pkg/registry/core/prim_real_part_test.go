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
	"math/big"
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestRealPartExtended(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Exact complex (integer parts parsed as exact BigComplex)
		{Name: "real-part of complex", Code: `(real-part 3+4i)`, Expected: values.NewBigIntegerFromInt64(3)},
		{Name: "real-part of complex negative", Code: `(real-part -3+4i)`, Expected: values.NewBigIntegerFromInt64(-3)},
		{Name: "real-part of pure imaginary", Code: `(real-part 0+4i)`, Expected: values.NewBigIntegerFromInt64(0)},

		// Inexact complex (float parts)
		{Name: "real-part of inexact complex", Code: `(real-part 3.0+4.0i)`, Expected: values.NewFloat(3.0)},
		{Name: "real-part of inexact complex negative", Code: `(real-part -3.0+4.0i)`, Expected: values.NewFloat(-3.0)},

		// Real numbers: real-part is the number itself (exactness preserved per R7RS §6.2.6)
		{Name: "real-part of integer", Code: `(real-part 5)`, Expected: values.NewInteger(5)},
		{Name: "real-part of float", Code: `(real-part 5.5)`, Expected: values.NewFloat(5.5)},
		{Name: "real-part of rational", Code: `(real-part 3/4)`, Expected: values.NewRationalFromBigInt(big.NewInt(3), big.NewInt(4))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestRealPartErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "real-part of string", Code: `(real-part "hello")`},
		{Name: "real-part of symbol", Code: `(real-part 'foo)`},
		{Name: "real-part of list", Code: `(real-part '(1 2 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
