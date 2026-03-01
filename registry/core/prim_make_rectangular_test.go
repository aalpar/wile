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

func TestMakeRectangularComprehensive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Integer args - R7RS §6.2.6: exact + exact = exact BigComplex
		{Name: "make-rectangular integers", Code: `(make-rectangular 3 4)`,
			Expected: values.NewBigComplex(values.NewBigIntegerFromInt64(3), values.NewBigIntegerFromInt64(4))},
		{Name: "make-rectangular negative integers", Code: `(make-rectangular -3 -4)`,
			Expected: values.NewBigComplex(values.NewBigIntegerFromInt64(-3), values.NewBigIntegerFromInt64(-4))},
		// When imaginary is zero, exact result is the exact real part
		{Name: "make-rectangular zero imaginary", Code: `(make-rectangular 5 0)`,
			Expected: values.NewBigIntegerFromInt64(5)},
		{Name: "make-rectangular zero real", Code: `(make-rectangular 0 5)`,
			Expected: values.NewBigComplex(values.NewBigIntegerFromInt64(0), values.NewBigIntegerFromInt64(5))},

		// Float args - inexact Complex
		{Name: "make-rectangular floats", Code: `(make-rectangular 3.0 4.0)`,
			Expected: values.NewComplexFromParts(3.0, 4.0)},
		// Mixed exact/inexact -> inexact Complex
		{Name: "make-rectangular mixed int float", Code: `(make-rectangular 3 4.0)`,
			Expected: values.NewComplexFromParts(3.0, 4.0)},

		// Rational args - exact BigComplex with Rational parts
		{Name: "make-rectangular rationals", Code: `(make-rectangular 1/2 3/4)`,
			Expected: values.NewBigComplex(values.NewRational(1, 2), values.NewRational(3, 4))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestMakeRectangularErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "make-rectangular string arg", Code: `(make-rectangular "3" 4)`},
		{Name: "make-rectangular symbol arg", Code: `(make-rectangular 3 'four)`},
		{Name: "make-rectangular complex arg", Code: `(make-rectangular 1+2i 3)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
