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

func TestExact(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Integer - already exact
		{Name: "exact on integer", Code: `(exact 42)`, Expected: values.NewInteger(42)},
		{Name: "exact on negative integer", Code: `(exact -42)`, Expected: values.NewInteger(-42)},
		{Name: "exact on zero", Code: `(exact 0)`, Expected: values.NewInteger(0)},

		// Float to rational
		{Name: "exact on float 0.5", Code: `(exact 0.5)`, Expected: values.NewRational(1, 2)},
		{Name: "exact on float 0.25", Code: `(exact 0.25)`, Expected: values.NewRational(1, 4)},
		{Name: "exact on float 1.5", Code: `(exact 1.5)`, Expected: values.NewRational(3, 2)},
		// R7RS §6.2.6: exact on integer float returns Integer (simpler exact representation)
		{Name: "exact on integer float", Code: `(exact 3.0)`, Expected: values.NewInteger(3)},

		// Rational - already exact
		{Name: "exact on rational", Code: `(exact 3/4)`, Expected: values.NewRational(3, 4)},
		{Name: "exact on negative rational", Code: `(exact -3/4)`, Expected: values.NewRational(-3, 4)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestExact_Complex tests exact on complex numbers.
// R7RS §6.2.6: exact converts both real and imaginary parts.
func TestExact_Complex(t *testing.T) {
	// exact on inexact complex produces exact BigComplex
	t.Run("exact on 1.5+2.5i", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(exact 1.5+2.5i)")
		qt.Assert(t, err, qt.IsNil)
		_, ok := result.(*values.BigComplex)
		qt.Assert(t, ok, qt.IsTrue)
	})

	t.Run("exact on 3.0+0.0i", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(exact 3.0+0.0i)")
		qt.Assert(t, err, qt.IsNil)
		_, ok := result.(*values.BigComplex)
		qt.Assert(t, ok, qt.IsTrue)
	})

	t.Run("exact? of exact complex", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectTrue(t, "(exact? (exact 1.5+2.5i))")
	})

	// exact on already-exact complex (BigComplex with integer parts)
	t.Run("exact on exact complex passthrough", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectTrue(t, "(exact? (exact 1+2i))")
	})
}

// TestExact_ComplexFractionalParts tests H4 regression:
// (exact <complex>) must convert fractional parts to Rational, not truncate to Integer.
//
// Architectural Review H4: toExactPart converts BigFloat to integer by
// truncation instead of Rational. (exact 1.5+0i) produces 1 instead of 3/2.
func TestExact_ComplexFractionalParts(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Fractional real part should become Rational 3/2, not Integer 1
		{
			Name:     "fractional real preserves fraction",
			Code:     "(= (real-part (exact 1.5+0i)) 3/2)",
			Expected: values.TrueValue,
		},
		{
			Name:     "fractional imag preserves fraction",
			Code:     "(= (imag-part (exact 0+2.5i)) 5/2)",
			Expected: values.TrueValue,
		},
		{
			Name:     "both fractional parts preserve fractions",
			Code:     "(and (= (real-part (exact 1.5+2.5i)) 3/2) (= (imag-part (exact 1.5+2.5i)) 5/2))",
			Expected: values.TrueValue,
		},
		// Integer-valued floats should simplify to integers
		{
			Name:     "integer-valued real simplifies",
			Code:     "(integer? (real-part (exact 3.0+2.5i)))",
			Expected: values.TrueValue,
		},
		{
			Name:     "integer-valued imag simplifies",
			Code:     "(integer? (imag-part (exact 1.5+4.0i)))",
			Expected: values.TrueValue,
		},
		// Verify exactness is preserved
		{
			Name:     "exact complex with fractional parts is exact",
			Code:     "(exact? (exact 1.5+2.5i))",
			Expected: values.TrueValue,
		},
		{
			Name:     "real part of exact complex is exact",
			Code:     "(exact? (real-part (exact 1.5+2.5i)))",
			Expected: values.TrueValue,
		},
		{
			Name:     "imag part of exact complex is exact",
			Code:     "(exact? (imag-part (exact 1.5+2.5i)))",
			Expected: values.TrueValue,
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

func TestExactErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "exact on non-number string", Code: `(exact "hello")`},
		{Name: "exact on symbol", Code: `(exact 'foo)`},
		{Name: "exact on list", Code: `(exact '(1 2 3))`},
		{Name: "exact on +inf.0", Code: `(exact +inf.0)`},
		{Name: "exact on -inf.0", Code: `(exact -inf.0)`},
		{Name: "exact on +nan.0", Code: `(exact +nan.0)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
