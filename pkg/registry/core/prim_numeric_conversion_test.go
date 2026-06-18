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
	"math"
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// ----------------------------------------------------------------------------
// integer? Predicate Tests
// ----------------------------------------------------------------------------

func TestIntegerQ(t *testing.T) {
	t.Run("integer? on exact integer", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(integer? 42)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
	})

	t.Run("integer? on inexact integer", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(integer? 3.0)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
	})

	t.Run("integer? on non-integer float", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(integer? 3.5)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, valuestest.SchemeEquals, values.FalseValue)
	})

	t.Run("integer? on non-integer rational", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(integer? 1/2)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, valuestest.SchemeEquals, values.FalseValue)
	})

	t.Run("integer? on integer rational", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(integer? 1/1)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
	})

	t.Run("integer? on complex with zero imaginary", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(integer? 3+0i)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
	})

	t.Run("integer? on complex with non-zero imaginary", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(integer? 3+1i)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, valuestest.SchemeEquals, values.FalseValue)
	})

	t.Run("integer? on negative integer", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(integer? -5)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
	})

	t.Run("integer? on zero", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(integer? 0)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
	})

	t.Run("integer? on non-number", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(integer? \"hello\")")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, valuestest.SchemeEquals, values.FalseValue)
	})
}

// ----------------------------------------------------------------------------
// sqrt Tests
// ----------------------------------------------------------------------------

func TestSqrtExtended(t *testing.T) {
	t.Run("sqrt of perfect square", func(t *testing.T) {
		// R7RS §6.2.6: sqrt of exact perfect square returns exact integer
		result, err := testhelpers.RunSchemeCode(t, "(sqrt 4)")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(2))
	})

	t.Run("sqrt of non-perfect square", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(sqrt 2.0)")
		qt.Assert(t, err, qt.IsNil)
		floatResult, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.Abs(floatResult.Value-1.4142135623730951) < 0.000001, qt.IsTrue)
	})

	t.Run("sqrt of negative number returns complex", func(t *testing.T) {
		// R7RS §6.2.6: sqrt of exact negative perfect square returns exact BigComplex
		result, err := testhelpers.RunSchemeCode(t, "(sqrt -1)")
		qt.Assert(t, err, qt.IsNil)
		bcResult, ok := result.(*values.BigComplex)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, bcResult.Real().IsZero(), qt.IsTrue)
		qt.Assert(t, bcResult.IsExact(), qt.IsTrue)
		qt.Assert(t, result, valuestest.SchemeEquals, values.NewBigComplex(
			values.NewBigIntegerFromInt64(0),
			values.NewBigIntegerFromInt64(1),
		))
	})

	t.Run("sqrt of complex number", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(sqrt 1+0i)")
		qt.Assert(t, err, qt.IsNil)
		complexResult, ok := result.(*values.Complex)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, real(complexResult.Value), qt.Equals, 1.0)
		qt.Assert(t, imag(complexResult.Value), qt.Equals, 0.0)
	})

	t.Run("sqrt of zero", func(t *testing.T) {
		// R7RS §6.2.6: sqrt of exact 0 returns exact 0
		result, err := testhelpers.RunSchemeCode(t, "(sqrt 0)")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(0))
	})

	t.Run("sqrt of rational", func(t *testing.T) {
		// R7RS §6.2.6: sqrt of exact perfect-square rational returns exact rational
		result, err := testhelpers.RunSchemeCode(t, "(sqrt 1/4)")
		qt.Assert(t, err, qt.IsNil)
		rResult, ok := result.(*values.Rational)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, rResult, valuestest.SchemeEquals, values.NewRational(1, 2))
	})
}

// ----------------------------------------------------------------------------
// expt Tests
// ----------------------------------------------------------------------------

func TestExptExtended(t *testing.T) {
	t.Run("expt with positive integer exponent", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(expt 2 3)")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(8))
	})

	t.Run("expt with negative integer exponent", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(expt 2 -1)")
		qt.Assert(t, err, qt.IsNil)
		// R7RS: exact inputs should give exact output
		ratResult, ok := result.(*values.Rational)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, ratResult.Float64Truncated(), qt.Equals, 0.5)
	})

	t.Run("expt with float base", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(expt 2.0 3)")
		qt.Assert(t, err, qt.IsNil)
		floatResult, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, floatResult.Value, qt.Equals, 8.0)
	})

	t.Run("expt with float exponent", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(expt 2 3.0)")
		qt.Assert(t, err, qt.IsNil)
		floatResult, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, floatResult.Value, qt.Equals, 8.0)
	})

	t.Run("expt with complex base", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(expt 1+1i 2)")
		qt.Assert(t, err, qt.IsNil)
		complexResult, ok := result.(*values.Complex)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.Abs(real(complexResult.Value)) < 0.000001, qt.IsTrue)
		qt.Assert(t, math.Abs(imag(complexResult.Value)-2.0) < 0.000001, qt.IsTrue)
	})

	t.Run("expt with zero exponent", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(expt 5 0)")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(1))
	})

	t.Run("expt with rational base", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(expt 1/2 2)")
		qt.Assert(t, err, qt.IsNil)
		// R7RS: exact inputs should give exact output
		ratResult, ok := result.(*values.Rational)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, ratResult.Float64Truncated(), qt.Equals, 0.25)
	})

	t.Run("expt with large exponent", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(expt 10 2)")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(100))
	})
}

// ----------------------------------------------------------------------------
// numerator Tests
// ----------------------------------------------------------------------------

func TestNumeratorExtended(t *testing.T) {
	t.Run("numerator of rational", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(numerator 3/4)")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(3))
	})

	t.Run("numerator of integer", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(numerator 5)")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(5))
	})

	t.Run("numerator of float", func(t *testing.T) {
		// R7RS §6.2.6: inexact input → inexact output
		result, err := testhelpers.RunSchemeCode(t, "(numerator 2.5)")
		qt.Assert(t, err, qt.IsNil)
		floatResult, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, floatResult.Value, qt.Equals, 5.0)
	})

	t.Run("numerator of negative rational", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(numerator -3/4)")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(-3))
	})

	t.Run("numerator of zero", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(numerator 0)")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(0))
	})
}

// ----------------------------------------------------------------------------
// denominator Tests
// ----------------------------------------------------------------------------

func TestDenominatorExtended(t *testing.T) {
	t.Run("denominator of rational", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(denominator 3/4)")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(4))
	})

	t.Run("denominator of integer", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(denominator 5)")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(1))
	})

	t.Run("denominator of float", func(t *testing.T) {
		// R7RS §6.2.6: inexact input → inexact output
		result, err := testhelpers.RunSchemeCode(t, "(denominator 2.5)")
		qt.Assert(t, err, qt.IsNil)
		floatResult, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, floatResult.Value, qt.Equals, 2.0)
	})

	t.Run("denominator of negative rational", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(denominator -3/4)")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(4))
	})

	t.Run("denominator of integer zero", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(denominator 0)")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(1))
	})

	t.Run("denominator of simplified rational", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(denominator 2/4)")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(2))
	})
}

// ----------------------------------------------------------------------------
// number->string Tests
// ----------------------------------------------------------------------------

func TestNumberToStringExtended(t *testing.T) {
	t.Run("number->string on integer", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(number->string 42)")
		qt.Assert(t, err, qt.IsNil)
		strResult, ok := result.(*values.String)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, strResult.Value, qt.Equals, "42")
	})

	t.Run("number->string on hex", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(number->string 42 16)")
		qt.Assert(t, err, qt.IsNil)
		strResult, ok := result.(*values.String)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, strResult.Value, qt.Equals, "2a")
	})

	t.Run("number->string on binary", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(number->string 42 2)")
		qt.Assert(t, err, qt.IsNil)
		strResult, ok := result.(*values.String)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, strResult.Value, qt.Equals, "101010")
	})

	t.Run("number->string on octal", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(number->string 42 8)")
		qt.Assert(t, err, qt.IsNil)
		strResult, ok := result.(*values.String)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, strResult.Value, qt.Equals, "52")
	})

	t.Run("number->string on float", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(number->string 3.14)")
		qt.Assert(t, err, qt.IsNil)
		strResult, ok := result.(*values.String)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, strResult.Value, qt.Equals, "3.14")
	})

	t.Run("number->string on negative integer", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(number->string -42)")
		qt.Assert(t, err, qt.IsNil)
		strResult, ok := result.(*values.String)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, strResult.Value, qt.Equals, "-42")
	})

	t.Run("number->string on zero", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(number->string 0)")
		qt.Assert(t, err, qt.IsNil)
		strResult, ok := result.(*values.String)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, strResult.Value, qt.Equals, "0")
	})

	t.Run("number->string on rational", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(number->string 1/2)")
		qt.Assert(t, err, qt.IsNil)
		strResult, ok := result.(*values.String)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, strResult.Value, qt.Equals, "1/2")
	})

	t.Run("number->string on complex", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(number->string 1+2i)")
		qt.Assert(t, err, qt.IsNil)
		strResult, ok := result.(*values.String)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, strResult.Value, qt.Equals, "1+2i")
	})
}

// ----------------------------------------------------------------------------
// string->number Tests
// ----------------------------------------------------------------------------

func TestStringToNumberExtended(t *testing.T) {
	t.Run("string->number on integer string", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(string->number \"42\")")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(42))
	})

	t.Run("string->number on hex string", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(string->number \"2a\" 16)")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(42))
	})

	t.Run("string->number on invalid string", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(string->number \"xyz\")")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, valuestest.SchemeEquals, values.FalseValue)
	})

	t.Run("string->number on float string", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(string->number \"3.14\")")
		qt.Assert(t, err, qt.IsNil)
		floatResult, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, floatResult.Value, qt.Equals, 3.14)
	})

	t.Run("string->number on negative integer string", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(string->number \"-42\")")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(-42))
	})

	t.Run("string->number on zero string", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(string->number \"0\")")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(0))
	})

	t.Run("string->number on binary string", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(string->number \"101010\" 2)")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(42))
	})

	t.Run("string->number on octal string", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(string->number \"52\" 8)")
		qt.Assert(t, err, qt.IsNil)
		intResult, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, intResult.Value, qt.Equals, int64(42))
	})

	t.Run("string->number on empty string", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(string->number \"\")")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, valuestest.SchemeEquals, values.FalseValue)
	})

	t.Run("string->number on negative float string", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(string->number \"-3.14\")")
		qt.Assert(t, err, qt.IsNil)
		floatResult, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, floatResult.Value, qt.Equals, -3.14)
	})
}

// TestStringToNumber_Prefixes tests R7RS §7.1.1 prefix directives in string->number.
// Radix prefixes: #b (binary), #o (octal), #d (decimal), #x (hex).
// Exactness prefixes: #e (exact), #i (inexact).
// Up to two prefixes (one radix + one exactness) in either order.
func TestStringToNumber_Prefixes(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Radix prefixes
		{Name: "hex prefix", Code: `(string->number "#x2a")`, Expected: values.NewInteger(42)},
		{Name: "binary prefix", Code: `(string->number "#b101010")`, Expected: values.NewInteger(42)},
		{Name: "octal prefix", Code: `(string->number "#o52")`, Expected: values.NewInteger(42)},
		{Name: "decimal prefix", Code: `(string->number "#d42")`, Expected: values.NewInteger(42)},

		// Exactness prefixes
		{Name: "exact prefix on float", Code: `(string->number "#e1.5")`, Expected: values.NewRational(3, 2)},
		{Name: "exact prefix on integer float", Code: `(string->number "#e3.0")`, Expected: values.NewInteger(3)},
		{Name: "inexact prefix on integer", Code: `(string->number "#i3")`, Expected: values.NewFloat(3.0)},
		{Name: "inexact prefix on rational", Code: `(string->number "#i1/2")`, Expected: values.NewFloat(0.5)},

		// Combined prefixes (both orders)
		{Name: "exact + hex", Code: `(string->number "#e#x2a")`, Expected: values.NewInteger(42)},
		{Name: "hex + exact", Code: `(string->number "#x#e2a")`, Expected: values.NewInteger(42)},
		{Name: "inexact + binary", Code: `(string->number "#i#b101")`, Expected: values.NewFloat(5.0)},
		{Name: "binary + inexact", Code: `(string->number "#b#i101")`, Expected: values.NewFloat(5.0)},

		// Case insensitive prefixes
		{Name: "uppercase hex prefix", Code: `(string->number "#X2A")`, Expected: values.NewInteger(42)},
		{Name: "uppercase exact prefix", Code: `(string->number "#E1.5")`, Expected: values.NewRational(3, 2)},

		// Rational parsing
		{Name: "rational string", Code: `(string->number "3/4")`, Expected: values.NewRational(3, 4)},
		{Name: "rational simplification", Code: `(string->number "10/2")`, Expected: values.NewInteger(5)},

		// Radix prefix overrides argument
		{Name: "prefix overrides arg radix", Code: `(string->number "#x2a" 10)`, Expected: values.NewInteger(42)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}

	// Invalid prefix tests — should return #f
	t.Run("invalid prefix returns false", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(string->number "#q42")`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, valuestest.SchemeEquals, values.FalseValue)
	})

	t.Run("empty after prefix returns false", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(string->number "#x")`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, valuestest.SchemeEquals, values.FalseValue)
	})
}

// ----------------------------------------------------------------------------
// numerator/denominator Exactness Preservation Tests
// R7RS §6.2.6: inexact input → inexact output, exact input → exact output
// ----------------------------------------------------------------------------

func TestNumeratorDenominator_Exactness(t *testing.T) {
	// Inexact input → inexact output
	t.Run("numerator of 0.5 is inexact", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(numerator 0.5)")
		qt.Assert(t, err, qt.IsNil)
		floatResult, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, floatResult.Value, qt.Equals, 1.0)
	})

	t.Run("denominator of 0.5 is inexact", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(denominator 0.5)")
		qt.Assert(t, err, qt.IsNil)
		floatResult, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, floatResult.Value, qt.Equals, 2.0)
	})

	t.Run("inexact? of numerator of 0.5", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectTrue(t, "(inexact? (numerator 0.5))")
	})

	t.Run("inexact? of denominator of 0.5", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectTrue(t, "(inexact? (denominator 0.5))")
	})

	// Exact input → exact output
	t.Run("exact? of numerator of 3/4", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectTrue(t, "(exact? (numerator 3/4))")
	})

	t.Run("exact? of denominator of 3/4", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectTrue(t, "(exact? (denominator 3/4))")
	})
}
