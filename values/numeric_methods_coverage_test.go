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

package values_test

import (
	"math"
	"math/big"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

// TestIntegerPredicates covers IsInteger, IsRational, IsFinite, IsNaN, IsPositive,
// IsNegative, Sign, ToExact, and ToInexact for Integer.
func TestIntegerPredicates(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name       string
		val        *values.Integer
		isPositive bool
		isNegative bool
		sign       int
	}{
		{"positive", values.NewInteger(42), true, false, 1},
		{"negative", values.NewInteger(-7), false, true, -1},
		{"zero", values.NewInteger(0), false, false, 0},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(tc.val.IsInteger(), qt.IsTrue)
			c.Assert(tc.val.IsRational(), qt.IsTrue)
			c.Assert(tc.val.IsFinite(), qt.IsTrue)
			c.Assert(tc.val.IsNaN(), qt.IsFalse)
			c.Assert(tc.val.IsPositive(), qt.Equals, tc.isPositive)
			c.Assert(tc.val.IsNegative(), qt.Equals, tc.isNegative)
			c.Assert(tc.val.Sign(), qt.Equals, tc.sign)
		})
	}
}

func TestIntegerExactness(t *testing.T) {
	c := qt.New(t)
	i := values.NewInteger(5)

	exact := i.ToExact()
	c.Assert(exact, valuestest.SchemeEquals, i)

	inexact := i.ToInexact()
	f, ok := inexact.(*values.Float)
	c.Assert(ok, qt.IsTrue)
	c.Assert(f.Value, qt.Equals, 5.0)
}

func TestIntegerCompare(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		a    *values.Integer
		b    values.Number
		want int
	}{
		{"int<int", values.NewInteger(1), values.NewInteger(2), -1},
		{"int=int", values.NewInteger(5), values.NewInteger(5), 0},
		{"int>int", values.NewInteger(9), values.NewInteger(3), 1},
		{"int<float", values.NewInteger(1), values.NewFloat(1.5), -1},
		{"int=float", values.NewInteger(2), values.NewFloat(2.0), 0},
		{"int>float", values.NewInteger(3), values.NewFloat(2.5), 1},
		{"int<rational", values.NewInteger(1), values.NewRational(3, 2), -1},
		{"int<bigint", values.NewInteger(1), values.NewBigIntegerFromInt64(2), -1},
		{"int<bigfloat", values.NewInteger(1), values.NewBigFloatFromFloat64(1.5), -1},
		{"int<complex", values.NewInteger(1), values.NewComplexFromParts(2.0, 0.0), -1},
		{"int=complex", values.NewInteger(2), values.NewComplexFromParts(2.0, 0.0), 0},
		{"int<bigcomplex", values.NewInteger(1), values.NewBigComplex(values.NewBigIntegerFromInt64(2), values.NewBigIntegerFromInt64(0)), -1},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(tc.a.Compare(tc.b), qt.Equals, tc.want)
		})
	}
}

// TestFloatPredicates covers IsInteger, IsRational, IsFinite, IsNaN, IsPositive,
// IsNegative, Sign, Negate, Abs, ToExact, ToInexact, and HashCode for Float.
func TestFloatPredicates(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name       string
		val        *values.Float
		isInteger  bool
		isRational bool
		isFinite   bool
		isNaN      bool
		isPositive bool
		isNegative bool
		sign       int
	}{
		{"positive", values.NewFloat(3.14), false, true, true, false, true, false, 1},
		{"negative", values.NewFloat(-2.5), false, true, true, false, false, true, -1},
		{"zero", values.NewFloat(0.0), true, true, true, false, false, false, 0},
		{"integer value", values.NewFloat(7.0), true, true, true, false, true, false, 1},
		{"+inf", values.NewFloat(math.Inf(1)), false, false, false, false, true, false, 1},
		{"-inf", values.NewFloat(math.Inf(-1)), false, false, false, false, false, true, -1},
		{"NaN", values.NewFloat(math.NaN()), false, false, false, true, false, false, 0},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(tc.val.IsInteger(), qt.Equals, tc.isInteger)
			c.Assert(tc.val.IsRational(), qt.Equals, tc.isRational)
			c.Assert(tc.val.IsFinite(), qt.Equals, tc.isFinite)
			c.Assert(tc.val.IsNaN(), qt.Equals, tc.isNaN)
			c.Assert(tc.val.IsPositive(), qt.Equals, tc.isPositive)
			c.Assert(tc.val.IsNegative(), qt.Equals, tc.isNegative)
			c.Assert(tc.val.Sign(), qt.Equals, tc.sign)
		})
	}
}

func TestFloatAbsNegate(t *testing.T) {
	c := qt.New(t)

	f := values.NewFloat(-3.5)
	abs := f.Abs()
	c.Assert(abs, valuestest.SchemeEquals, values.NewFloat(3.5))

	neg := f.Negate()
	c.Assert(neg, valuestest.SchemeEquals, values.NewFloat(3.5))
}

func TestFloatExactness(t *testing.T) {
	c := qt.New(t)

	f := values.NewFloat(2.5)
	exact := f.ToExact()
	c.Assert(exact.IsExact(), qt.IsTrue)

	inexact := f.ToInexact()
	c.Assert(inexact, valuestest.SchemeEquals, f)
}

func TestFloatHashCode(t *testing.T) {
	c := qt.New(t)
	f1 := values.NewFloat(3.14)
	f2 := values.NewFloat(3.14)
	c.Assert(f1.HashCode(), qt.Equals, f2.HashCode())
}

func TestFloatCompare(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		a    *values.Float
		b    values.Number
		want int
	}{
		{"float<float", values.NewFloat(1.0), values.NewFloat(2.0), -1},
		{"float=float", values.NewFloat(5.0), values.NewFloat(5.0), 0},
		{"float>float", values.NewFloat(9.0), values.NewFloat(3.0), 1},
		{"float<int", values.NewFloat(1.0), values.NewInteger(2), -1},
		{"float<bigint", values.NewFloat(1.0), values.NewBigIntegerFromInt64(2), -1},
		{"float<bigfloat", values.NewFloat(1.0), values.NewBigFloatFromFloat64(2.0), -1},
		{"float<rational", values.NewFloat(1.0), values.NewRational(3, 2), -1},
		{"float<complex", values.NewFloat(1.0), values.NewComplexFromParts(2.0, 0.0), -1},
		{"float=complex", values.NewFloat(2.0), values.NewComplexFromParts(2.0, 3.0), 0},
		{"float<bigcomplex", values.NewFloat(1.0), values.NewBigComplex(values.NewBigIntegerFromInt64(2), values.NewBigIntegerFromInt64(0)), -1},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(tc.a.Compare(tc.b), qt.Equals, tc.want)
		})
	}
}

// TestComplexPredicates covers IsInteger, IsRational, IsFinite, IsNaN, Negate,
// Abs, RealPart, ImagPart, ToExact, ToInexact, and Compare for Complex.
func TestComplexPredicates(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name       string
		val        *values.Complex
		isInteger  bool
		isRational bool
		isFinite   bool
		isNaN      bool
	}{
		{"standard", values.NewComplexFromParts(1.0, 2.0), false, false, true, false},
		{"real integer", values.NewComplexFromParts(5.0, 0.0), true, true, true, false},
		{"with inf", values.NewComplexFromParts(math.Inf(1), 0.0), false, false, false, false},
		{"with NaN real", values.NewComplexFromParts(math.NaN(), 0.0), false, false, false, true},
		{"with NaN imag", values.NewComplexFromParts(0.0, math.NaN()), false, false, false, true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(tc.val.IsInteger(), qt.Equals, tc.isInteger)
			c.Assert(tc.val.IsRational(), qt.Equals, tc.isRational)
			c.Assert(tc.val.IsFinite(), qt.Equals, tc.isFinite)
			c.Assert(tc.val.IsNaN(), qt.Equals, tc.isNaN)
		})
	}
}

func TestComplexMethods(t *testing.T) {
	c := qt.New(t)

	z := values.NewComplexFromParts(3.0, 4.0)

	neg := z.Negate()
	negC := neg.(*values.Complex)
	c.Assert(real(negC.Value), qt.Equals, -3.0)
	c.Assert(imag(negC.Value), qt.Equals, -4.0)

	abs := z.Abs()
	absF := abs.(*values.Float)
	c.Assert(absF.Value, qt.Equals, 5.0)

	realPart := z.RealPart()
	c.Assert(realPart, valuestest.SchemeEquals, values.NewFloat(3.0))

	imagPart := z.ImagPart()
	c.Assert(imagPart, valuestest.SchemeEquals, values.NewFloat(4.0))
}

func TestComplexExactness(t *testing.T) {
	c := qt.New(t)

	z := values.NewComplexFromParts(2.0, 3.0)

	exact := z.ToExact()
	c.Assert(exact.IsExact(), qt.IsTrue)

	inexact := z.ToInexact()
	c.Assert(inexact, valuestest.SchemeEquals, z)
}

func TestComplexCompare(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		a    *values.Complex
		b    values.Number
		want int
	}{
		{"complex<complex", values.NewComplexFromParts(1.0, 0.0), values.NewComplexFromParts(2.0, 0.0), -1},
		{"complex=complex", values.NewComplexFromParts(5.0, 1.0), values.NewComplexFromParts(5.0, 2.0), 0},
		{"complex<float", values.NewComplexFromParts(1.0, 0.0), values.NewFloat(2.0), -1},
		{"complex<int", values.NewComplexFromParts(1.0, 0.0), values.NewInteger(2), -1},
		{"complex<bigint", values.NewComplexFromParts(1.0, 0.0), values.NewBigIntegerFromInt64(2), -1},
		{"complex<bigfloat", values.NewComplexFromParts(1.0, 0.0), values.NewBigFloatFromFloat64(2.0), -1},
		{"complex<rational", values.NewComplexFromParts(1.0, 0.0), values.NewRational(3, 1), -1},
		{"complex<bigcomplex", values.NewComplexFromParts(1.0, 0.0), values.NewBigComplex(values.NewBigIntegerFromInt64(2), values.NewBigIntegerFromInt64(0)), -1},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(tc.a.Compare(tc.b), qt.Equals, tc.want)
		})
	}
}

// TestRationalPredicates covers IsRational, IsFinite, IsNaN, IsPositive,
// IsNegative, Sign, Negate, Abs, ToExact, ToInexact, Compare, and HashCode for Rational.
func TestRationalPredicates(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name       string
		val        *values.Rational
		isPositive bool
		isNegative bool
		sign       int
	}{
		{"positive", values.NewRational(3, 4), true, false, 1},
		{"negative", values.NewRational(-1, 3), false, true, -1},
		{"zero", values.NewRational(0, 1), false, false, 0},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(tc.val.IsRational(), qt.IsTrue)
			c.Assert(tc.val.IsFinite(), qt.IsTrue)
			c.Assert(tc.val.IsNaN(), qt.IsFalse)
			c.Assert(tc.val.IsPositive(), qt.Equals, tc.isPositive)
			c.Assert(tc.val.IsNegative(), qt.Equals, tc.isNegative)
			c.Assert(tc.val.Sign(), qt.Equals, tc.sign)
		})
	}
}

func TestRationalAbsNegate(t *testing.T) {
	c := qt.New(t)

	r := values.NewRational(-3, 4)
	abs := r.Abs()
	c.Assert(abs.(*values.Rational).Rat().Sign(), qt.Equals, 1)

	neg := r.Negate()
	c.Assert(neg.(*values.Rational).Rat().Sign(), qt.Equals, 1)
}

func TestRationalExactness(t *testing.T) {
	c := qt.New(t)

	r := values.NewRational(1, 3)
	exact := r.ToExact()
	c.Assert(exact, valuestest.SchemeEquals, r)

	inexact := r.ToInexact()
	// L18: ToInexact now returns BigFloat for precision
	bf, ok := inexact.(*values.BigFloat)
	c.Assert(ok, qt.IsTrue)
	c.Assert(bf.Float64() > 0.33, qt.IsTrue)
}

func TestRationalHashCode(t *testing.T) {
	c := qt.New(t)
	r1 := values.NewRational(1, 2)
	r2 := values.NewRational(1, 2)
	c.Assert(r1.HashCode(), qt.Equals, r2.HashCode())
}

func TestRationalCompare(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		a    *values.Rational
		b    values.Number
		want int
	}{
		{"rat<rat", values.NewRational(1, 4), values.NewRational(1, 2), -1},
		{"rat=rat", values.NewRational(1, 2), values.NewRational(1, 2), 0},
		{"rat>rat", values.NewRational(3, 4), values.NewRational(1, 4), 1},
		{"rat<int", values.NewRational(1, 2), values.NewInteger(1), -1},
		{"rat<bigint", values.NewRational(1, 2), values.NewBigIntegerFromInt64(1), -1},
		{"rat<float", values.NewRational(1, 2), values.NewFloat(0.75), -1},
		{"rat<bigfloat", values.NewRational(1, 2), values.NewBigFloatFromFloat64(0.75), -1},
		{"rat<complex", values.NewRational(1, 2), values.NewComplexFromParts(1.0, 0.0), -1},
		{"rat<bigcomplex", values.NewRational(1, 2), values.NewBigComplex(values.NewBigIntegerFromInt64(1), values.NewBigIntegerFromInt64(0)), -1},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(tc.a.Compare(tc.b), qt.Equals, tc.want)
		})
	}
}

// TestBigIntegerPredicates covers IsInteger, IsRational, IsFinite, IsNaN,
// Abs, Sign, and HashCode for BigInteger.
func TestBigIntegerPredicates(t *testing.T) {
	c := qt.New(t)

	pos := values.NewBigIntegerFromInt64(42)
	neg := values.NewBigIntegerFromInt64(-7)
	zero := values.NewBigIntegerFromInt64(0)

	c.Assert(pos.IsInteger(), qt.IsTrue)
	c.Assert(pos.IsRational(), qt.IsTrue)
	c.Assert(pos.IsFinite(), qt.IsTrue)
	c.Assert(pos.IsNaN(), qt.IsFalse)
	c.Assert(pos.Sign(), qt.Equals, 1)
	c.Assert(neg.Sign(), qt.Equals, -1)
	c.Assert(zero.Sign(), qt.Equals, 0)

	abs := neg.Abs()
	c.Assert(abs.(*values.BigInteger).BigInt().Int64(), qt.Equals, int64(7))
}

func TestBigIntegerHashCode(t *testing.T) {
	c := qt.New(t)
	b1 := values.NewBigIntegerFromInt64(999)
	b2 := values.NewBigIntegerFromInt64(999)
	c.Assert(b1.HashCode(), qt.Equals, b2.HashCode())
}

// TestBigFloatPredicates covers IsInteger, IsRational, IsFinite, IsNaN,
// Abs, and Sign for BigFloat.
func TestBigFloatPredicates(t *testing.T) {
	c := qt.New(t)

	pos := values.NewBigFloatFromFloat64(3.14)
	neg := values.NewBigFloatFromFloat64(-2.5)
	intVal := values.NewBigFloatFromFloat64(7.0)

	c.Assert(pos.IsRational(), qt.IsTrue)
	c.Assert(pos.IsFinite(), qt.IsTrue)
	c.Assert(pos.IsNaN(), qt.IsFalse)
	c.Assert(pos.IsInteger(), qt.IsFalse)
	c.Assert(intVal.IsInteger(), qt.IsTrue)
	c.Assert(pos.Sign(), qt.Equals, 1)
	c.Assert(neg.Sign(), qt.Equals, -1)

	abs := neg.Abs()
	absF := abs.(*values.BigFloat)
	f, _ := absF.BigFloatValue().Float64()
	c.Assert(f, qt.Equals, 2.5)
}

// TestBigComplexPredicates covers IsInteger, IsRational, IsFinite, IsNaN,
// Abs, RealPart, and ImagPart for BigComplex.
func TestBigComplexPredicates(t *testing.T) {
	c := qt.New(t)

	bc := values.NewBigComplex(values.NewBigIntegerFromInt64(3), values.NewBigIntegerFromInt64(4))

	c.Assert(bc.IsFinite(), qt.IsTrue)
	c.Assert(bc.IsNaN(), qt.IsFalse)
	c.Assert(bc.IsInteger(), qt.IsFalse)
	c.Assert(bc.IsRational(), qt.IsFalse)

	// Real-only BigComplex
	bcReal := values.NewBigComplex(values.NewBigIntegerFromInt64(5), values.NewBigIntegerFromInt64(0))
	c.Assert(bcReal.IsInteger(), qt.IsTrue)
	c.Assert(bcReal.IsRational(), qt.IsTrue)
}

func TestBigComplexParts(t *testing.T) {
	c := qt.New(t)

	bc := values.NewBigComplex(values.NewBigIntegerFromInt64(3), values.NewBigIntegerFromInt64(4))

	realPart := bc.RealPart()
	c.Assert(realPart, valuestest.SchemeEquals, values.NewBigIntegerFromInt64(3))

	imagPart := bc.ImagPart()
	c.Assert(imagPart, valuestest.SchemeEquals, values.NewBigIntegerFromInt64(4))

	abs := bc.Abs()
	absF := abs.(*values.BigFloat)
	f, _ := absF.BigFloatValue().Float64()
	c.Assert(f, qt.Equals, 5.0)
}

// TestFloatToExactConversions covers the floatToExact utility function
// through Float.ToExact() with integer and non-integer float values.
func TestFloatToExactConversions(t *testing.T) {
	c := qt.New(t)

	// Float that is an integer -> BigInteger
	intFloat := values.NewFloat(42.0)
	exact := intFloat.ToExact()
	_, isBigInt := exact.(*values.BigInteger)
	c.Assert(isBigInt, qt.IsTrue)

	// Float that is not an integer -> Rational
	fracFloat := values.NewFloat(0.5)
	exactFrac := fracFloat.ToExact()
	_, isRat := exactFrac.(*values.Rational)
	c.Assert(isRat, qt.IsTrue)

	// Large float
	largeFloat := values.NewFloat(1e18)
	exactLarge := largeFloat.ToExact()
	c.Assert(exactLarge.IsExact(), qt.IsTrue)
}

// TestNumericTowerUtilities covers Simplify and ExactnessOf.
func TestNumericTowerUtilities(t *testing.T) {
	c := qt.New(t)

	// ExactnessOf
	c.Assert(values.ExactnessOf(values.NewInteger(1)), qt.Equals, values.Exact)
	c.Assert(values.ExactnessOf(values.NewFloat(1.0)), qt.Equals, values.Inexact)
	c.Assert(values.ExactnessOf(values.NewRational(1, 2)), qt.Equals, values.Exact)
	c.Assert(values.ExactnessOf(values.NewBigIntegerFromInt64(1)), qt.Equals, values.Exact)
	c.Assert(values.ExactnessOf(values.NewBigFloatFromFloat64(1.0)), qt.Equals, values.Inexact)
	c.Assert(values.ExactnessOf(values.NewComplexFromParts(1.0, 0.0)), qt.Equals, values.Inexact)

	// Simplify
	bigInt := values.NewBigIntegerFromInt64(42)
	simplified := values.Simplify(bigInt)
	_, isInt := simplified.(*values.Integer)
	c.Assert(isInt, qt.IsTrue)

	// Simplify a BigComplex with zero imaginary part
	bc := values.NewBigComplex(values.NewBigIntegerFromInt64(5), values.NewBigIntegerFromInt64(0))
	simplifiedBC := values.Simplify(bc)
	_, isInt2 := simplifiedBC.(*values.Integer)
	c.Assert(isInt2, qt.IsTrue)

	// Simplify a BigFloat that is integer-valued
	bf := values.NewBigFloat(new(big.Float).SetInt64(100))
	simplifiedBF := values.Simplify(bf)
	_, isInt3 := simplifiedBF.(*values.Integer)
	c.Assert(isInt3, qt.IsTrue)

	// Simplify a Rational that is integer-valued
	r := values.NewRational(6, 2)
	simplifiedR := values.Simplify(r)
	_, isInt4 := simplifiedR.(*values.Integer)
	c.Assert(isInt4, qt.IsTrue)
}
