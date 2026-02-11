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

package values

import (
	"math"
	"math/big"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestIntegerPredicates covers IsInteger, IsRational, IsFinite, IsNaN, IsPositive,
// IsNegative, Sign, ToExact, and ToInexact for Integer.
func TestIntegerPredicates(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name       string
		val        *Integer
		isPositive bool
		isNegative bool
		sign       int
	}{
		{"positive", NewInteger(42), true, false, 1},
		{"negative", NewInteger(-7), false, true, -1},
		{"zero", NewInteger(0), false, false, 0},
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
	i := NewInteger(5)

	exact := i.ToExact()
	c.Assert(exact, SchemeEquals, i)

	inexact := i.ToInexact()
	f, ok := inexact.(*Float)
	c.Assert(ok, qt.IsTrue)
	c.Assert(f.Value, qt.Equals, 5.0)
}

func TestIntegerCompare(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		a    *Integer
		b    Number
		want int
	}{
		{"int<int", NewInteger(1), NewInteger(2), -1},
		{"int=int", NewInteger(5), NewInteger(5), 0},
		{"int>int", NewInteger(9), NewInteger(3), 1},
		{"int<float", NewInteger(1), NewFloat(1.5), -1},
		{"int=float", NewInteger(2), NewFloat(2.0), 0},
		{"int>float", NewInteger(3), NewFloat(2.5), 1},
		{"int<rational", NewInteger(1), NewRational(3, 2), -1},
		{"int<bigint", NewInteger(1), NewBigIntegerFromInt64(2), -1},
		{"int<bigfloat", NewInteger(1), NewBigFloatFromFloat64(1.5), -1},
		{"int<complex", NewInteger(1), NewComplexFromParts(2.0, 0.0), -1},
		{"int=complex", NewInteger(2), NewComplexFromParts(2.0, 0.0), 0},
		{"int<bigcomplex", NewInteger(1), NewBigComplex(NewBigIntegerFromInt64(2), NewBigIntegerFromInt64(0)), -1},
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
		val        *Float
		isInteger  bool
		isRational bool
		isFinite   bool
		isNaN      bool
		isPositive bool
		isNegative bool
		sign       int
	}{
		{"positive", NewFloat(3.14), false, true, true, false, true, false, 1},
		{"negative", NewFloat(-2.5), false, true, true, false, false, true, -1},
		{"zero", NewFloat(0.0), true, true, true, false, false, false, 0},
		{"integer value", NewFloat(7.0), true, true, true, false, true, false, 1},
		{"+inf", NewFloat(math.Inf(1)), false, false, false, false, true, false, 1},
		{"-inf", NewFloat(math.Inf(-1)), false, false, false, false, false, true, -1},
		{"NaN", NewFloat(math.NaN()), false, false, false, true, false, false, 0},
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

	f := NewFloat(-3.5)
	abs := f.Abs()
	c.Assert(abs, SchemeEquals, NewFloat(3.5))

	neg := f.Negate()
	c.Assert(neg, SchemeEquals, NewFloat(3.5))
}

func TestFloatExactness(t *testing.T) {
	c := qt.New(t)

	f := NewFloat(2.5)
	exact := f.ToExact()
	c.Assert(exact.IsExact(), qt.IsTrue)

	inexact := f.ToInexact()
	c.Assert(inexact, SchemeEquals, f)
}

func TestFloatHashCode(t *testing.T) {
	c := qt.New(t)
	f1 := NewFloat(3.14)
	f2 := NewFloat(3.14)
	c.Assert(f1.HashCode(), qt.Equals, f2.HashCode())
}

func TestFloatCompare(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		a    *Float
		b    Number
		want int
	}{
		{"float<float", NewFloat(1.0), NewFloat(2.0), -1},
		{"float=float", NewFloat(5.0), NewFloat(5.0), 0},
		{"float>float", NewFloat(9.0), NewFloat(3.0), 1},
		{"float<int", NewFloat(1.0), NewInteger(2), -1},
		{"float<bigint", NewFloat(1.0), NewBigIntegerFromInt64(2), -1},
		{"float<bigfloat", NewFloat(1.0), NewBigFloatFromFloat64(2.0), -1},
		{"float<rational", NewFloat(1.0), NewRational(3, 2), -1},
		{"float<complex", NewFloat(1.0), NewComplexFromParts(2.0, 0.0), -1},
		{"float=complex", NewFloat(2.0), NewComplexFromParts(2.0, 3.0), 0},
		{"float<bigcomplex", NewFloat(1.0), NewBigComplex(NewBigIntegerFromInt64(2), NewBigIntegerFromInt64(0)), -1},
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
		val        *Complex
		isInteger  bool
		isRational bool
		isFinite   bool
		isNaN      bool
	}{
		{"standard", NewComplexFromParts(1.0, 2.0), false, false, true, false},
		{"real integer", NewComplexFromParts(5.0, 0.0), true, false, true, false},
		{"with inf", NewComplexFromParts(math.Inf(1), 0.0), false, false, false, false},
		{"with NaN real", NewComplexFromParts(math.NaN(), 0.0), false, false, false, true},
		{"with NaN imag", NewComplexFromParts(0.0, math.NaN()), false, false, false, true},
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

	z := NewComplexFromParts(3.0, 4.0)

	neg := z.Negate()
	negC := neg.(*Complex)
	c.Assert(real(negC.Value), qt.Equals, -3.0)
	c.Assert(imag(negC.Value), qt.Equals, -4.0)

	abs := z.Abs()
	absF := abs.(*Float)
	c.Assert(absF.Value, qt.Equals, 5.0)

	realPart := z.RealPart()
	c.Assert(realPart, SchemeEquals, NewFloat(3.0))

	imagPart := z.ImagPart()
	c.Assert(imagPart, SchemeEquals, NewFloat(4.0))
}

func TestComplexExactness(t *testing.T) {
	c := qt.New(t)

	z := NewComplexFromParts(2.0, 3.0)

	exact := z.ToExact()
	c.Assert(exact.IsExact(), qt.IsTrue)

	inexact := z.ToInexact()
	c.Assert(inexact, SchemeEquals, z)
}

func TestComplexCompare(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		a    *Complex
		b    Number
		want int
	}{
		{"complex<complex", NewComplexFromParts(1.0, 0.0), NewComplexFromParts(2.0, 0.0), -1},
		{"complex=complex", NewComplexFromParts(5.0, 1.0), NewComplexFromParts(5.0, 2.0), 0},
		{"complex<float", NewComplexFromParts(1.0, 0.0), NewFloat(2.0), -1},
		{"complex<int", NewComplexFromParts(1.0, 0.0), NewInteger(2), -1},
		{"complex<bigint", NewComplexFromParts(1.0, 0.0), NewBigIntegerFromInt64(2), -1},
		{"complex<bigfloat", NewComplexFromParts(1.0, 0.0), NewBigFloatFromFloat64(2.0), -1},
		{"complex<rational", NewComplexFromParts(1.0, 0.0), NewRational(3, 1), -1},
		{"complex<bigcomplex", NewComplexFromParts(1.0, 0.0), NewBigComplex(NewBigIntegerFromInt64(2), NewBigIntegerFromInt64(0)), -1},
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
		val        *Rational
		isPositive bool
		isNegative bool
		sign       int
	}{
		{"positive", NewRational(3, 4), true, false, 1},
		{"negative", NewRational(-1, 3), false, true, -1},
		{"zero", NewRational(0, 1), false, false, 0},
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

	r := NewRational(-3, 4)
	abs := r.Abs()
	c.Assert(abs.(*Rational).value.Sign(), qt.Equals, 1)

	neg := r.Negate()
	c.Assert(neg.(*Rational).value.Sign(), qt.Equals, 1)
}

func TestRationalExactness(t *testing.T) {
	c := qt.New(t)

	r := NewRational(1, 3)
	exact := r.ToExact()
	c.Assert(exact, SchemeEquals, r)

	inexact := r.ToInexact()
	f, ok := inexact.(*Float)
	c.Assert(ok, qt.IsTrue)
	c.Assert(f.Value > 0.33, qt.IsTrue)
}

func TestRationalHashCode(t *testing.T) {
	c := qt.New(t)
	r1 := NewRational(1, 2)
	r2 := NewRational(1, 2)
	c.Assert(r1.HashCode(), qt.Equals, r2.HashCode())
}

func TestRationalCompare(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		a    *Rational
		b    Number
		want int
	}{
		{"rat<rat", NewRational(1, 4), NewRational(1, 2), -1},
		{"rat=rat", NewRational(1, 2), NewRational(1, 2), 0},
		{"rat>rat", NewRational(3, 4), NewRational(1, 4), 1},
		{"rat<int", NewRational(1, 2), NewInteger(1), -1},
		{"rat<bigint", NewRational(1, 2), NewBigIntegerFromInt64(1), -1},
		{"rat<float", NewRational(1, 2), NewFloat(0.75), -1},
		{"rat<bigfloat", NewRational(1, 2), NewBigFloatFromFloat64(0.75), -1},
		{"rat<complex", NewRational(1, 2), NewComplexFromParts(1.0, 0.0), -1},
		{"rat<bigcomplex", NewRational(1, 2), NewBigComplex(NewBigIntegerFromInt64(1), NewBigIntegerFromInt64(0)), -1},
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

	pos := NewBigIntegerFromInt64(42)
	neg := NewBigIntegerFromInt64(-7)
	zero := NewBigIntegerFromInt64(0)

	c.Assert(pos.IsInteger(), qt.IsTrue)
	c.Assert(pos.IsRational(), qt.IsTrue)
	c.Assert(pos.IsFinite(), qt.IsTrue)
	c.Assert(pos.IsNaN(), qt.IsFalse)
	c.Assert(pos.Sign(), qt.Equals, 1)
	c.Assert(neg.Sign(), qt.Equals, -1)
	c.Assert(zero.Sign(), qt.Equals, 0)

	abs := neg.Abs()
	c.Assert(abs.(*BigInteger).value.Int64(), qt.Equals, int64(7))
}

func TestBigIntegerHashCode(t *testing.T) {
	c := qt.New(t)
	b1 := NewBigIntegerFromInt64(999)
	b2 := NewBigIntegerFromInt64(999)
	c.Assert(b1.HashCode(), qt.Equals, b2.HashCode())
}

// TestBigFloatPredicates covers IsInteger, IsRational, IsFinite, IsNaN,
// Abs, and Sign for BigFloat.
func TestBigFloatPredicates(t *testing.T) {
	c := qt.New(t)

	pos := NewBigFloatFromFloat64(3.14)
	neg := NewBigFloatFromFloat64(-2.5)
	intVal := NewBigFloatFromFloat64(7.0)

	c.Assert(pos.IsRational(), qt.IsTrue)
	c.Assert(pos.IsFinite(), qt.IsTrue)
	c.Assert(pos.IsNaN(), qt.IsFalse)
	c.Assert(pos.IsInteger(), qt.IsFalse)
	c.Assert(intVal.IsInteger(), qt.IsTrue)
	c.Assert(pos.Sign(), qt.Equals, 1)
	c.Assert(neg.Sign(), qt.Equals, -1)

	abs := neg.Abs()
	absF := abs.(*BigFloat)
	f, _ := absF.BigFloatValue().Float64()
	c.Assert(f, qt.Equals, 2.5)
}

// TestBigComplexPredicates covers IsInteger, IsRational, IsFinite, IsNaN,
// Abs, RealPart, and ImagPart for BigComplex.
func TestBigComplexPredicates(t *testing.T) {
	c := qt.New(t)

	bc := NewBigComplex(NewBigIntegerFromInt64(3), NewBigIntegerFromInt64(4))

	c.Assert(bc.IsFinite(), qt.IsTrue)
	c.Assert(bc.IsNaN(), qt.IsFalse)
	c.Assert(bc.IsInteger(), qt.IsFalse)
	c.Assert(bc.IsRational(), qt.IsFalse)

	// Real-only BigComplex
	bcReal := NewBigComplex(NewBigIntegerFromInt64(5), NewBigIntegerFromInt64(0))
	c.Assert(bcReal.IsInteger(), qt.IsTrue)
	c.Assert(bcReal.IsRational(), qt.IsTrue)
}

func TestBigComplexParts(t *testing.T) {
	c := qt.New(t)

	bc := NewBigComplex(NewBigIntegerFromInt64(3), NewBigIntegerFromInt64(4))

	realPart := bc.RealPart()
	c.Assert(realPart, SchemeEquals, NewBigIntegerFromInt64(3))

	imagPart := bc.ImagPart()
	c.Assert(imagPart, SchemeEquals, NewBigIntegerFromInt64(4))

	abs := bc.Abs()
	absF := abs.(*BigFloat)
	f, _ := absF.BigFloatValue().Float64()
	c.Assert(f, qt.Equals, 5.0)
}

// TestFloatToExactConversions covers the floatToExact utility function
// through Float.ToExact() with integer and non-integer float values.
func TestFloatToExactConversions(t *testing.T) {
	c := qt.New(t)

	// Float that is an integer -> BigInteger
	intFloat := NewFloat(42.0)
	exact := intFloat.ToExact()
	_, isBigInt := exact.(*BigInteger)
	c.Assert(isBigInt, qt.IsTrue)

	// Float that is not an integer -> Rational
	fracFloat := NewFloat(0.5)
	exactFrac := fracFloat.ToExact()
	_, isRat := exactFrac.(*Rational)
	c.Assert(isRat, qt.IsTrue)

	// Large float
	largeFloat := NewFloat(1e18)
	exactLarge := largeFloat.ToExact()
	c.Assert(exactLarge.IsExact(), qt.IsTrue)
}

// TestNumericTowerUtilities covers Simplify and ExactnessOf.
func TestNumericTowerUtilities(t *testing.T) {
	c := qt.New(t)

	// ExactnessOf
	c.Assert(ExactnessOf(NewInteger(1)), qt.Equals, Exact)
	c.Assert(ExactnessOf(NewFloat(1.0)), qt.Equals, Inexact)
	c.Assert(ExactnessOf(NewRational(1, 2)), qt.Equals, Exact)
	c.Assert(ExactnessOf(NewBigIntegerFromInt64(1)), qt.Equals, Exact)
	c.Assert(ExactnessOf(NewBigFloatFromFloat64(1.0)), qt.Equals, Inexact)
	c.Assert(ExactnessOf(NewComplexFromParts(1.0, 0.0)), qt.Equals, Inexact)

	// Simplify
	bigInt := NewBigIntegerFromInt64(42)
	simplified := Simplify(bigInt)
	_, isInt := simplified.(*Integer)
	c.Assert(isInt, qt.IsTrue)

	// Simplify a BigComplex with zero imaginary part
	bc := NewBigComplex(NewBigIntegerFromInt64(5), NewBigIntegerFromInt64(0))
	simplifiedBC := Simplify(bc)
	_, isInt2 := simplifiedBC.(*Integer)
	c.Assert(isInt2, qt.IsTrue)

	// Simplify a BigFloat that is integer-valued
	bf := NewBigFloat(new(big.Float).SetInt64(100))
	simplifiedBF := Simplify(bf)
	_, isInt3 := simplifiedBF.(*Integer)
	c.Assert(isInt3, qt.IsTrue)

	// Simplify a Rational that is integer-valued
	r := NewRational(6, 2)
	simplifiedR := Simplify(r)
	_, isInt4 := simplifiedR.(*Integer)
	c.Assert(isInt4, qt.IsTrue)
}
