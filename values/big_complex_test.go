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

func TestBigComplex_Constructors(t *testing.T) {
	c := qt.New(t)

	// From BigIntegers (exact)
	bc1 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	c.Assert(bc1.Real().(*values.BigInteger).Int64(), qt.Equals, int64(3))
	c.Assert(bc1.Imag().(*values.BigInteger).Int64(), qt.Equals, int64(4))
	c.Assert(bc1.IsExact(), qt.IsTrue)

	// From BigFloats (inexact)
	bc2 := values.NewBigComplexFromBigFloats(
		values.NewBigFloatFromFloat64(1.5),
		values.NewBigFloatFromFloat64(2.5),
	)
	c.Assert(bc2.RealAsBigFloat().Float64(), qt.Equals, 1.5)
	c.Assert(bc2.ImagAsBigFloat().Float64(), qt.Equals, 2.5)
	c.Assert(bc2.IsExact(), qt.IsFalse)

	// Mixed parts (inexact due to BigFloat)
	bc3 := values.NewBigComplex(
		values.NewBigIntegerFromInt64(1),
		values.NewBigFloatFromFloat64(2.0),
	)
	c.Assert(bc3.IsExact(), qt.IsFalse)
}

func TestBigComplex_Arithmetic(t *testing.T) {
	c := qt.New(t)

	bc1 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	bc2 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(1),
		values.NewBigIntegerFromInt64(2),
	)

	// Add: (3+4i) + (1+2i) = (4+6i)
	sum := bc1.Add(bc2)
	c.Assert(sum, qt.IsNotNil)
	bc := sum.(*values.BigComplex)
	c.Assert(bc.Real().(*values.BigInteger).Int64(), qt.Equals, int64(4))
	c.Assert(bc.Imag().(*values.BigInteger).Int64(), qt.Equals, int64(6))

	// Subtract: (3+4i) - (1+2i) = (2+2i)
	diff := bc1.Subtract(bc2)
	c.Assert(diff, qt.IsNotNil)
	bc = diff.(*values.BigComplex)
	c.Assert(bc.Real().(*values.BigInteger).Int64(), qt.Equals, int64(2))
	c.Assert(bc.Imag().(*values.BigInteger).Int64(), qt.Equals, int64(2))

	// Multiply: (3+4i) * (1+2i) = (3*1 - 4*2) + (3*2 + 4*1)i = -5 + 10i
	prod := bc1.Multiply(bc2)
	c.Assert(prod, qt.IsNotNil)
	bc = prod.(*values.BigComplex)
	c.Assert(bc.Real().(*values.BigInteger).Int64(), qt.Equals, int64(-5))
	c.Assert(bc.Imag().(*values.BigInteger).Int64(), qt.Equals, int64(10))

	// Negate
	neg := bc1.Negate()
	c.Assert(neg, qt.IsNotNil)
	bc = neg.(*values.BigComplex)
	c.Assert(bc.Real().(*values.BigInteger).Int64(), qt.Equals, int64(-3))
	c.Assert(bc.Imag().(*values.BigInteger).Int64(), qt.Equals, int64(-4))
}

func TestBigComplex_Division(t *testing.T) {
	c := qt.New(t)

	// (3+4i) / (1+2i) = ((3*1+4*2) + (4*1-3*2)i) / (1+4) = (11 - 2i) / 5 = 2.2 - 0.4i
	bc1 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	bc2 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(1),
		values.NewBigIntegerFromInt64(2),
	)

	quot := bc1.Divide(bc2)
	c.Assert(quot, qt.IsNotNil)
	// Division always produces BigFloat parts
	realPart := quot.(*values.BigComplex).RealAsBigFloat().Float64()
	imagPart := quot.(*values.BigComplex).ImagAsBigFloat().Float64()
	c.Assert(math.Abs(realPart-2.2) < 0.0001, qt.IsTrue)
	c.Assert(math.Abs(imagPart-(-0.4)) < 0.0001, qt.IsTrue)

	// Division by zero panics
	zero := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(0),
		values.NewBigIntegerFromInt64(0),
	)
	c.Assert(func() { bc1.Divide(zero) }, qt.PanicMatches, "division by zero")

	// Division where zero parts produce *Integer intermediates via
	// multiplyResultForZero. (0+1i)/(0+1i): bc=0*0=Integer(0),
	// ad=0*1=Integer(0), so numerImag = Integer(0).Subtract(Integer(0))
	// = Integer(0). This Integer(0) reaches toBigFloat, which must handle
	// *Integer without panicking.
	bcPureImag := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(0),
		values.NewBigIntegerFromInt64(1),
	)
	quotImag := bcPureImag.Divide(bcPureImag)
	c.Assert(quotImag, qt.IsNotNil)
	// (0+1i)/(0+1i) = (0+1)/(0+1) + (0-0)i/(0+1) = 1+0i → simplifies to BigFloat(1)
	c.Assert(quotImag.(*values.BigFloat).Float64(), qt.Equals, 1.0)
}

func TestBigComplex_MixedArithmetic(t *testing.T) {
	c := qt.New(t)

	bc := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)

	// Add with BigInteger: (3+4i) + 5 = (8+4i)
	sum1 := bc.Add(values.NewBigIntegerFromInt64(5))
	c.Assert(sum1.(*values.BigComplex).Real().(*values.BigInteger).Int64(), qt.Equals, int64(8))
	c.Assert(sum1.(*values.BigComplex).Imag().(*values.BigInteger).Int64(), qt.Equals, int64(4))

	// Add with BigFloat: (3+4i) + 1.5 = (4.5+4i) - becomes inexact
	sum2 := bc.Add(values.NewBigFloatFromFloat64(1.5))
	c.Assert(sum2.(*values.BigComplex).IsExact(), qt.IsFalse)
	c.Assert(sum2.(*values.BigComplex).RealAsBigFloat().Float64(), qt.Equals, 4.5)

	// Add with Integer: promotes Integer to BigInteger
	sum3 := bc.Add(values.NewInteger(10))
	c.Assert(sum3.(*values.BigComplex).Real().(*values.BigInteger).Int64(), qt.Equals, int64(13))

	// Add with Float: becomes inexact
	sum4 := bc.Add(values.NewFloat(2.5))
	c.Assert(sum4.(*values.BigComplex).IsExact(), qt.IsFalse)

	// Add with Complex
	cplx := values.NewComplexFromParts(1.0, 1.0)
	sum5 := bc.Add(cplx)
	c.Assert(sum5.(*values.BigComplex).RealAsBigFloat().Float64(), qt.Equals, 4.0)
	c.Assert(sum5.(*values.BigComplex).ImagAsBigFloat().Float64(), qt.Equals, 5.0)
}

func TestBigComplex_Exactness(t *testing.T) {
	c := qt.New(t)

	// Exact complex (both parts BigInteger)
	exact := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	c.Assert(exact.IsExact(), qt.IsTrue)

	// Inexact complex (both parts BigFloat)
	inexact := values.NewBigComplexFromBigFloats(
		values.NewBigFloatFromFloat64(3.0),
		values.NewBigFloatFromFloat64(4.0),
	)
	c.Assert(inexact.IsExact(), qt.IsFalse)

	// Mixed (one BigInteger, one BigFloat) - inexact
	mixed := values.NewBigComplex(
		values.NewBigIntegerFromInt64(3),
		values.NewBigFloatFromFloat64(4.0),
	)
	c.Assert(mixed.IsExact(), qt.IsFalse)

	// ToInexact
	inexactFromExact := exact.ToInexact()
	c.Assert(inexactFromExact.(*values.BigComplex).IsExact(), qt.IsFalse)

	// ToExact
	exactFromInexact := inexact.ToExact()
	c.Assert(exactFromInexact.(*values.BigComplex).IsExact(), qt.IsTrue)
}

// TestBigComplex_ToExactFractionalParts tests H4 regression:
// BigComplex.ToExact() must convert fractional BigFloat parts to Rational,
// not truncate to BigInteger.
//
// Architectural Review H4: toExactPart converts BigFloat to integer by
// truncation instead of Rational. (exact 1.5+0i) produces 1 instead of 3/2.
func TestBigComplex_ToExactFractionalParts(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name             string
		real             float64
		imag             float64
		simplifiesToReal bool   // If true, result should be a Number (not BigComplex)
		wantRealType     string // "Rational", "BigInteger", or "Integer"
		wantRealValue    float64
		wantImagType     string
		wantImagValue    float64
	}{
		{
			name:             "fractional real, zero imag - simplifies to real",
			real:             1.5,
			imag:             0.0,
			simplifiesToReal: true,
			wantRealType:     "Rational",
			wantRealValue:    1.5,
		},
		{
			name:             "zero real, fractional imag - stays complex",
			real:             0.0,
			imag:             2.5,
			simplifiesToReal: false,
			wantRealType:     "zero",
			wantRealValue:    0.0,
			wantImagType:     "Rational",
			wantImagValue:    2.5,
		},
		{
			name:             "both parts fractional",
			real:             1.5,
			imag:             2.5,
			simplifiesToReal: false,
			wantRealType:     "Rational",
			wantRealValue:    1.5,
			wantImagType:     "Rational",
			wantImagValue:    2.5,
		},
		{
			name:             "integer-valued float and fractional",
			real:             3.0,
			imag:             2.5,
			simplifiesToReal: false,
			wantRealType:     "Integer", // 3.0 simplifies to integer
			wantRealValue:    3.0,
			wantImagType:     "Rational",
			wantImagValue:    2.5,
		},
		{
			name:             "fractional real, integer-valued imag",
			real:             1.5,
			imag:             4.0,
			simplifiesToReal: false,
			wantRealType:     "Rational",
			wantRealValue:    1.5,
			wantImagType:     "Integer",
			wantImagValue:    4.0,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			bc := values.NewBigComplexFromBigFloats(
				values.NewBigFloatFromFloat64(tc.real),
				values.NewBigFloatFromFloat64(tc.imag),
			)
			exact := bc.ToExact()
			c.Assert(exact.IsExact(), qt.IsTrue)

			// If result simplifies to real (imag == 0), check the real value directly
			if tc.simplifiesToReal {
				switch tc.wantRealType {
				case "Rational":
					rat, ok := exact.(*values.Rational)
					c.Assert(ok, qt.IsTrue, qt.Commentf("Result should be Rational, got %T: %v", exact, exact.SchemeString()))
					c.Assert(rat.Float64(), qt.Equals, tc.wantRealValue)
				case "Integer":
					switch v := exact.(type) {
					case *values.Integer:
						c.Assert(float64(v.Value), qt.Equals, tc.wantRealValue)
					case *values.BigInteger:
						c.Assert(float64(v.Int64()), qt.Equals, tc.wantRealValue)
					default:
						t.Fatalf("Result should be Integer or BigInteger, got %T: %v", exact, exact.SchemeString())
					}
				}
				return
			}

			// Otherwise, result should be BigComplex
			bcExact, ok := exact.(*values.BigComplex)
			c.Assert(ok, qt.IsTrue, qt.Commentf("Result should be BigComplex, got %T: %v", exact, exact.SchemeString()))

			// Check real part
			realPart := bcExact.Real()
			switch tc.wantRealType {
			case "zero":
				c.Assert(realPart.IsZero(), qt.IsTrue, qt.Commentf("Real part should be zero"))
			case "Rational":
				rat, ok := realPart.(*values.Rational)
				c.Assert(ok, qt.IsTrue, qt.Commentf("Real part should be Rational, got %T: %v", realPart, realPart.SchemeString()))
				c.Assert(rat.Float64(), qt.Equals, tc.wantRealValue)
			case "Integer":
				// Could be Integer or BigInteger depending on value
				switch v := realPart.(type) {
				case *values.Integer:
					c.Assert(float64(v.Value), qt.Equals, tc.wantRealValue)
				case *values.BigInteger:
					c.Assert(float64(v.Int64()), qt.Equals, tc.wantRealValue)
				default:
					t.Fatalf("Real part should be Integer or BigInteger, got %T: %v", realPart, realPart.SchemeString())
				}
			}

			// Check imaginary part
			imagPart := bcExact.Imag()
			switch tc.wantImagType {
			case "zero":
				c.Assert(imagPart.IsZero(), qt.IsTrue, qt.Commentf("Imaginary part should be zero"))
			case "Rational":
				rat, ok := imagPart.(*values.Rational)
				c.Assert(ok, qt.IsTrue, qt.Commentf("Imaginary part should be Rational, got %T: %v", imagPart, imagPart.SchemeString()))
				c.Assert(rat.Float64(), qt.Equals, tc.wantImagValue)
			case "Integer":
				switch v := imagPart.(type) {
				case *values.Integer:
					c.Assert(float64(v.Value), qt.Equals, tc.wantImagValue)
				case *values.BigInteger:
					c.Assert(float64(v.Int64()), qt.Equals, tc.wantImagValue)
				default:
					t.Fatalf("Imaginary part should be Integer or BigInteger, got %T: %v", imagPart, imagPart.SchemeString())
				}
			}
		})
	}
}

func TestBigComplex_Simplification(t *testing.T) {
	c := qt.New(t)

	// When imaginary part becomes 0, should simplify to real number
	bc1 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(2),
	)
	bc2 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(0),
		values.NewBigIntegerFromInt64(2),
	)

	// (3+2i) - (0+2i) = 3 (should simplify to BigInteger)
	result := bc1.Subtract(bc2)
	_, isBigInt := result.(*values.BigInteger)
	c.Assert(isBigInt, qt.IsTrue)
	c.Assert(result.(*values.BigInteger).Int64(), qt.Equals, int64(3))
}

func TestBigComplex_MagnitudePhase(t *testing.T) {
	c := qt.New(t)

	// 3+4i has magnitude 5 and phase atan2(4, 3)
	bc := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)

	mag := bc.Magnitude()
	c.Assert(mag.Float64(), qt.Equals, 5.0)

	phase := bc.Phase()
	expected := math.Atan2(4, 3)
	c.Assert(math.Abs(phase.Float64()-expected) < 0.0001, qt.IsTrue)

	// Conjugate: (3+4i)* = 3-4i
	conj := bc.Conjugate()
	c.Assert(conj.Real().(*values.BigInteger).Int64(), qt.Equals, int64(3))
	c.Assert(conj.Imag().(*values.BigInteger).Int64(), qt.Equals, int64(-4))
}

func TestBigComplex_EqualTo(t *testing.T) {
	c := qt.New(t)

	bc1 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	bc2 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	bc3 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(1),
		values.NewBigIntegerFromInt64(2),
	)

	c.Assert(bc1.EqualTo(bc2), qt.IsTrue)
	c.Assert(bc1.EqualTo(bc3), qt.IsFalse)

	// Equal to regular Complex with same values
	cplx := values.NewComplexFromParts(3.0, 4.0)
	c.Assert(bc1.EqualTo(cplx), qt.IsTrue)

	// Not equal to different type
	c.Assert(bc1.EqualTo(values.NewInteger(3)), qt.IsFalse)
}

func TestBigComplex_SchemeString(t *testing.T) {
	c := qt.New(t)

	// Positive imaginary
	bc1 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	c.Assert(bc1.SchemeString(), qt.Equals, "3+4i")

	// Negative imaginary
	bc2 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(-4),
	)
	c.Assert(bc2.SchemeString(), qt.Equals, "3-4i")

	// With BigFloat parts
	bc3 := values.NewBigComplexFromBigFloats(
		values.NewBigFloatFromFloat64(1.5),
		values.NewBigFloatFromFloat64(2.5),
	)
	str := bc3.SchemeString()
	c.Assert(str, qt.Contains, "1.5")
	c.Assert(str, qt.Contains, "2.5")
	c.Assert(str, qt.Contains, "i")
}

func TestBigComplex_Properties(t *testing.T) {
	c := qt.New(t)

	bc := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	zero := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(0),
		values.NewBigIntegerFromInt64(0),
	)
	realOnly := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(5),
		values.NewBigIntegerFromInt64(0),
	)

	c.Assert(bc.IsZero(), qt.IsFalse)
	c.Assert(zero.IsZero(), qt.IsTrue)
	c.Assert(bc.IsReal(), qt.IsFalse)
	c.Assert(realOnly.IsReal(), qt.IsTrue)
	c.Assert(bc.IsVoid(), qt.IsFalse)
	c.Assert((*values.BigComplex)(nil).IsVoid(), qt.IsTrue)
}

func TestBigComplex_Comparison(t *testing.T) {
	c := qt.New(t)

	bc1 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	bc2 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(5),
		values.NewBigIntegerFromInt64(1),
	)
	bc3 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(100),
	)

	// Compare by real parts only
	c.Assert(bc1.LessThan(bc2), qt.IsTrue)  // 3 < 5
	c.Assert(bc2.LessThan(bc1), qt.IsFalse) // 5 > 3
	c.Assert(bc1.LessThan(bc3), qt.IsFalse) // 3 == 3 (imaginary ignored)

	// Compare with other numeric types
	c.Assert(bc1.Compare(values.NewBigIntegerFromInt64(5)), qt.Equals, -1) // 3 < 5
	c.Assert(bc1.Compare(values.NewBigIntegerFromInt64(2)), qt.Equals, 1)  // 3 > 2
	c.Assert(bc1.Compare(values.NewBigIntegerFromInt64(3)), qt.Equals, 0)  // 3 == 3
}

func TestBigComplex_ZeroOptimizations(t *testing.T) {
	c := qt.New(t)

	bc := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	zero := values.NewBigIntegerFromInt64(0)

	// Add zero returns equal value (not necessarily same pointer)
	result := bc.Add(zero)
	c.Assert(result, valuestest.SchemeEquals, bc)

	// Subtract zero returns equal value (not necessarily same pointer)
	result = bc.Subtract(zero)
	c.Assert(result, valuestest.SchemeEquals, bc)

	// Multiply by zero returns zero
	result = bc.Multiply(zero)
	c.Assert(result.IsZero(), qt.IsTrue)
}

func TestBigComplex_RationalParts(t *testing.T) {
	c := qt.New(t)

	// Create BigComplex with Rational parts (exact)
	bc := values.NewBigComplex(
		values.NewRational(3, 2), // 3/2
		values.NewRational(1, 2), // 1/2
	)

	// Should be exact
	c.Assert(bc.IsExact(), qt.IsTrue)
	c.Assert(bc.Real().(*values.Rational).Float64(), qt.Equals, 1.5)
	c.Assert(bc.Imag().(*values.Rational).Float64(), qt.Equals, 0.5)

	// SchemeString should format correctly
	c.Assert(bc.SchemeString(), qt.Equals, "3/2+1/2i")

	// Negative imaginary
	bcNeg := values.NewBigComplex(
		values.NewRational(3, 2),
		values.NewRational(-1, 2),
	)
	c.Assert(bcNeg.SchemeString(), qt.Equals, "3/2-1/2i")
}

func TestBigComplex_RationalArithmetic(t *testing.T) {
	c := qt.New(t)

	bc1 := values.NewBigComplex(
		values.NewRational(3, 2), // 3/2
		values.NewRational(1, 2), // 1/2
	)
	bc2 := values.NewBigComplex(
		values.NewRational(1, 2), // 1/2
		values.NewRational(1, 4), // 1/4
	)

	// Add: (3/2 + 1/2i) + (1/2 + 1/4i) = (2 + 3/4i)
	sum := bc1.Add(bc2)
	c.Assert(sum.(*values.BigComplex).IsExact(), qt.IsTrue)
	realPart := sum.(*values.BigComplex).Real().(*values.Rational)
	imagPart := sum.(*values.BigComplex).Imag().(*values.Rational)
	c.Assert(realPart.Float64(), qt.Equals, 2.0)
	c.Assert(imagPart.Float64(), qt.Equals, 0.75)

	// Subtract: (3/2 + 1/2i) - (1/2 + 1/4i) = (1 + 1/4i)
	diff := bc1.Subtract(bc2)
	c.Assert(diff.(*values.BigComplex).IsExact(), qt.IsTrue)
	realPart = diff.(*values.BigComplex).Real().(*values.Rational)
	imagPart = diff.(*values.BigComplex).Imag().(*values.Rational)
	c.Assert(realPart.Float64(), qt.Equals, 1.0)
	c.Assert(imagPart.Float64(), qt.Equals, 0.25)

	// Multiply: (3/2 + 1/2i) * (1/2 + 1/4i) = (3/4 - 1/8) + (3/8 + 1/4)i = 5/8 + 5/8i
	prod := bc1.Multiply(bc2)
	c.Assert(prod.(*values.BigComplex).IsExact(), qt.IsTrue)
	realPart = prod.(*values.BigComplex).Real().(*values.Rational)
	imagPart = prod.(*values.BigComplex).Imag().(*values.Rational)
	c.Assert(realPart.Float64(), qt.Equals, 0.625) // 5/8
	c.Assert(imagPart.Float64(), qt.Equals, 0.625) // 5/8
}

func TestBigComplex_RationalWithScalar(t *testing.T) {
	c := qt.New(t)

	bc := values.NewBigComplex(
		values.NewRational(3, 2), // 3/2
		values.NewRational(1, 2), // 1/2
	)

	// Add Rational: (3/2 + 1/2i) + 1/4 = (7/4 + 1/2i)
	sum := bc.Add(values.NewRational(1, 4))
	c.Assert(sum.(*values.BigComplex).IsExact(), qt.IsTrue)
	realPart := sum.(*values.BigComplex).Real().(*values.Rational)
	c.Assert(realPart.Float64(), qt.Equals, 1.75) // 7/4

	// Multiply Rational: (3/2 + 1/2i) * 2 = (3 + 1i)
	prod := bc.Multiply(values.NewRational(2, 1))
	c.Assert(prod.(*values.BigComplex).IsExact(), qt.IsTrue)

	// Divide by Rational: (3/2 + 1/2i) / (1/2) = (3 + 1i)
	quot := bc.Divide(values.NewRational(1, 2))
	c.Assert(quot.(*values.BigComplex).IsExact(), qt.IsTrue)
	realPart = quot.(*values.BigComplex).Real().(*values.Rational)
	imagPart := quot.(*values.BigComplex).Imag().(*values.Rational)
	c.Assert(realPart.Float64(), qt.Equals, 3.0)
	c.Assert(imagPart.Float64(), qt.Equals, 1.0)
}

func TestBigComplex_RationalExactnessContagion(t *testing.T) {
	c := qt.New(t)

	exact := values.NewBigComplex(
		values.NewRational(3, 2),
		values.NewRational(1, 2),
	)

	// Adding a Float makes it inexact
	sumFloat := exact.Add(values.NewFloat(1.0))
	c.Assert(sumFloat.(*values.BigComplex).IsExact(), qt.IsFalse)

	// Adding a BigFloat makes it inexact
	sumBigFloat := exact.Add(values.NewBigFloatFromFloat64(1.0))
	c.Assert(sumBigFloat.(*values.BigComplex).IsExact(), qt.IsFalse)

	// Adding an Integer keeps it exact (Integer promotes to BigInteger)
	sumInt := exact.Add(values.NewInteger(1))
	c.Assert(sumInt.(*values.BigComplex).IsExact(), qt.IsTrue)

	// Adding a BigInteger keeps it exact
	sumBigInt := exact.Add(values.NewBigIntegerFromInt64(1))
	c.Assert(sumBigInt.(*values.BigComplex).IsExact(), qt.IsTrue)

	// Adding another exact Rational keeps it exact
	sumRat := exact.Add(values.NewRational(1, 4))
	c.Assert(sumRat.(*values.BigComplex).IsExact(), qt.IsTrue)
}

func TestBigComplex_MixedRationalBigInteger(t *testing.T) {
	c := qt.New(t)

	// Mixed: Rational real, BigInteger imag
	bc := values.NewBigComplex(
		values.NewRational(3, 2),
		values.NewBigIntegerFromInt64(4),
	)

	// Should still be exact
	c.Assert(bc.IsExact(), qt.IsTrue)

	// Add another mixed
	bc2 := values.NewBigComplex(
		values.NewBigIntegerFromInt64(1),
		values.NewRational(1, 2),
	)
	sum := bc.Add(bc2)
	c.Assert(sum.(*values.BigComplex).IsExact(), qt.IsTrue)
}

func TestBigComplex_HashCode(t *testing.T) {
	c := qt.New(t)

	// Stability: same value produces same hash.
	bc1 := values.NewBigComplexFromBigFloats(
		values.NewBigFloatFromFloat64(3.0),
		values.NewBigFloatFromFloat64(4.0),
	)
	bc2 := values.NewBigComplexFromBigFloats(
		values.NewBigFloatFromFloat64(3.0),
		values.NewBigFloatFromFloat64(4.0),
	)
	c.Assert(bc1.HashCode(), qt.Equals, bc2.HashCode())

	// Exact (BigInteger) parts: stability.
	be1 := values.NewBigComplex(values.NewBigIntegerFromInt64(1), values.NewBigIntegerFromInt64(2))
	be2 := values.NewBigComplex(values.NewBigIntegerFromInt64(1), values.NewBigIntegerFromInt64(2))
	c.Assert(be1.HashCode(), qt.Equals, be2.HashCode())

	// Distinctness: different values hash differently.
	h1 := values.NewBigComplexFromBigFloats(values.NewBigFloatFromFloat64(1), values.NewBigFloatFromFloat64(2)).HashCode()
	h2 := values.NewBigComplexFromBigFloats(values.NewBigFloatFromFloat64(2), values.NewBigFloatFromFloat64(1)).HashCode()
	c.Assert(h1, qt.Not(qt.Equals), h2)
}

// TestBigComplex_FloatInfDispatch is the regression test for issue #362.
// Float(Inf/NaN) combined with BigComplex must preserve the imaginary part.
func TestBigComplex_FloatInfDispatch(t *testing.T) {
	c := qt.New(t)

	// BigComplex(3+4i) with exact BigInteger parts.
	bc := values.NewBigComplex(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)

	tcs := []struct {
		name     string
		result   values.Number
		realInf  bool
		realNaN  bool
		imagZero bool
	}{
		{
			name:     "+inf.0 + (3+4i)",
			result:   values.NewFloat(math.Inf(1)).Add(bc),
			realInf:  true,
			realNaN:  false,
			imagZero: false,
		},
		{
			name:     "(3+4i) + +inf.0",
			result:   bc.Add(values.NewFloat(math.Inf(1))),
			realInf:  true,
			realNaN:  false,
			imagZero: false,
		},
		{
			name:     "-inf.0 + (3+4i)",
			result:   values.NewFloat(math.Inf(-1)).Add(bc),
			realInf:  true,
			realNaN:  false,
			imagZero: false,
		},
		{
			name:     "+nan.0 + (3+4i)",
			result:   values.NewFloat(math.NaN()).Add(bc),
			realInf:  false,
			realNaN:  true,
			imagZero: false,
		},
		{
			name:     "+inf.0 - (3+4i)",
			result:   values.NewFloat(math.Inf(1)).Subtract(bc),
			realInf:  true,
			realNaN:  false,
			imagZero: false,
		},
		{
			name:     "(3+4i) - +inf.0",
			result:   bc.Subtract(values.NewFloat(math.Inf(1))),
			realInf:  true,
			realNaN:  false,
			imagZero: false,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// Result must be *BigComplex (not *Float or *Complex).
			bResult, ok := tc.result.(*values.BigComplex)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected *BigComplex, got %T", tc.result))

			realPart := bResult.Real()
			imagPart := bResult.Imag()

			// Imaginary part must not be zero (the original bug: imaginary lost).
			c.Assert(imagPart.IsZero(), qt.IsFalse)

			if tc.realInf {
				c.Assert(realPart.IsFinite(), qt.IsFalse)
				c.Assert(realPart.IsNaN(), qt.IsFalse)
			}
			if tc.realNaN {
				c.Assert(realPart.IsNaN(), qt.IsTrue)
			}
		})
	}
}

func TestBigComplex_InfNaNPredicates(t *testing.T) {
	c := qt.New(t)

	infReal := values.NewBigComplex(
		values.NewBigFloat(new(big.Float).SetInf(false)),
		values.NewBigIntegerFromInt64(4),
	)
	nanReal := values.NewBigComplex(
		values.NewBigFloatNaN(),
		values.NewBigIntegerFromInt64(4),
	)
	finite := values.NewBigComplex(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)

	// BigComplex with Inf real part.
	c.Assert(infReal.IsFinite(), qt.IsFalse)
	c.Assert(infReal.IsNaN(), qt.IsFalse)
	c.Assert(infReal.IsRational(), qt.IsFalse)

	// BigComplex with NaN real part.
	c.Assert(nanReal.IsFinite(), qt.IsFalse)
	c.Assert(nanReal.IsNaN(), qt.IsTrue)
	c.Assert(nanReal.IsRational(), qt.IsFalse)

	// Ordinary finite BigComplex.
	c.Assert(finite.IsFinite(), qt.IsTrue)
	c.Assert(finite.IsNaN(), qt.IsFalse)
}

// TestBigComplex_NaNEquality verifies IEEE 754: NaN is never equal to
// anything, including itself. This is a regression test for a bug where
// BigComplex.EqualTo fell through to Compare (which returns 0 for NaN),
// causing NaN == NaN to return true.
func TestBigComplex_NaNEquality(t *testing.T) {
	c := qt.New(t)

	nanReal := values.NewBigComplex(
		values.NewBigFloatNaN(),
		values.NewBigIntegerFromInt64(4),
	)
	nanImag := values.NewBigComplex(
		values.NewBigIntegerFromInt64(3),
		values.NewBigFloatNaN(),
	)
	nanBoth := values.NewBigComplex(
		values.NewBigFloatNaN(),
		values.NewBigFloatNaN(),
	)
	finite := values.NewBigComplex(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)

	// NaN != NaN (IEEE 754).
	c.Assert(nanReal.EqualTo(nanReal), qt.IsFalse)
	c.Assert(nanImag.EqualTo(nanImag), qt.IsFalse)
	c.Assert(nanBoth.EqualTo(nanBoth), qt.IsFalse)

	// NaN != finite.
	c.Assert(nanReal.EqualTo(finite), qt.IsFalse)
	c.Assert(finite.EqualTo(nanReal), qt.IsFalse)

	// NaN BigComplex != NaN BigComplex (different instances).
	nanReal2 := values.NewBigComplex(
		values.NewBigFloatNaN(),
		values.NewBigIntegerFromInt64(4),
	)
	c.Assert(nanReal.EqualTo(nanReal2), qt.IsFalse)
}
