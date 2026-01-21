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
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestBigComplex_Constructors(t *testing.T) {
	c := qt.New(t)

	// From BigIntegers (exact)
	bc1 := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(3),
		NewBigIntegerFromInt64(4),
	)
	c.Assert(bc1.Real().(*BigInteger).Int64(), qt.Equals, int64(3))
	c.Assert(bc1.Imag().(*BigInteger).Int64(), qt.Equals, int64(4))
	c.Assert(bc1.IsExact(), qt.IsTrue)

	// From BigFloats (inexact)
	bc2 := NewBigComplexFromBigFloats(
		NewBigFloatFromFloat64(1.5),
		NewBigFloatFromFloat64(2.5),
	)
	c.Assert(bc2.RealAsBigFloat().Float64(), qt.Equals, 1.5)
	c.Assert(bc2.ImagAsBigFloat().Float64(), qt.Equals, 2.5)
	c.Assert(bc2.IsExact(), qt.IsFalse)

	// Mixed parts (inexact due to BigFloat)
	bc3 := NewBigComplex(
		NewBigIntegerFromInt64(1),
		NewBigFloatFromFloat64(2.0),
	)
	c.Assert(bc3.IsExact(), qt.IsFalse)
}

func TestBigComplex_Arithmetic(t *testing.T) {
	c := qt.New(t)

	bc1 := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(3),
		NewBigIntegerFromInt64(4),
	)
	bc2 := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(1),
		NewBigIntegerFromInt64(2),
	)

	// Add: (3+4i) + (1+2i) = (4+6i)
	sum := bc1.Add(bc2)
	c.Assert(sum, qt.IsNotNil)
	bc := sum.(*BigComplex)
	c.Assert(bc.Real().(*BigInteger).Int64(), qt.Equals, int64(4))
	c.Assert(bc.Imag().(*BigInteger).Int64(), qt.Equals, int64(6))

	// Subtract: (3+4i) - (1+2i) = (2+2i)
	diff := bc1.Subtract(bc2)
	c.Assert(diff, qt.IsNotNil)
	bc = diff.(*BigComplex)
	c.Assert(bc.Real().(*BigInteger).Int64(), qt.Equals, int64(2))
	c.Assert(bc.Imag().(*BigInteger).Int64(), qt.Equals, int64(2))

	// Multiply: (3+4i) * (1+2i) = (3*1 - 4*2) + (3*2 + 4*1)i = -5 + 10i
	prod := bc1.Multiply(bc2)
	c.Assert(prod, qt.IsNotNil)
	bc = prod.(*BigComplex)
	c.Assert(bc.Real().(*BigInteger).Int64(), qt.Equals, int64(-5))
	c.Assert(bc.Imag().(*BigInteger).Int64(), qt.Equals, int64(10))

	// Negate
	neg := bc1.Negate()
	c.Assert(neg, qt.IsNotNil)
	bc = neg.(*BigComplex)
	c.Assert(bc.Real().(*BigInteger).Int64(), qt.Equals, int64(-3))
	c.Assert(bc.Imag().(*BigInteger).Int64(), qt.Equals, int64(-4))
}

func TestBigComplex_Division(t *testing.T) {
	c := qt.New(t)

	// (3+4i) / (1+2i) = ((3*1+4*2) + (4*1-3*2)i) / (1+4) = (11 - 2i) / 5 = 2.2 - 0.4i
	bc1 := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(3),
		NewBigIntegerFromInt64(4),
	)
	bc2 := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(1),
		NewBigIntegerFromInt64(2),
	)

	quot := bc1.Divide(bc2)
	c.Assert(quot, qt.IsNotNil)
	// Division always produces BigFloat parts
	realPart := quot.(*BigComplex).RealAsBigFloat().Float64()
	imagPart := quot.(*BigComplex).ImagAsBigFloat().Float64()
	c.Assert(math.Abs(realPart-2.2) < 0.0001, qt.IsTrue)
	c.Assert(math.Abs(imagPart-(-0.4)) < 0.0001, qt.IsTrue)

	// Division by zero returns nil
	zero := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(0),
		NewBigIntegerFromInt64(0),
	)
	c.Assert(bc1.Divide(zero), qt.IsNil)
}

func TestBigComplex_MixedArithmetic(t *testing.T) {
	c := qt.New(t)

	bc := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(3),
		NewBigIntegerFromInt64(4),
	)

	// Add with BigInteger: (3+4i) + 5 = (8+4i)
	sum1 := bc.Add(NewBigIntegerFromInt64(5))
	c.Assert(sum1.(*BigComplex).Real().(*BigInteger).Int64(), qt.Equals, int64(8))
	c.Assert(sum1.(*BigComplex).Imag().(*BigInteger).Int64(), qt.Equals, int64(4))

	// Add with BigFloat: (3+4i) + 1.5 = (4.5+4i) - becomes inexact
	sum2 := bc.Add(NewBigFloatFromFloat64(1.5))
	c.Assert(sum2.(*BigComplex).IsExact(), qt.IsFalse)
	c.Assert(sum2.(*BigComplex).RealAsBigFloat().Float64(), qt.Equals, 4.5)

	// Add with Integer: promotes Integer to BigInteger
	sum3 := bc.Add(NewInteger(10))
	c.Assert(sum3.(*BigComplex).Real().(*BigInteger).Int64(), qt.Equals, int64(13))

	// Add with Float: becomes inexact
	sum4 := bc.Add(NewFloat(2.5))
	c.Assert(sum4.(*BigComplex).IsExact(), qt.IsFalse)

	// Add with Complex
	cplx := NewComplexFromParts(1.0, 1.0)
	sum5 := bc.Add(cplx)
	c.Assert(sum5.(*BigComplex).RealAsBigFloat().Float64(), qt.Equals, 4.0)
	c.Assert(sum5.(*BigComplex).ImagAsBigFloat().Float64(), qt.Equals, 5.0)
}

func TestBigComplex_Exactness(t *testing.T) {
	c := qt.New(t)

	// Exact complex (both parts BigInteger)
	exact := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(3),
		NewBigIntegerFromInt64(4),
	)
	c.Assert(exact.IsExact(), qt.IsTrue)

	// Inexact complex (both parts BigFloat)
	inexact := NewBigComplexFromBigFloats(
		NewBigFloatFromFloat64(3.0),
		NewBigFloatFromFloat64(4.0),
	)
	c.Assert(inexact.IsExact(), qt.IsFalse)

	// Mixed (one BigInteger, one BigFloat) - inexact
	mixed := NewBigComplex(
		NewBigIntegerFromInt64(3),
		NewBigFloatFromFloat64(4.0),
	)
	c.Assert(mixed.IsExact(), qt.IsFalse)

	// ToInexact
	inexactFromExact := exact.ToInexact()
	c.Assert(inexactFromExact.(*BigComplex).IsExact(), qt.IsFalse)

	// ToExact
	exactFromInexact := inexact.ToExact()
	c.Assert(exactFromInexact.(*BigComplex).IsExact(), qt.IsTrue)
}

func TestBigComplex_Simplification(t *testing.T) {
	c := qt.New(t)

	// When imaginary part becomes 0, should simplify to real number
	bc1 := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(3),
		NewBigIntegerFromInt64(2),
	)
	bc2 := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(0),
		NewBigIntegerFromInt64(2),
	)

	// (3+2i) - (0+2i) = 3 (should simplify to BigInteger)
	result := bc1.Subtract(bc2)
	_, isBigInt := result.(*BigInteger)
	c.Assert(isBigInt, qt.IsTrue)
	c.Assert(result.(*BigInteger).Int64(), qt.Equals, int64(3))
}

func TestBigComplex_MagnitudePhase(t *testing.T) {
	c := qt.New(t)

	// 3+4i has magnitude 5 and phase atan2(4, 3)
	bc := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(3),
		NewBigIntegerFromInt64(4),
	)

	mag := bc.Magnitude()
	c.Assert(mag.Float64(), qt.Equals, 5.0)

	phase := bc.Phase()
	expected := math.Atan2(4, 3)
	c.Assert(math.Abs(phase.Float64()-expected) < 0.0001, qt.IsTrue)

	// Conjugate: (3+4i)* = 3-4i
	conj := bc.Conjugate()
	c.Assert(conj.Real().(*BigInteger).Int64(), qt.Equals, int64(3))
	c.Assert(conj.Imag().(*BigInteger).Int64(), qt.Equals, int64(-4))
}

func TestBigComplex_EqualTo(t *testing.T) {
	c := qt.New(t)

	bc1 := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(3),
		NewBigIntegerFromInt64(4),
	)
	bc2 := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(3),
		NewBigIntegerFromInt64(4),
	)
	bc3 := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(1),
		NewBigIntegerFromInt64(2),
	)

	c.Assert(bc1.EqualTo(bc2), qt.IsTrue)
	c.Assert(bc1.EqualTo(bc3), qt.IsFalse)

	// Equal to regular Complex with same values
	cplx := NewComplexFromParts(3.0, 4.0)
	c.Assert(bc1.EqualTo(cplx), qt.IsTrue)

	// Not equal to different type
	c.Assert(bc1.EqualTo(NewInteger(3)), qt.IsFalse)
}

func TestBigComplex_SchemeString(t *testing.T) {
	c := qt.New(t)

	// Positive imaginary
	bc1 := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(3),
		NewBigIntegerFromInt64(4),
	)
	c.Assert(bc1.SchemeString(), qt.Equals, "3+4i")

	// Negative imaginary
	bc2 := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(3),
		NewBigIntegerFromInt64(-4),
	)
	c.Assert(bc2.SchemeString(), qt.Equals, "3-4i")

	// With BigFloat parts
	bc3 := NewBigComplexFromBigFloats(
		NewBigFloatFromFloat64(1.5),
		NewBigFloatFromFloat64(2.5),
	)
	str := bc3.SchemeString()
	c.Assert(str, qt.Contains, "1.5")
	c.Assert(str, qt.Contains, "2.5")
	c.Assert(str, qt.Contains, "i")
}

func TestBigComplex_Properties(t *testing.T) {
	c := qt.New(t)

	bc := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(3),
		NewBigIntegerFromInt64(4),
	)
	zero := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(0),
		NewBigIntegerFromInt64(0),
	)
	realOnly := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(5),
		NewBigIntegerFromInt64(0),
	)

	c.Assert(bc.IsZero(), qt.IsFalse)
	c.Assert(zero.IsZero(), qt.IsTrue)
	c.Assert(bc.IsReal(), qt.IsFalse)
	c.Assert(realOnly.IsReal(), qt.IsTrue)
	c.Assert(bc.IsVoid(), qt.IsFalse)
	c.Assert((*BigComplex)(nil).IsVoid(), qt.IsTrue)
}

func TestBigComplex_Comparison(t *testing.T) {
	c := qt.New(t)

	bc1 := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(3),
		NewBigIntegerFromInt64(4),
	)
	bc2 := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(5),
		NewBigIntegerFromInt64(1),
	)
	bc3 := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(3),
		NewBigIntegerFromInt64(100),
	)

	// Compare by real parts only
	c.Assert(bc1.LessThan(bc2), qt.IsTrue)  // 3 < 5
	c.Assert(bc2.LessThan(bc1), qt.IsFalse) // 5 > 3
	c.Assert(bc1.LessThan(bc3), qt.IsFalse) // 3 == 3 (imaginary ignored)

	// Compare with other numeric types
	c.Assert(bc1.Compare(NewBigIntegerFromInt64(5)), qt.Equals, -1) // 3 < 5
	c.Assert(bc1.Compare(NewBigIntegerFromInt64(2)), qt.Equals, 1)  // 3 > 2
	c.Assert(bc1.Compare(NewBigIntegerFromInt64(3)), qt.Equals, 0)  // 3 == 3
}

func TestBigComplex_ZeroOptimizations(t *testing.T) {
	c := qt.New(t)

	bc := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(3),
		NewBigIntegerFromInt64(4),
	)
	zero := NewBigIntegerFromInt64(0)

	// Add zero returns self
	result := bc.Add(zero)
	c.Assert(result, qt.Equals, bc)

	// Subtract zero returns self
	result = bc.Subtract(zero)
	c.Assert(result, qt.Equals, bc)

	// Multiply by zero returns zero
	result = bc.Multiply(zero)
	c.Assert(result.IsZero(), qt.IsTrue)
}
