// Copyright 2025 Aaron Alpar
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
	"math/big"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestBigInteger_Constructors(t *testing.T) {
	c := qt.New(t)

	// From int64
	bi1 := NewBigIntegerFromInt64(42)
	c.Assert(bi1.Int64(), qt.Equals, int64(42))

	// From string
	bi2 := NewBigIntegerFromString("12345678901234567890", 10)
	c.Assert(bi2, qt.IsNotNil)
	c.Assert(bi2.BigInt().String(), qt.Equals, "12345678901234567890")

	// From big.Int
	bigVal := big.NewInt(99)
	bi3 := NewBigInteger(bigVal)
	c.Assert(bi3.Int64(), qt.Equals, int64(99))

	// Invalid string returns nil
	bi4 := NewBigIntegerFromString("invalid", 10)
	c.Assert(bi4, qt.IsNil)
}

func TestBigInteger_Arithmetic(t *testing.T) {
	c := qt.New(t)

	bi1 := NewBigIntegerFromInt64(100)
	bi2 := NewBigIntegerFromInt64(50)

	// Add
	sum := bi1.Add(bi2)
	c.Assert(sum.(*BigInteger).Int64(), qt.Equals, int64(150))

	// Subtract
	diff := bi1.Subtract(bi2)
	c.Assert(diff.(*BigInteger).Int64(), qt.Equals, int64(50))

	// Multiply
	prod := bi1.Multiply(bi2)
	c.Assert(prod.(*BigInteger).Int64(), qt.Equals, int64(5000))

	// Divide (returns Rational for exact division)
	quot := bi1.Divide(bi2)
	c.Assert(quot, qt.IsNotNil)

	// Negate
	neg := bi1.Negate()
	c.Assert(neg.(*BigInteger).Int64(), qt.Equals, int64(-100))
}

func TestBigInteger_Comparison(t *testing.T) {
	c := qt.New(t)

	bi1 := NewBigIntegerFromInt64(100)
	bi2 := NewBigIntegerFromInt64(50)
	bi3 := NewBigIntegerFromInt64(100)

	c.Assert(bi1.Compare(bi2), qt.Equals, 1)
	c.Assert(bi2.Compare(bi1), qt.Equals, -1)
	c.Assert(bi1.Compare(bi3), qt.Equals, 0)

	c.Assert(bi2.LessThan(bi1), qt.IsTrue)
	c.Assert(bi1.LessThan(bi2), qt.IsFalse)
}

func TestBigInteger_Properties(t *testing.T) {
	c := qt.New(t)

	positive := NewBigIntegerFromInt64(42)
	negative := NewBigIntegerFromInt64(-42)
	zero := NewBigIntegerFromInt64(0)

	c.Assert(positive.IsPositive(), qt.IsTrue)
	c.Assert(positive.IsNegative(), qt.IsFalse)
	c.Assert(positive.IsZero(), qt.IsFalse)

	c.Assert(negative.IsPositive(), qt.IsFalse)
	c.Assert(negative.IsNegative(), qt.IsTrue)
	c.Assert(negative.IsZero(), qt.IsFalse)

	c.Assert(zero.IsPositive(), qt.IsFalse)
	c.Assert(zero.IsNegative(), qt.IsFalse)
	c.Assert(zero.IsZero(), qt.IsTrue)

	c.Assert(positive.IsExact(), qt.IsTrue)
}

func TestBigInteger_Conversions(t *testing.T) {
	c := qt.New(t)

	bi := NewBigIntegerFromInt64(42)

	// ToExact should return itself
	c.Assert(bi.ToExact(), qt.Equals, bi)

	// ToInexact should return Float
	inexact := bi.ToInexact()
	f, ok := inexact.(*Float)
	c.Assert(ok, qt.IsTrue)
	c.Assert(f.Value, qt.Equals, float64(42))

	// SchemeString
	c.Assert(bi.SchemeString(), qt.Equals, "42")
}

func TestBigInteger_EqualTo(t *testing.T) {
	c := qt.New(t)

	bi1 := NewBigIntegerFromInt64(42)
	bi2 := NewBigIntegerFromInt64(42)
	bi3 := NewBigIntegerFromInt64(99)
	int1 := NewInteger(42)

	c.Assert(bi1.EqualTo(bi2), qt.IsTrue)
	c.Assert(bi1.EqualTo(bi3), qt.IsFalse)
	c.Assert(bi1.EqualTo(int1), qt.IsTrue) // Should equal regular Integer
	c.Assert(bi1.EqualTo(NewFloat(42.0)), qt.IsFalse)
}

func TestBigInteger_MixedArithmetic(t *testing.T) {
	c := qt.New(t)

	bi := NewBigIntegerFromInt64(100)

	// Add with Integer
	sum := bi.Add(NewInteger(50))
	c.Assert(sum.(*BigInteger).Int64(), qt.Equals, int64(150))

	// Add with Float
	sumF := bi.Add(NewFloat(0.5))
	c.Assert(sumF.(*Float).Value, qt.Equals, float64(100.5))

	// Add with Complex
	sumC := bi.Add(NewComplex(complex(1, 2)))
	comp, ok := sumC.(*Complex)
	c.Assert(ok, qt.IsTrue)
	c.Assert(real(comp.Datum()), qt.Equals, float64(101))
}

func TestBigFloat_Constructors(t *testing.T) {
	c := qt.New(t)

	// From float64
	bf1 := NewBigFloatFromFloat64(3.14)
	c.Assert(bf1.Float64(), qt.Equals, float64(3.14))

	// From string
	bf2 := NewBigFloatFromString("3.14159265358979323846")
	c.Assert(bf2, qt.IsNotNil)

	// From big.Float
	bigVal := big.NewFloat(2.71)
	bf3 := NewBigFloat(bigVal)
	c.Assert(bf3.Float64(), qt.Equals, float64(2.71))

	// Invalid string returns nil
	bf4 := NewBigFloatFromString("invalid")
	c.Assert(bf4, qt.IsNil)
}

func TestBigFloat_Arithmetic(t *testing.T) {
	c := qt.New(t)

	bf1 := NewBigFloatFromFloat64(100.0)
	bf2 := NewBigFloatFromFloat64(50.0)

	// Add
	sum := bf1.Add(bf2)
	c.Assert(sum.(*BigFloat).Float64(), qt.Equals, float64(150.0))

	// Subtract
	diff := bf1.Subtract(bf2)
	c.Assert(diff.(*BigFloat).Float64(), qt.Equals, float64(50.0))

	// Multiply
	prod := bf1.Multiply(bf2)
	c.Assert(prod.(*BigFloat).Float64(), qt.Equals, float64(5000.0))

	// Divide
	quot := bf1.Divide(bf2)
	c.Assert(quot.(*BigFloat).Float64(), qt.Equals, float64(2.0))

	// Negate
	neg := bf1.Negate()
	c.Assert(neg.(*BigFloat).Float64(), qt.Equals, float64(-100.0))
}

func TestBigFloat_Comparison(t *testing.T) {
	c := qt.New(t)

	bf1 := NewBigFloatFromFloat64(100.0)
	bf2 := NewBigFloatFromFloat64(50.0)
	bf3 := NewBigFloatFromFloat64(100.0)

	c.Assert(bf1.Compare(bf2), qt.Equals, 1)
	c.Assert(bf2.Compare(bf1), qt.Equals, -1)
	c.Assert(bf1.Compare(bf3), qt.Equals, 0)

	c.Assert(bf2.LessThan(bf1), qt.IsTrue)
	c.Assert(bf1.LessThan(bf2), qt.IsFalse)
}

func TestBigFloat_Properties(t *testing.T) {
	c := qt.New(t)

	positive := NewBigFloatFromFloat64(3.14)
	negative := NewBigFloatFromFloat64(-3.14)
	zero := NewBigFloatFromFloat64(0.0)

	c.Assert(positive.IsPositive(), qt.IsTrue)
	c.Assert(positive.IsNegative(), qt.IsFalse)
	c.Assert(positive.IsZero(), qt.IsFalse)

	c.Assert(negative.IsPositive(), qt.IsFalse)
	c.Assert(negative.IsNegative(), qt.IsTrue)
	c.Assert(negative.IsZero(), qt.IsFalse)

	c.Assert(zero.IsPositive(), qt.IsFalse)
	c.Assert(zero.IsNegative(), qt.IsFalse)
	c.Assert(zero.IsZero(), qt.IsTrue)

	c.Assert(positive.IsExact(), qt.IsFalse) // BigFloat is inexact
}

func TestBigFloat_Conversions(t *testing.T) {
	c := qt.New(t)

	bf := NewBigFloatFromFloat64(3.14)

	// ToInexact should return itself
	c.Assert(bf.ToInexact(), qt.Equals, bf)

	// ToExact should return Rational
	exact := bf.ToExact()
	_, ok := exact.(*Rational)
	c.Assert(ok, qt.IsTrue)

	// SchemeString
	c.Assert(bf.SchemeString(), qt.Not(qt.Equals), "")
}

func TestBigFloat_EqualTo(t *testing.T) {
	c := qt.New(t)

	bf1 := NewBigFloatFromFloat64(3.14)
	bf2 := NewBigFloatFromFloat64(3.14)
	bf3 := NewBigFloatFromFloat64(2.71)
	f1 := NewFloat(3.14)

	c.Assert(bf1.EqualTo(bf2), qt.IsTrue)
	c.Assert(bf1.EqualTo(bf3), qt.IsFalse)
	c.Assert(bf1.EqualTo(f1), qt.IsTrue) // Should equal regular Float
	c.Assert(bf1.EqualTo(NewInteger(3)), qt.IsFalse)
}

func TestBigFloat_MixedArithmetic(t *testing.T) {
	c := qt.New(t)

	bf := NewBigFloatFromFloat64(100.0)

	// Add with Integer
	sum := bf.Add(NewInteger(50))
	c.Assert(sum.(*BigFloat).Float64(), qt.Equals, float64(150.0))

	// Add with Float
	sumF := bf.Add(NewFloat(0.5))
	c.Assert(sumF.(*BigFloat).Float64(), qt.Equals, float64(100.5))

	// Add with BigInteger
	sumBI := bf.Add(NewBigIntegerFromInt64(25))
	c.Assert(sumBI.(*BigFloat).Float64(), qt.Equals, float64(125.0))

	// Add with Complex
	sumC := bf.Add(NewComplex(complex(1, 2)))
	comp, ok := sumC.(*Complex)
	c.Assert(ok, qt.IsTrue)
	c.Assert(real(comp.Datum()), qt.Equals, float64(101))
}

func TestBigInteger_DivisionByZero(t *testing.T) {
	c := qt.New(t)

	bi := NewBigIntegerFromInt64(100)
	zero := NewBigIntegerFromInt64(0)

	result := bi.Divide(zero)
	c.Assert(result, qt.IsNil)
}

func TestBigFloat_DivisionByZero(t *testing.T) {
	c := qt.New(t)

	bf := NewBigFloatFromFloat64(100.0)
	zero := NewBigFloatFromFloat64(0.0)

	result := bf.Divide(zero)
	c.Assert(result, qt.IsNil)
}

func TestBigInteger_IsVoid(t *testing.T) {
	c := qt.New(t)

	bi := NewBigIntegerFromInt64(42)
	c.Assert(bi.IsVoid(), qt.IsFalse)

	var nilBI *BigInteger
	c.Assert(nilBI.IsVoid(), qt.IsTrue)
}

func TestBigFloat_IsVoid(t *testing.T) {
	c := qt.New(t)

	bf := NewBigFloatFromFloat64(3.14)
	c.Assert(bf.IsVoid(), qt.IsFalse)

	var nilBF *BigFloat
	c.Assert(nilBF.IsVoid(), qt.IsTrue)
}

func TestBigInteger_ZeroOptimizations(t *testing.T) {
	c := qt.New(t)

	bi := NewBigIntegerFromInt64(100)
	zero := NewBigIntegerFromInt64(0)

	// Add with zero returns original
	sum := bi.Add(zero)
	c.Assert(sum, qt.Equals, bi)

	// Zero + bi returns bi
	sum2 := zero.Add(bi)
	c.Assert(sum2, qt.Equals, bi)

	// Multiply by zero
	prod := bi.Multiply(zero)
	c.Assert(prod.(*BigInteger).IsZero(), qt.IsTrue)
}

func TestBigFloat_ZeroOptimizations(t *testing.T) {
	c := qt.New(t)

	bf := NewBigFloatFromFloat64(100.0)
	zero := NewBigFloatFromFloat64(0.0)

	// Add with zero returns original
	sum := bf.Add(zero)
	c.Assert(sum, qt.Equals, bf)

	// Zero + bf returns bf
	sum2 := zero.Add(bf)
	c.Assert(sum2, qt.Equals, bf)

	// Multiply by zero
	prod := bf.Multiply(zero)
	c.Assert(prod.(*BigFloat).IsZero(), qt.IsTrue)
}
