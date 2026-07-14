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

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

func TestBigInteger_Constructors(t *testing.T) {
	c := qt.New(t)

	// From int64
	bi1 := values.NewBigIntegerFromInt64(42)
	c.Assert(bi1.Int64(), qt.Equals, int64(42))

	// From string
	bi2 := values.NewBigIntegerFromString("12345678901234567890", 10)
	c.Assert(bi2, qt.IsNotNil)
	c.Assert(bi2.BigInt().String(), qt.Equals, "12345678901234567890")

	// From big.Int
	bigVal := big.NewInt(99)
	bi3 := values.NewBigInteger(bigVal)
	c.Assert(bi3.Int64(), qt.Equals, int64(99))

	// Invalid string returns nil
	bi4 := values.NewBigIntegerFromString("invalid", 10)
	c.Assert(bi4, qt.IsNil)
}

func TestBigInteger_Arithmetic(t *testing.T) {
	c := qt.New(t)

	bi1 := values.NewBigIntegerFromInt64(100)
	bi2 := values.NewBigIntegerFromInt64(50)

	// Add
	sum := bi1.Add(bi2)
	c.Assert(sum.(*values.BigInteger).Int64(), qt.Equals, int64(150))

	// Subtract
	diff := bi1.Subtract(bi2)
	c.Assert(diff.(*values.BigInteger).Int64(), qt.Equals, int64(50))

	// Multiply
	prod := bi1.Multiply(bi2)
	c.Assert(prod.(*values.BigInteger).Int64(), qt.Equals, int64(5000))

	// Divide (returns Rational for exact division)
	quot, err := bi1.Divide(bi2)
	c.Assert(err, qt.IsNil)
	c.Assert(quot, qt.IsNotNil)

	// Negate
	neg := bi1.Negate()
	c.Assert(neg.(*values.BigInteger).Int64(), qt.Equals, int64(-100))
}

func TestBigInteger_Comparison(t *testing.T) {
	c := qt.New(t)

	bi1 := values.NewBigIntegerFromInt64(100)
	bi2 := values.NewBigIntegerFromInt64(50)
	bi3 := values.NewBigIntegerFromInt64(100)

	c.Assert(bi1.Compare(bi2), qt.Equals, 1)
	c.Assert(bi2.Compare(bi1), qt.Equals, -1)
	c.Assert(bi1.Compare(bi3), qt.Equals, 0)

	c.Assert(bi2.LessThan(bi1), qt.IsTrue)
	c.Assert(bi1.LessThan(bi2), qt.IsFalse)
}

func TestBigInteger_Properties(t *testing.T) {
	c := qt.New(t)

	positive := values.NewBigIntegerFromInt64(42)
	negative := values.NewBigIntegerFromInt64(-42)
	zero := values.NewBigIntegerFromInt64(0)

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

	bi := values.NewBigIntegerFromInt64(42)

	// ToExact should return itself
	biExact, err := bi.ToExact()
	c.Assert(err, qt.IsNil)
	c.Assert(biExact, valuestest.SchemeEquals, bi)

	// ToInexact should return Float
	inexact := bi.ToInexact()
	f, ok := inexact.(*values.Float)
	c.Assert(ok, qt.IsTrue)
	c.Assert(f.Value, qt.Equals, float64(42))

	// SchemeString
	c.Assert(bi.SchemeString(), qt.Equals, "42")
}

func TestBigInteger_EqualTo(t *testing.T) {
	c := qt.New(t)

	bi1 := values.NewBigIntegerFromInt64(42)
	bi2 := values.NewBigIntegerFromInt64(42)
	bi3 := values.NewBigIntegerFromInt64(99)
	int1 := values.NewInteger(42)

	c.Assert(bi1.EqualTo(bi2), qt.IsTrue)
	c.Assert(bi1.EqualTo(bi3), qt.IsFalse)
	c.Assert(bi1.EqualTo(int1), qt.IsTrue) // Should equal regular Integer
	c.Assert(bi1.EqualTo(values.NewFloat(42.0)), qt.IsFalse)
}

func TestBigInteger_MixedArithmetic(t *testing.T) {
	c := qt.New(t)

	bi := values.NewBigIntegerFromInt64(100)

	// Add with Integer
	sum := bi.Add(values.NewInteger(50))
	c.Assert(sum.(*values.BigInteger).Int64(), qt.Equals, int64(150))

	// Add with Float -> Float. Exactness contagion (R7RS 6.2.2): the exact operand is
	// absorbed into the inexact one's representation, it does not drag the result up
	// the precision axis. Chez gives (+ 100 0.5) => 100.5, a flonum.
	sumF := bi.Add(values.NewFloat(0.5))
	f, ok := sumF.(*values.Float)
	c.Assert(ok, qt.IsTrue, qt.Commentf("Expected *values.Float (contagion), got %T", sumF))
	c.Assert(f.Value, qt.Equals, float64(100.5))
	c.Assert(f.IsExact(), qt.Equals, false) // Must be inexact

	// Add with Complex → BigComplex. The contagion deliberately does NOT extend to the
	// complex axis: an exact operand rounded into complex128 gets a manufactured +0.0
	// imaginary part, which is not an exact 0, and the exact-zero sign rules stop
	// applying. See promotion.go Zone 3.
	sumC := bi.Add(values.NewComplex(complex(1, 2)))
	bc, ok := sumC.(*values.BigComplex)
	c.Assert(ok, qt.IsTrue, qt.Commentf("Expected *BigComplex, got %T", sumC))
	c.Assert(bc.RealAsBigFloat().Float64Truncated(), qt.Equals, float64(101))
}

func TestBigFloat_Constructors(t *testing.T) {
	c := qt.New(t)

	// From float64
	bf1 := values.NewBigFloatFromFloat64(3.14)
	c.Assert(bf1.Float64Truncated(), qt.Equals, float64(3.14))

	// From string
	bf2 := values.NewBigFloatFromString("3.14159265358979323846")
	c.Assert(bf2, qt.IsNotNil)

	// From big.Float
	bigVal := big.NewFloat(2.71)
	bf3 := values.NewBigFloat(bigVal)
	c.Assert(bf3.Float64Truncated(), qt.Equals, float64(2.71))

	// Invalid string returns nil
	bf4 := values.NewBigFloatFromString("invalid")
	c.Assert(bf4, qt.IsNil)
}

func TestBigFloat_Arithmetic(t *testing.T) {
	c := qt.New(t)

	bf1 := values.NewBigFloatFromFloat64(100.0)
	bf2 := values.NewBigFloatFromFloat64(50.0)

	// Add
	sum := bf1.Add(bf2)
	c.Assert(sum.(*values.BigFloat).Float64Truncated(), qt.Equals, float64(150.0))

	// Subtract
	diff := bf1.Subtract(bf2)
	c.Assert(diff.(*values.BigFloat).Float64Truncated(), qt.Equals, float64(50.0))

	// Multiply
	prod := bf1.Multiply(bf2)
	c.Assert(prod.(*values.BigFloat).Float64Truncated(), qt.Equals, float64(5000.0))

	// Divide
	quot, err := bf1.Divide(bf2)
	c.Assert(err, qt.IsNil)
	c.Assert(quot.(*values.BigFloat).Float64Truncated(), qt.Equals, float64(2.0))

	// Negate
	neg := bf1.Negate()
	c.Assert(neg.(*values.BigFloat).Float64Truncated(), qt.Equals, float64(-100.0))
}

func TestBigFloat_Comparison(t *testing.T) {
	c := qt.New(t)

	bf1 := values.NewBigFloatFromFloat64(100.0)
	bf2 := values.NewBigFloatFromFloat64(50.0)
	bf3 := values.NewBigFloatFromFloat64(100.0)

	c.Assert(bf1.Compare(bf2), qt.Equals, 1)
	c.Assert(bf2.Compare(bf1), qt.Equals, -1)
	c.Assert(bf1.Compare(bf3), qt.Equals, 0)

	c.Assert(bf2.LessThan(bf1), qt.IsTrue)
	c.Assert(bf1.LessThan(bf2), qt.IsFalse)
}

func TestBigFloat_Properties(t *testing.T) {
	c := qt.New(t)

	positive := values.NewBigFloatFromFloat64(3.14)
	negative := values.NewBigFloatFromFloat64(-3.14)
	zero := values.NewBigFloatFromFloat64(0.0)

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

	bf := values.NewBigFloatFromFloat64(3.14)

	// ToInexact should return itself
	c.Assert(bf.ToInexact(), valuestest.SchemeEquals, bf)

	// ToExact should return Rational
	exact, err := bf.ToExact()
	c.Assert(err, qt.IsNil)
	_, ok := exact.(*values.Rational)
	c.Assert(ok, qt.IsTrue)

	// SchemeString
	c.Assert(bf.SchemeString(), qt.Not(qt.Equals), "")
}

// TestBigFloat_Float64WithAccuracy exercises the loss-signal-aware float64
// accessor on the three observable branches: NaN flag (returns Exact per the
// Q-6 NaN identity rule), finite/exact pass-through, and overflow.
func TestBigFloat_Float64WithAccuracy(t *testing.T) {
	cases := []struct {
		name      string
		input     *values.BigFloat
		wantValue float64
		wantAcc   big.Accuracy
		wantNaN   bool
	}{
		{"finite-exact", values.NewBigFloatFromFloat64(2.5), 2.5, big.Exact, false},
		{"nan-identity", values.NewBigFloatNaN(), 0, big.Exact, true},
		{"overflow-above", values.NewBigFloatFromString("1e500"), math.Inf(1), big.Above, false},
		{"overflow-below", values.NewBigFloatFromString("-1e500"), math.Inf(-1), big.Below, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			f, acc := tc.input.Float64WithAccuracy()
			if tc.wantNaN {
				c.Assert(math.IsNaN(f), qt.IsTrue)
			} else {
				c.Assert(f, qt.Equals, tc.wantValue)
			}
			c.Assert(acc, qt.Equals, tc.wantAcc)
		})
	}
}

func TestBigFloat_EqualTo(t *testing.T) {
	c := qt.New(t)

	bf1 := values.NewBigFloatFromFloat64(3.14)
	bf2 := values.NewBigFloatFromFloat64(3.14)
	bf3 := values.NewBigFloatFromFloat64(2.71)
	f1 := values.NewFloat(3.14)

	c.Assert(bf1.EqualTo(bf2), qt.IsTrue)
	c.Assert(bf1.EqualTo(bf3), qt.IsFalse)
	// A BigFloat is NOT equal to a Float of the same value: both are inexact, and
	// their precisions are distinguishable by arithmetic, so R7RS 6.1 makes them
	// distinct numbers. (An exact Integer 3 was already unequal, for a different
	// reason -- exactness.)
	c.Assert(bf1.EqualTo(f1), qt.IsFalse)
	c.Assert(bf1.EqualTo(values.NewInteger(3)), qt.IsFalse)
}

func TestBigFloat_MixedArithmetic(t *testing.T) {
	c := qt.New(t)

	bf := values.NewBigFloatFromFloat64(100.0)

	// Add with Integer
	sum := bf.Add(values.NewInteger(50))
	c.Assert(sum.(*values.BigFloat).Float64Truncated(), qt.Equals, float64(150.0))

	// Add with Float
	sumF := bf.Add(values.NewFloat(0.5))
	c.Assert(sumF.(*values.BigFloat).Float64Truncated(), qt.Equals, float64(100.5))

	// Add with BigInteger
	sumBI := bf.Add(values.NewBigIntegerFromInt64(25))
	c.Assert(sumBI.(*values.BigFloat).Float64Truncated(), qt.Equals, float64(125.0))

	// Add with Complex (returns BigComplex to preserve BigFloat precision)
	sumC := bf.Add(values.NewComplex(complex(1, 2)))
	bc, ok := sumC.(*values.BigComplex)
	c.Assert(ok, qt.IsTrue)
	// Real part should be 100 + 1 = 101
	realPart := bc.Real()
	c.Assert(realPart.(*values.BigFloat).Float64Truncated(), qt.Equals, float64(101))
}

func TestBigInteger_DivisionByZero(t *testing.T) {
	c := qt.New(t)

	bi := values.NewBigIntegerFromInt64(100)
	zero := values.NewBigIntegerFromInt64(0)

	_, err := bi.Divide(zero)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Matches, ".*division by zero")
}

func TestBigFloat_DivisionByZero(t *testing.T) {
	c := qt.New(t)

	// Division by exact zero returns error.
	bf := values.NewBigFloatFromFloat64(100.0)
	exactZero := values.NewBigIntegerFromInt64(0)
	_, err := bf.Divide(exactZero)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Matches, ".*division by zero")

	// Division by inexact zero returns +Inf per IEEE 754 / R7RS §6.2.6.
	inexactZero := values.NewBigFloatFromFloat64(0.0)
	result, err := bf.Divide(inexactZero)
	c.Assert(err, qt.IsNil)
	rf, ok := result.(*values.BigFloat)
	c.Assert(ok, qt.IsTrue)
	c.Assert(rf.BigFloatValue().IsInf(), qt.IsTrue)
}

func TestBigInteger_IsVoid(t *testing.T) {
	c := qt.New(t)

	bi := values.NewBigIntegerFromInt64(42)
	c.Assert(bi.IsVoid(), qt.IsFalse)

	var nilBI *values.BigInteger
	c.Assert(nilBI.IsVoid(), qt.IsTrue)
}

func TestBigFloat_IsVoid(t *testing.T) {
	c := qt.New(t)

	bf := values.NewBigFloatFromFloat64(3.14)
	c.Assert(bf.IsVoid(), qt.IsFalse)

	var nilBF *values.BigFloat
	c.Assert(nilBF.IsVoid(), qt.IsTrue)
}

func TestBigInteger_ZeroOptimizations(t *testing.T) {
	c := qt.New(t)

	bi := values.NewBigIntegerFromInt64(100)
	zero := values.NewBigIntegerFromInt64(0)

	// Add with zero returns original
	sum := bi.Add(zero)
	c.Assert(sum, valuestest.SchemeEquals, bi)

	// Zero + bi returns bi
	sum2 := zero.Add(bi)
	c.Assert(sum2, valuestest.SchemeEquals, bi)

	// Multiply by zero — returns exact zero (may be Integer due to R7RS exact-zero rule)
	prod := bi.Multiply(zero)
	c.Assert(prod.IsZero(), qt.IsTrue)
	c.Assert(prod.IsExact(), qt.IsTrue)
}

func TestBigFloat_ZeroOptimizations(t *testing.T) {
	c := qt.New(t)

	bf := values.NewBigFloatFromFloat64(100.0)
	zero := values.NewBigFloatFromFloat64(0.0)

	// Add with zero returns original
	sum := bf.Add(zero)
	c.Assert(sum, valuestest.SchemeEquals, bf)

	// Zero + bf returns bf
	sum2 := zero.Add(bf)
	c.Assert(sum2, valuestest.SchemeEquals, bf)

	// Multiply by zero
	prod := bf.Multiply(zero)
	c.Assert(prod.(*values.BigFloat).IsZero(), qt.IsTrue)
}

func TestBigFloat_InfPredicates(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name       string
		bf         *values.BigFloat
		isFinite   bool
		isRational bool
		isNaN      bool
		isInteger  bool
		isZero     bool
		str        string
	}{
		{
			name:       "positive inf",
			bf:         values.NewBigFloat(new(big.Float).SetInf(false)),
			isFinite:   false,
			isRational: false,
			isNaN:      false,
			isInteger:  false,
			isZero:     false,
			str:        "+inf.0",
		},
		{
			name:       "negative inf",
			bf:         values.NewBigFloat(new(big.Float).SetInf(true)),
			isFinite:   false,
			isRational: false,
			isNaN:      false,
			isInteger:  false,
			isZero:     false,
			str:        "-inf.0",
		},
		{
			name:       "nan",
			bf:         values.NewBigFloatNaN(),
			isFinite:   false,
			isRational: false,
			isNaN:      true,
			isInteger:  false,
			isZero:     false,
			str:        "+nan.0",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(tc.bf.IsFinite(), qt.Equals, tc.isFinite)
			c.Assert(tc.bf.IsRational(), qt.Equals, tc.isRational)
			c.Assert(tc.bf.IsNaN(), qt.Equals, tc.isNaN)
			c.Assert(tc.bf.IsInteger(), qt.Equals, tc.isInteger)
			c.Assert(tc.bf.IsZero(), qt.Equals, tc.isZero)
			c.Assert(tc.bf.SchemeString(), qt.Equals, tc.str)
		})
	}
}

func TestBigFloat_InfArithmetic(t *testing.T) {
	c := qt.New(t)

	posInf := values.NewBigFloat(new(big.Float).SetInf(false))
	three := values.NewBigFloatFromFloat64(3.0)

	tcs := []struct {
		name   string
		result values.Number
		finite bool
		isNaN  bool
	}{
		{"inf + 3 = inf", posInf.Add(three), false, false},
		{"inf * 3 = inf", posInf.Multiply(three), false, false},
		{"inf + inf = inf", posInf.Add(posInf), false, false},
		{"inf - 3 = inf", posInf.Subtract(three), false, false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(tc.result.IsFinite(), qt.Equals, tc.finite)
			c.Assert(tc.result.IsNaN(), qt.Equals, tc.isNaN)
		})
	}
}

func TestBigFloat_ErrNaNRecovery(t *testing.T) {
	c := qt.New(t)

	posInf := values.NewBigFloat(new(big.Float).SetInf(false))
	negInf := values.NewBigFloat(new(big.Float).SetInf(true))
	zero := values.NewBigFloatFromFloat64(0.0)

	infDivInf, err := posInf.Divide(posInf)
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name   string
		result values.Number
	}{
		{"+inf + -inf = NaN", posInf.Add(negInf)},
		{"0 * +inf = NaN", zero.Multiply(posInf)},
		{"+inf / +inf = NaN", infDivInf},
		{"+inf - +inf = NaN", posInf.Subtract(posInf)},
		{"nan + 3 = NaN", values.NewBigFloatNaN().Add(values.NewBigFloatFromFloat64(3))},
		{"3 + nan = NaN", values.NewBigFloatFromFloat64(3).Add(values.NewBigFloatNaN())},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(tc.result.IsNaN(), qt.IsTrue)
		})
	}
}

func TestBigFloat_HashConsistency(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		f    *values.Float
		bf   *values.BigFloat
	}{
		{
			name: "+inf.0",
			f:    values.NewFloat(math.Inf(1)),
			bf:   values.NewBigFloat(new(big.Float).SetInf(false)),
		},
		{
			name: "-inf.0",
			f:    values.NewFloat(math.Inf(-1)),
			bf:   values.NewBigFloat(new(big.Float).SetInf(true)),
		},
		{
			name: "+nan.0",
			f:    values.NewFloat(math.NaN()),
			bf:   values.NewBigFloatNaN(),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(tc.f.HashCode(), qt.Equals, tc.bf.HashCode())
		})
	}
}

func TestBigFloat_NaNEquality(t *testing.T) {
	c := qt.New(t)

	nan1 := values.NewBigFloatNaN()
	nan2 := values.NewBigFloatNaN()
	finite := values.NewBigFloatFromFloat64(1.0)

	// EqualTo backs the equivalence predicates (eqv?/equal?), not numeric =, and an
	// equivalence relation is reflexive: the same object is equivalent to itself,
	// NaN payload or not. R7RS §6.1 requires eq? ⊆ eqv? ⊆ equal?, and eqv? settles
	// identity before inspecting the value — so equal? must too. See
	// TestFloat_NaNEquality for the full argument.
	c.Assert(nan1.EqualTo(nan1), qt.IsTrue)

	// Distinct objects: identity does not hold, so IEEE-754 decides and NaN != NaN.
	c.Assert(nan1.EqualTo(nan2), qt.IsFalse)
	c.Assert(nan1.EqualTo(finite), qt.IsFalse)
	c.Assert(finite.EqualTo(nan1), qt.IsFalse)
}

func TestBigFloat_FromFloat64NaN(t *testing.T) {
	c := qt.New(t)

	// NewBigFloatFromFloat64(NaN) must not panic and must produce NaN.
	bf := values.NewBigFloatFromFloat64(math.NaN())
	c.Assert(bf.IsNaN(), qt.IsTrue)
	c.Assert(bf.SchemeString(), qt.Equals, "+nan.0")
}

func TestBigFloat_SchemeString_DecimalPoint(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		val  float64
		want string
	}{
		{"integer value", 2.0, "2.0"},
		{"negative integer", -5.0, "-5.0"},
		{"zero", 0.0, "0.0"},
		{"fractional", 1.5, "1.5"},
		{"large integer", 1000000.0, "1e+06"},
		{"positive infinity", math.Inf(1), "+inf.0"},
		{"negative infinity", math.Inf(-1), "-inf.0"},
		{"NaN", math.NaN(), "+nan.0"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			bf := values.NewBigFloatFromFloat64(tc.val)
			c.Assert(bf.SchemeString(), qt.Equals, tc.want)
		})
	}
}
