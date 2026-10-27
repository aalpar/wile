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
	"math/rand"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestBigFloatFromStringIsFixedPrecisionAndRoundTrips pins the fixed precision
// class: a decimal string of any length reads at DefaultBigFloatPrecision, and
// the writer's shortest digits (Text('g', -1)) read back as the same value. A
// precision derived from the digit count broke the second half: the 256-bit
// value of a long literal printed with ~78 digits, which read back at ~260 bits
// as a different value.
func TestBigFloatFromStringIsFixedPrecisionAndRoundTrips(t *testing.T) {
	rng := rand.New(rand.NewSource(192))
	for n := 2; n <= 130; n++ {
		for range 60 {
			var b strings.Builder
			b.WriteByte(byte('1' + rng.Intn(9)))
			b.WriteByte('.')
			for j := 1; j < n; j++ {
				b.WriteByte(byte('0' + rng.Intn(10)))
			}
			in := b.String()
			x := NewBigFloatFromString(in)
			if x.value.Prec() != DefaultBigFloatPrecision {
				t.Fatalf("n=%d: %s read at %d bits", n, in, x.value.Prec())
			}
			written := x.value.Text('g', -1)
			if NewBigFloatFromString(written).value.Cmp(x.value) != 0 {
				t.Fatalf("n=%d: %s wrote as %s, which reads back as another value", n, in, written)
			}
		}
	}
}

// TestOperandPrecision pins the operand-max precision: the widest BigFloat
// operand or BigFloat part, the default when none carries one (an exact
// operand, a NaN); and KernelPrecision, the same capped for the kernels.
func TestOperandPrecision(t *testing.T) {
	at := func(prec uint) *BigFloat {
		return NewBigFloat(new(big.Float).SetPrec(prec).SetInt64(3))
	}
	tcs := []struct {
		name string
		ns   []Value
		want uint
	}{
		{"no operand", nil, DefaultBigFloatPrecision},
		{"exact operands only", []Value{NewInteger(1), NewRationalFromRat(big.NewRat(1, 3))}, DefaultBigFloatPrecision},
		{"float operand", []Value{NewFloat(1.5)}, DefaultBigFloatPrecision},
		{"nan carries no precision", []Value{NewBigFloatNaN()}, DefaultBigFloatPrecision},
		{"widest bigfloat", []Value{at(300), NewInteger(2), at(500)}, 500},
		{"narrow bigfloat", []Value{at(100)}, 100},
		{"bigcomplex bigfloat part", []Value{NewBigComplex(NewRationalFromRat(big.NewRat(1, 3)), at(700))}, 700},
		{"bigcomplex exact parts", []Value{NewBigComplex(NewBigIntegerFromInt64(1), NewBigIntegerFromInt64(2))}, DefaultBigFloatPrecision},
		{"not capped", []Value{at(MaxBigFloatKernelPrecision + 1)}, MaxBigFloatKernelPrecision + 1},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, OperandPrecision(tc.ns...), qt.Equals, tc.want)
			qt.Assert(t, KernelPrecision(tc.ns...), qt.Equals, min(tc.want, MaxBigFloatKernelPrecision))
		})
	}
}

// TestPromoteToBigFloatPair pins that arithmetic converts an operand at its
// BigFloat partner's precision, in either operand order.
func TestPromoteToBigFloatPair(t *testing.T) {
	wide := NewBigFloat(new(big.Float).SetPrec(500).SetInt64(1))
	third := NewRationalFromRat(big.NewRat(1, 3))
	tcs := []struct {
		name string
		p, o Number
	}{
		{"bigfloat receiver", wide, third},
		{"exact receiver", third, wide},
		{"integer receiver", NewInteger(7), wide},
		{"float receiver", NewFloat(0.1), wide},
		{"bigfloat receiver float operand", wide, NewFloat(0.1)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			a, b := promoteToBigFloatPair(tc.p, tc.o)
			qt.Assert(t, a.(*BigFloat).value.Prec(), qt.Equals, uint(500))
			qt.Assert(t, b.(*BigFloat).value.Prec(), qt.Equals, uint(500))
		})
	}
	nan, x := promoteToBigFloatPair(NewBigFloatNaN(), third)
	qt.Assert(t, nan.IsNaN(), qt.IsTrue)
	qt.Assert(t, x.(*BigFloat).value.Prec(), qt.Equals, uint(DefaultBigFloatPrecision))
	_, f := promoteToBigFloatPair(wide, NewFloat(math.NaN()))
	qt.Assert(t, f.IsNaN(), qt.IsTrue)
}

// TestBigFloatSqrtArms pins (*BigFloat).Sqrt's NaN arm, which answers before the
// sign test (a NaN's zero value would otherwise read as zero), and its
// precision, the operand's own on both the real and the branch-cut arm.
func TestBigFloatSqrtArms(t *testing.T) {
	nan := NewBigFloatNaN()
	qt.Assert(t, nan.Sqrt().IsNaN(), qt.IsTrue)
	pos := NewBigFloat(new(big.Float).SetPrec(500).SetInt64(2))
	qt.Assert(t, pos.Sqrt().(*BigFloat).value.Prec(), qt.Equals, uint(500))
	neg := NewBigFloat(new(big.Float).SetPrec(500).SetInt64(-2))
	root := neg.Sqrt().(*BigComplex)
	qt.Assert(t, root.Imag().(*BigFloat).value.Prec(), qt.Equals, uint(500))
	qt.Assert(t, root.Imag().(*BigFloat).value.Cmp(pos.Sqrt().(*BigFloat).value), qt.Equals, 0)
}

// TestBigComplexLessThanExactParts pins that exact real parts compare exactly:
// rounded to 256 bits first, 1 and 1+2^-300 ordered as equal.
func TestBigComplexLessThanExactParts(t *testing.T) {
	near := new(big.Rat).Add(big.NewRat(1, 1), new(big.Rat).SetFrac(big.NewInt(1), new(big.Int).Lsh(big.NewInt(1), 300)))
	a := NewBigComplex(NewBigIntegerFromInt64(1), NewBigIntegerFromInt64(1))
	b := NewBigComplex(NewRationalFromRat(near), NewBigIntegerFromInt64(1))
	qt.Assert(t, a.LessThan(b), qt.IsTrue)
	qt.Assert(t, b.LessThan(a), qt.IsFalse)
	qt.Assert(t, bigComplexLessThan[KindBigComplex](a, b), qt.IsTrue)
}
