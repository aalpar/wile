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

func TestDecimalLiteralPrecision(t *testing.T) {
	tcs := []struct {
		in   string
		want uint
	}{
		{"1.5", DefaultBigFloatPrecision},
		{"1e400", DefaultBigFloatPrecision},
		{"0." + strings.Repeat("0", 200) + "1", DefaultBigFloatPrecision},
		{strings.Repeat("7", 100), 334},
		{"1." + strings.Repeat("0", 5000), DefaultBigFloatPrecision},
		{"1" + strings.Repeat("0", 300), 1001},
		{"-1." + strings.Repeat("7", 99) + "e5", 334},
	}
	for _, tc := range tcs {
		t.Run(tc.in[:min(len(tc.in), 20)], func(t *testing.T) {
			c := qt.New(t)
			c.Assert(decimalLiteralPrecision(tc.in), qt.Equals, tc.want)
		})
	}
}

// TestDecimalLiteralPrecisionIntegersAreExact checks the formula against the
// oracle rather than a constant: an n-digit integer must parse exactly.
func TestDecimalLiteralPrecisionIntegersAreExact(t *testing.T) {
	c := qt.New(t)
	for n := 1; n <= 2000; n++ {
		s := strings.Repeat("9", n)
		want, _ := new(big.Int).SetString(s, 10)
		got, acc := NewBigFloatFromString(s).BigFloatValue().Int(nil)
		c.Assert(acc, qt.Equals, big.Exact, qt.Commentf("n=%d", n))
		c.Assert(got.Cmp(want), qt.Equals, 0, qt.Commentf("n=%d", n))
	}
}

// TestLongDecimalFractionRoundTrips pins the Matula margin: fractions with a
// leading 8 or 9 fail a +1 bound for some digit strings.
func TestLongDecimalFractionRoundTrips(t *testing.T) {
	rng := rand.New(rand.NewSource(192))
	for n := 78; n <= 130; n++ {
		for range 60 {
			var b strings.Builder
			b.WriteByte(byte('8' + rng.Intn(2)))
			b.WriteByte('.')
			for j := 1; j < n-1; j++ {
				b.WriteByte(byte('0' + rng.Intn(10)))
			}
			b.WriteByte(byte('1' + rng.Intn(9)))
			in := b.String()
			got := NewBigFloatFromString(in).value.Text('g', -1)
			if got != in {
				t.Fatalf("n=%d: %s read back as %s", n, in, got)
			}
		}
	}
}

// TestOperandPrecision pins the operand-max working precision: the widest
// BigFloat operand or BigFloat part, the default when none carries one (an exact
// operand, a NaN), and the cap for a derived precision past the literal bound.
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
		{"capped", []Value{at(MaxBigFloatLiteralPrecision + 1)}, MaxBigFloatLiteralPrecision},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, OperandPrecision(tc.ns...), qt.Equals, tc.want)
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
