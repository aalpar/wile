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
	"errors"
	"math"
	"math/big"
	"testing"

	"github.com/aalpar/wile/werr"

	qt "github.com/frankban/quicktest"
)

func TestToFloat64WithAccuracy(t *testing.T) {
	cases := []struct {
		name       string
		input      Number
		wantValue  float64
		wantAcc    big.Accuracy
		wantIsReal bool
	}{
		// ---- Integer ----
		{"integer-exact-zero", NewInteger(0), 0.0, big.Exact, true},
		{"integer-exact-small", NewInteger(7), 7.0, big.Exact, true},
		{"integer-exact-negative", NewInteger(-42), -42.0, big.Exact, true},
		// 2^53+1 rounds toward zero → result is below the true value
		{"integer-overflow-positive", NewInteger(int64(1)<<53 + 1), float64(int64(1) << 53), big.Below, true},
		// -(2^53+1) rounds toward zero → result is above the true value
		{"integer-overflow-negative", NewInteger(-(int64(1)<<53 + 1)), -float64(int64(1) << 53), big.Above, true},
		// -2^63 is an exact power of 2 — representable in float64
		{"integer-minint64", NewInteger(math.MinInt64), float64(math.MinInt64), big.Exact, true},
		// MaxInt64 = 2^63-1 rounds up to 2^63 in float64 → result is above true value
		{"integer-maxint64", NewInteger(math.MaxInt64), float64(math.MaxInt64), big.Above, true},

		// ---- BigInteger ----
		{"big-integer-exact", NewBigIntegerFromInt64(1234), 1234.0, big.Exact, true},
		// 10^400 overflows float64 → +Inf (above true value; max float64 ≈ 1.8e308)
		{"big-integer-overflow-pos",
			NewBigInteger(new(big.Int).Exp(big.NewInt(10), big.NewInt(400), nil)),
			math.Inf(1), big.Above, true},
		// -10^400 overflows float64 → -Inf (below true value)
		{"big-integer-overflow-neg",
			NewBigInteger(new(big.Int).Neg(new(big.Int).Exp(big.NewInt(10), big.NewInt(400), nil))),
			math.Inf(-1), big.Below, true},
		// 2^54 is an exact power of 2 — representable in float64
		{"big-integer-near-mantissa-boundary",
			NewBigInteger(new(big.Int).Lsh(big.NewInt(1), 54)),
			math.Ldexp(1, 54), big.Exact, true},

		// ---- Float ----
		{"float-identity", NewFloat(3.5), 3.5, big.Exact, true},
		{"float-zero", NewFloat(0.0), 0.0, big.Exact, true},
		{"float-negative-zero", NewFloat(math.Copysign(0, -1)), math.Copysign(0, -1), big.Exact, true},
		{"float-positive-inf", NewFloat(math.Inf(1)), math.Inf(1), big.Exact, true},
		{"float-negative-inf", NewFloat(math.Inf(-1)), math.Inf(-1), big.Exact, true},
		{"float-nan", NewFloat(math.NaN()), math.NaN(), big.Exact, true},
		{"float-maxfloat64", NewFloat(math.MaxFloat64), math.MaxFloat64, big.Exact, true},
		{"float-smallest-subnormal", NewFloat(math.SmallestNonzeroFloat64), math.SmallestNonzeroFloat64, big.Exact, true},

		// ---- BigFloat ----
		{"big-float-finite-exact", NewBigFloatFromFloat64(2.5), 2.5, big.Exact, true},
		// 1e500 overflows float64 → +Inf (above true value)
		{"big-float-overflow-positive", NewBigFloatFromString("1e500"), math.Inf(1), big.Above, true},
		// -1e500 overflows float64 → -Inf (below true value)
		{"big-float-overflow-negative", NewBigFloatFromString("-1e500"), math.Inf(-1), big.Below, true},
		// 1e-400 underflows float64 → 0.0 (below true value since 0 < 1e-400)
		{"big-float-underflow", NewBigFloatFromString("1e-400"), 0.0, big.Below, true},
		{"big-float-nan", NewBigFloatNaN(), math.NaN(), big.Exact, true},
		// 256-bit pi rounds below nearest float64 (math.Pi < BigFloat value)
		{"big-float-irrational-pi", NewBigFloatFromString("3.14159265358979323846"), math.Pi, big.Below, true},

		// ---- Rational ----
		{"rational-exact-half", NewRational(1, 2), 0.5, big.Exact, true},
		{"rational-exact-quarter", NewRational(1, 4), 0.25, big.Exact, true},
		// 1/3 rounds toward zero (down) in float64
		{"rational-onethird", NewRational(1, 3), 1.0 / 3.0, big.Below, true},
		// 1/10 rounds above in float64 (stored as 0.10000...1 in binary)
		{"rational-above", NewRational(1, 10), 0.1, big.Above, true},
		// 10^400 overflows float64 → +Inf (above true value)
		{"rational-overflow-positive",
			NewRationalFromBigInt(new(big.Int).Exp(big.NewInt(10), big.NewInt(400), nil), big.NewInt(1)),
			math.Inf(1), big.Above, true},
		// -10^400 overflows float64 → -Inf (below true value)
		{"rational-overflow-negative",
			NewRationalFromBigInt(new(big.Int).Neg(new(big.Int).Exp(big.NewInt(10), big.NewInt(400), nil)), big.NewInt(1)),
			math.Inf(-1), big.Below, true},

		// ---- Complex ----
		// Zero imaginary: isReal=true, real part is identity
		{"complex-real-zero-imag", NewComplex(complex(3.0, 0)), 3.0, big.Exact, true},
		// Non-zero imaginary: isReal=false, real part returned, imag dropped
		{"complex-with-imag", NewComplex(complex(3.0, 4.0)), 3.0, big.Exact, false},
		{"complex-nan-real", NewComplex(complex(math.NaN(), 0)), math.NaN(), big.Exact, true},

		// ---- BigComplex ----
		{"bigcomplex-exact-zero-imag",
			NewBigComplex(NewBigIntegerFromInt64(3), NewBigIntegerFromInt64(0)),
			3.0, big.Exact, true},
		// Non-zero imag: isReal=false
		{"bigcomplex-with-imag",
			NewBigComplex(NewBigIntegerFromInt64(3), NewBigIntegerFromInt64(4)),
			3.0, big.Exact, false},
		// Real part overflows → +Inf (above)
		{"bigcomplex-real-overflow",
			NewBigComplex(
				NewBigInteger(new(big.Int).Exp(big.NewInt(10), big.NewInt(500), nil)),
				NewBigIntegerFromInt64(0)),
			math.Inf(1), big.Above, true},
		// Real part overflows negative → -Inf (below true value since -Inf < -10^500)
		{"bigcomplex-real-below-overflow",
			NewBigComplex(
				NewBigInteger(new(big.Int).Neg(new(big.Int).Exp(big.NewInt(10), big.NewInt(500), nil))),
				NewBigIntegerFromInt64(0)),
			math.Inf(-1), big.Below, true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			f, acc, isReal, err := ToFloat64WithAccuracy(tc.input)
			c.Assert(err, qt.IsNil)
			if math.IsNaN(tc.wantValue) {
				c.Assert(math.IsNaN(f), qt.IsTrue)
			} else {
				c.Assert(f, qt.Equals, tc.wantValue)
			}
			c.Assert(acc, qt.Equals, tc.wantAcc)
			c.Assert(isReal, qt.Equals, tc.wantIsReal)
		})
	}
}

func TestToFloat64Lossless(t *testing.T) {
	losslessCases := []struct {
		name  string
		input Number
		want  float64
	}{
		{"integer", NewInteger(42), 42.0},
		{"big-integer", NewBigIntegerFromInt64(1234), 1234.0},
		{"float", NewFloat(3.5), 3.5},
		{"big-float-finite", NewBigFloatFromFloat64(2.5), 2.5},
		{"rational-half", NewRational(1, 2), 0.5},
		{"complex-zero-imag", NewComplex(complex(3.0, 0)), 3.0},
		{"bigcomplex-zero-imag", NewBigComplex(NewBigIntegerFromInt64(5), NewBigIntegerFromInt64(0)), 5.0},
	}
	for _, tc := range losslessCases {
		t.Run(tc.name+"-lossless", func(t *testing.T) {
			c := qt.New(t)
			f, err := ToFloat64Lossless(tc.input)
			c.Assert(err, qt.IsNil)
			c.Assert(f, qt.Equals, tc.want)
		})
	}

	lossyCases := []struct {
		name  string
		input Number
	}{
		{"integer-maxint64", NewInteger(math.MaxInt64)},
		{"rational-onethird", NewRational(1, 3)},
		{"rational-overflow-positive", NewRationalFromBigInt(new(big.Int).Exp(big.NewInt(10), big.NewInt(400), nil), big.NewInt(1))},
		{"bigfloat-overflow-positive", NewBigFloatFromString("1e500")},
		{"bigfloat-overflow-negative", NewBigFloatFromString("-1e500")},
		{"bigfloat-irrational", NewBigFloatFromString("3.141592653589793238462643383279")},
		{"complex-with-imag", NewComplex(complex(1, 1))},
		{"bigcomplex-with-imag", NewBigComplex(NewBigIntegerFromInt64(1), NewBigIntegerFromInt64(1))},
	}
	for _, tc := range lossyCases {
		t.Run(tc.name+"-errors", func(t *testing.T) {
			c := qt.New(t)
			_, err := ToFloat64Lossless(tc.input)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, werr.ErrLossyConversion), qt.IsTrue)
		})
	}
}

func TestToComplex128WithAccuracy(t *testing.T) {
	cases := []struct {
		name        string
		input       Number
		wantValue   complex128
		wantRealAcc big.Accuracy
		wantImagAcc big.Accuracy
	}{
		// ---- Integer ----
		{"integer-exact", NewInteger(7), complex(7, 0), big.Exact, big.Exact},

		// ---- BigInteger ----
		{"big-integer-exact", NewBigIntegerFromInt64(42), complex(42, 0), big.Exact, big.Exact},
		{"big-integer-real-overflow",
			NewBigInteger(new(big.Int).Exp(big.NewInt(10), big.NewInt(400), nil)),
			complex(math.Inf(1), 0), big.Above, big.Exact},

		// ---- Float ----
		{"float-exact", NewFloat(3.5), complex(3.5, 0), big.Exact, big.Exact},
		{"float-inf", NewFloat(math.Inf(1)), complex(math.Inf(1), 0), big.Exact, big.Exact},

		// ---- BigFloat ----
		{"big-float-exact", NewBigFloatFromFloat64(2.5), complex(2.5, 0), big.Exact, big.Exact},
		{"big-float-overflow", NewBigFloatFromString("1e500"), complex(math.Inf(1), 0), big.Above, big.Exact},

		// ---- Rational ----
		{"rational-exact", NewRational(1, 2), complex(0.5, 0), big.Exact, big.Exact},
		{"rational-below", NewRational(1, 3), complex(1.0/3.0, 0), big.Below, big.Exact},
		// 1/10 rounds above in float64 (stored as 0.10000...1 in binary)
		{"rational-above", NewRational(1, 10), complex(0.1, 0), big.Above, big.Exact},

		// ---- Complex ----
		{"complex-exact", NewComplex(complex(3.0, 4.0)), complex(3.0, 4.0), big.Exact, big.Exact},
		{"complex-zero-imag", NewComplex(complex(5.0, 0)), complex(5.0, 0), big.Exact, big.Exact},

		// ---- BigComplex ----
		{"bigcomplex-exact-both",
			NewBigComplex(NewBigIntegerFromInt64(3), NewBigIntegerFromInt64(4)),
			complex(3, 4), big.Exact, big.Exact},
		// Imaginary-component Below: 1/3 rounds down in float64
		{"bigcomplex-imag-below",
			NewBigComplex(NewBigIntegerFromInt64(0), NewRational(1, 3)),
			complex(0, 1.0/3.0), big.Exact, big.Below},
		// 1/10 rounds above in float64 (stored as 0.10000...1 in binary)
		{"bigcomplex-imag-above",
			NewBigComplex(NewBigIntegerFromInt64(0), NewRational(1, 10)),
			complex(0, 0.1), big.Exact, big.Above},
		// Real-component Above: overflow to +Inf
		{"bigcomplex-real-above",
			NewBigComplex(
				NewBigInteger(new(big.Int).Exp(big.NewInt(10), big.NewInt(500), nil)),
				NewBigIntegerFromInt64(0)),
			complex(math.Inf(1), 0), big.Above, big.Exact},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			res, err := ToComplex128WithAccuracy(tc.input)
			c.Assert(err, qt.IsNil)
			c.Assert(res, qt.DeepEquals, Complex128Result{
				Value:   tc.wantValue,
				RealAcc: tc.wantRealAcc,
				ImagAcc: tc.wantImagAcc,
			})
		})
	}
}

func TestToComplex128Lossless(t *testing.T) {
	losslessCases := []struct {
		name  string
		input Number
		want  complex128
	}{
		{"integer", NewInteger(42), complex(42, 0)},
		{"big-integer", NewBigIntegerFromInt64(5), complex(5, 0)},
		{"float", NewFloat(3.5), complex(3.5, 0)},
		{"rational-half", NewRational(1, 2), complex(0.5, 0)},
		{"complex-exact", NewComplex(complex(3.0, 4.0)), complex(3.0, 4.0)},
		{"bigcomplex-exact", NewBigComplex(NewBigIntegerFromInt64(3), NewBigIntegerFromInt64(4)), complex(3, 4)},
	}
	for _, tc := range losslessCases {
		t.Run(tc.name+"-lossless", func(t *testing.T) {
			c := qt.New(t)
			v, err := ToComplex128Lossless(tc.input)
			c.Assert(err, qt.IsNil)
			c.Assert(v, qt.Equals, tc.want)
		})
	}

	lossyCases := []struct {
		name  string
		input Number
	}{
		// Real-part lossy
		{"rational-real-below", NewRational(1, 3)},
		{"bigfloat-real-overflow", NewBigFloatFromString("1e500")},
		// Imag-part lossy (real is exact, imag is not)
		{"bigcomplex-imag-below", NewBigComplex(NewBigIntegerFromInt64(0), NewRational(1, 3))},
		{"bigcomplex-imag-above", NewBigComplex(NewBigIntegerFromInt64(0), NewRational(2, 3))},
	}
	for _, tc := range lossyCases {
		t.Run(tc.name+"-errors", func(t *testing.T) {
			c := qt.New(t)
			_, err := ToComplex128Lossless(tc.input)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, werr.ErrLossyConversion), qt.IsTrue)
		})
	}
}

func TestErrNotANumber(t *testing.T) {
	c := qt.New(t)
	var n Number

	_, _, _, err := ToFloat64WithAccuracy(n)
	c.Assert(errors.Is(err, werr.ErrNotANumber), qt.IsTrue)

	_, err = ToFloat64Lossless(n)
	c.Assert(errors.Is(err, werr.ErrNotANumber), qt.IsTrue)

	_, err = ToComplex128WithAccuracy(n)
	c.Assert(errors.Is(err, werr.ErrNotANumber), qt.IsTrue)

	_, err = ToComplex128Lossless(n)
	c.Assert(errors.Is(err, werr.ErrNotANumber), qt.IsTrue)
}
