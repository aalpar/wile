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

package helpers

import (
	"errors"
	"math"
	"math/big"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

func TestToComplex128(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name    string
		input   values.Value
		wantR   float64
		wantI   float64
		checkFn func(complex128) bool // optional: overrides wantR/wantI comparison
	}{
		// Integer
		{"integer zero", values.NewInteger(0), 0, 0, nil},
		{"integer positive", values.NewInteger(42), 42, 0, nil},
		{"integer negative", values.NewInteger(-7), -7, 0, nil},
		{"integer max int64", values.NewInteger(math.MaxInt64), float64(math.MaxInt64), 0, nil},
		{"integer min int64", values.NewInteger(math.MinInt64), float64(math.MinInt64), 0, nil},

		// BigInteger
		{"big integer zero", values.NewBigIntegerFromInt64(0), 0, 0, nil},
		{"big integer positive", values.NewBigIntegerFromInt64(42), 42, 0, nil},
		{"big integer negative", values.NewBigIntegerFromInt64(-100), -100, 0, nil},
		{"big integer large", values.NewBigIntegerFromInt64(math.MaxInt64), float64(math.MaxInt64), 0, nil},

		// Float
		{"float zero", values.NewFloat(0.0), 0, 0, nil},
		{"float positive", values.NewFloat(3.14), 3.14, 0, nil},
		{"float negative", values.NewFloat(-2.718), -2.718, 0, nil},
		{"float infinity", values.NewFloat(math.Inf(1)), math.Inf(1), 0, nil},
		{"float neg infinity", values.NewFloat(math.Inf(-1)), math.Inf(-1), 0, nil},
		{
			"float NaN", values.NewFloat(math.NaN()), 0, 0,
			func(result complex128) bool {
				return math.IsNaN(real(result)) && imag(result) == 0
			},
		},

		// BigFloat
		{"big float zero", values.NewBigFloatFromFloat64(0.0), 0, 0, nil},
		{"big float positive", values.NewBigFloatFromFloat64(3.14), 3.14, 0, nil},
		{"big float negative", values.NewBigFloatFromFloat64(-2.718), -2.718, 0, nil},

		// Rational
		{"rational 1/3", values.NewRational(1, 3), 1.0 / 3.0, 0, nil},
		{"rational 1/1", values.NewRational(1, 1), 1, 0, nil},
		{"rational -1/2", values.NewRational(-1, 2), -0.5, 0, nil},
		{"rational zero", values.NewRational(0, 1), 0, 0, nil},

		// Complex
		{"complex zero", values.NewComplex(complex(0, 0)), 0, 0, nil},
		{"complex 1+2i", values.NewComplex(complex(1, 2)), 1, 2, nil},
		{"complex real only", values.NewComplex(complex(5, 0)), 5, 0, nil},
		{"complex imag only", values.NewComplex(complex(0, 3)), 0, 3, nil},
		{"complex negative", values.NewComplex(complex(-1, -2)), -1, -2, nil},
		{
			"complex NaN parts", values.NewComplex(complex(math.NaN(), math.NaN())), 0, 0,
			func(result complex128) bool {
				return math.IsNaN(real(result)) && math.IsNaN(imag(result))
			},
		},

		// BigComplex
		{
			"big complex 1+2i",
			values.NewBigComplex(values.NewBigFloatFromFloat64(1), values.NewBigFloatFromFloat64(2)),
			1, 2, nil,
		},
		{
			"big complex zero",
			values.NewBigComplex(values.NewBigFloatFromFloat64(0), values.NewBigFloatFromFloat64(0)),
			0, 0, nil,
		},
		{
			"big complex negative",
			values.NewBigComplex(values.NewBigFloatFromFloat64(-3.5), values.NewBigFloatFromFloat64(-1.5)),
			-3.5, -1.5, nil,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := ToComplex128(tc.input)
			c.Assert(err, qt.IsNil)
			if tc.checkFn != nil {
				c.Assert(tc.checkFn(result), qt.IsTrue, qt.Commentf("result: %v", result))
			} else {
				c.Assert(real(result), qt.Equals, tc.wantR, qt.Commentf("real part"))
				c.Assert(imag(result), qt.Equals, tc.wantI, qt.Commentf("imag part"))
			}
		})
	}
}

func TestToComplex128_Errors(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name  string
		input values.Value
	}{
		{"string", values.NewString("hello")},
		{"character", values.NewCharacter('a')},
		{"boolean true", values.TrueValue},
		{"boolean false", values.FalseValue},
		{"empty list", values.EmptyList},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := ToComplex128(tc.input)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, werr.ErrNotANumber), qt.IsTrue)
		})
	}
}

func TestComplexOrFloat(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name    string
		input   complex128
		checkFn func(values.Value) bool
	}{
		{
			"zero imag returns float",
			complex(3.14, 0),
			func(v values.Value) bool {
				f, ok := v.(*values.Float)
				return ok && f.Value == 3.14
			},
		},
		{
			"positive zero imag returns float",
			complex(42, 0),
			func(v values.Value) bool {
				f, ok := v.(*values.Float)
				return ok && f.Value == 42
			},
		},
		{
			"negative zero imag returns float",
			complex(-1.5, 0),
			func(v values.Value) bool {
				f, ok := v.(*values.Float)
				return ok && f.Value == -1.5
			},
		},
		{
			"zero real and imag returns float",
			complex(0, 0),
			func(v values.Value) bool {
				f, ok := v.(*values.Float)
				return ok && f.Value == 0
			},
		},
		{
			"nonzero imag returns complex",
			complex(1, 2),
			func(v values.Value) bool {
				cx, ok := v.(*values.Complex)
				return ok && cx.Value == complex(1, 2)
			},
		},
		{
			"negative imag returns complex",
			complex(0, -3),
			func(v values.Value) bool {
				cx, ok := v.(*values.Complex)
				return ok && cx.Value == complex(0, -3)
			},
		},
		{
			"infinity real zero imag returns float",
			complex(math.Inf(1), 0),
			func(v values.Value) bool {
				f, ok := v.(*values.Float)
				return ok && math.IsInf(f.Value, 1)
			},
		},
		{
			"both NaN returns float NaN",
			complex(math.NaN(), math.NaN()),
			func(v values.Value) bool {
				f, ok := v.(*values.Float)
				return ok && math.IsNaN(f.Value)
			},
		},
		{
			"real NaN nonzero imag returns complex",
			complex(math.NaN(), 1),
			func(v values.Value) bool {
				cx, ok := v.(*values.Complex)
				if !ok {
					return false
				}
				return math.IsNaN(real(cx.Value)) && imag(cx.Value) == 1
			},
		},
		{
			"nonzero real imag NaN returns complex",
			complex(1, math.NaN()),
			func(v values.Value) bool {
				cx, ok := v.(*values.Complex)
				if !ok {
					return false
				}
				return real(cx.Value) == 1 && math.IsNaN(imag(cx.Value))
			},
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := ComplexOrFloat(tc.input)
			c.Assert(tc.checkFn(result), qt.IsTrue, qt.Commentf("result: %v (type %T)", result, result))
		})
	}
}

func TestToFloat64(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name    string
		input   values.Value
		want    float64
		checkFn func(float64) bool // optional: overrides want comparison
	}{
		// Integer — values that fit float64 exactly (≤ 2^53 magnitude
		// or exact powers of 2 above that).
		{"integer zero", values.NewInteger(0), 0, nil},
		{"integer positive", values.NewInteger(42), 42, nil},
		{"integer negative", values.NewInteger(-7), -7, nil},
		// math.MinInt64 = -2^63 is an exact power of 2 → representable.
		{"integer min", values.NewInteger(math.MinInt64), float64(math.MinInt64), nil},

		// Float
		{"float zero", values.NewFloat(0.0), 0, nil},
		{"float positive", values.NewFloat(3.14), 3.14, nil},
		{"float negative", values.NewFloat(-2.718), -2.718, nil},
		{"float infinity", values.NewFloat(math.Inf(1)), math.Inf(1), nil},
		{"float neg infinity", values.NewFloat(math.Inf(-1)), math.Inf(-1), nil},
		{
			"float NaN", values.NewFloat(math.NaN()), 0,
			math.IsNaN,
		},

		// Rational — only those losslessly representable as float64
		// (powers-of-2 denominator, numerator ≤ 2^53 magnitude).
		{"rational 1/1", values.NewRational(1, 1), 1, nil},
		{"rational -1/2", values.NewRational(-1, 2), -0.5, nil},
		{"rational zero", values.NewRational(0, 1), 0, nil},

		// BigInteger
		{"big integer positive", values.NewBigIntegerFromInt64(42), 42, nil},
		{"big integer negative", values.NewBigIntegerFromInt64(-7), -7, nil},
		{"big integer zero", values.NewBigIntegerFromInt64(0), 0, nil},

		// BigFloat — values constructed from float64 round-trip exactly.
		{"big float positive", values.NewBigFloatFromFloat64(3.14), 3.14, nil},
		{"big float negative", values.NewBigFloatFromFloat64(-2.718), -2.718, nil},
		{"big float zero", values.NewBigFloatFromFloat64(0.0), 0, nil},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := ToFloat64(tc.input)
			c.Assert(err, qt.IsNil)
			if tc.checkFn != nil {
				c.Assert(tc.checkFn(result), qt.IsTrue, qt.Commentf("result: %v", result))
			} else {
				c.Assert(result, qt.Equals, tc.want)
			}
		})
	}
}

func TestToFloat64_Errors(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name  string
		input values.Value
	}{
		{"complex", values.NewComplex(complex(1, 2))},
		{
			"big complex",
			values.NewBigComplex(values.NewBigFloatFromFloat64(1), values.NewBigFloatFromFloat64(2)),
		},
		{"string", values.NewString("hello")},
		{"character", values.NewCharacter('a')},
		{"boolean true", values.TrueValue},
		{"empty list", values.EmptyList},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := ToFloat64(tc.input)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, werr.ErrNotAReal), qt.IsTrue)
		})
	}
}

// TestToFloat64_LossyConversion verifies the PR 2 tightening: inputs that
// previously truncated silently now error with werr.ErrLossyConversion.
// Pre-PR-2, all of these succeeded with a silently-rounded float64.
func TestToFloat64_LossyConversion(t *testing.T) {
	c := qt.New(t)

	bigOverflow, _, err := big.ParseFloat("1e500", 10, 256, big.ToNearestEven)
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name  string
		input values.Value
	}{
		// math.MaxInt64 = 2^63 - 1; float64 rounds to 2^63 (Above).
		{"integer max int64", values.NewInteger(math.MaxInt64)},
		// 1/3 is irrational in float64 — rounds Below.
		{"rational 1/3", values.NewRational(1, 3)},
		// BigInteger that requires more than 53 mantissa bits — float64
		// can't preserve every digit. (2^100 + 1 cannot, because 2^100
		// alone uses the implicit leading-1 mantissa bit.)
		{"big integer precision loss",
			values.NewBigInteger(new(big.Int).Add(
				new(big.Int).Lsh(big.NewInt(1), 100),
				big.NewInt(1)))},
		// BigFloat overflowing float64 magnitude (saturates to +Inf, Above).
		{"big float overflow", values.NewBigFloat(bigOverflow)},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := ToFloat64(tc.input)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, werr.ErrLossyConversion), qt.IsTrue,
				qt.Commentf("expected ErrLossyConversion, got: %v", err))
		})
	}
}

func TestExtractReal(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name      string
		input     values.Value
		wantVal   float64
		wantExact bool
		checkFn   func(float64) bool // optional: overrides wantVal comparison
	}{
		// Integer (exact)
		{"integer zero", values.NewInteger(0), 0, true, nil},
		{"integer positive", values.NewInteger(42), 42, true, nil},
		{"integer negative", values.NewInteger(-7), -7, true, nil},
		{"integer max", values.NewInteger(math.MaxInt64), float64(math.MaxInt64), true, nil},

		// BigInteger (exact)
		{"big integer zero", values.NewBigIntegerFromInt64(0), 0, true, nil},
		{"big integer positive", values.NewBigIntegerFromInt64(42), 42, true, nil},
		{"big integer negative", values.NewBigIntegerFromInt64(-100), -100, true, nil},
		{"big integer large", values.NewBigIntegerFromInt64(math.MaxInt64), float64(math.MaxInt64), true, nil},

		// Float (inexact)
		{"float zero", values.NewFloat(0.0), 0, false, nil},
		{"float positive", values.NewFloat(3.14), 3.14, false, nil},
		{"float negative", values.NewFloat(-2.718), -2.718, false, nil},
		{"float infinity", values.NewFloat(math.Inf(1)), math.Inf(1), false, nil},
		{"float neg infinity", values.NewFloat(math.Inf(-1)), math.Inf(-1), false, nil},
		{
			"float NaN", values.NewFloat(math.NaN()), 0, false,
			math.IsNaN,
		},

		// Rational (exact)
		{"rational 1/3", values.NewRational(1, 3), 1.0 / 3.0, true, nil},
		{"rational 1/1", values.NewRational(1, 1), 1, true, nil},
		{"rational -1/2", values.NewRational(-1, 2), -0.5, true, nil},
		{"rational zero", values.NewRational(0, 1), 0, true, nil},

		// BigFloat (inexact)
		{"big float zero", values.NewBigFloatFromFloat64(0.0), 0, false, nil},
		{"big float positive", values.NewBigFloatFromFloat64(3.14), 3.14, false, nil},
		{"big float negative", values.NewBigFloatFromFloat64(-2.718), -2.718, false, nil},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			val, exact, err := ExtractReal(tc.input, "test-op")
			c.Assert(err, qt.IsNil)
			c.Assert(exact, qt.Equals, tc.wantExact, qt.Commentf("exactness"))
			if tc.checkFn != nil {
				c.Assert(tc.checkFn(val), qt.IsTrue, qt.Commentf("result: %v", val))
			} else {
				c.Assert(val, qt.Equals, tc.wantVal)
			}
		})
	}
}

func TestExtractReal_Errors(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name  string
		input values.Value
	}{
		{"complex", values.NewComplex(complex(1, 2))},
		{
			"big complex",
			values.NewBigComplex(values.NewBigFloatFromFloat64(1), values.NewBigFloatFromFloat64(2)),
		},
		{"string", values.NewString("hello")},
		{"character", values.NewCharacter('a')},
		{"boolean true", values.TrueValue},
		{"empty list", values.EmptyList},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, _, err := ExtractReal(tc.input, "test-op")
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, werr.ErrNotAReal), qt.IsTrue)
		})
	}
}
