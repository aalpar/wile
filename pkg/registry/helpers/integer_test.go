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
	"context"
	"errors"
	"math"
	"math/big"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"
)

// makeMC constructs a MachineContext with the given values as local bindings.
// This allows testing functions that read arguments via mc.Arg(i).
func makeMC(args ...values.Value) *machine.MachineContext {
	return makeMCWithContext(context.Background(), args...)
}

// makeMCWithContext is makeMC with the caller's context, for helpers whose
// contract includes observing cancellation.
func makeMCWithContext(ctx context.Context, args ...values.Value) *machine.MachineContext {
	local := environment.NewLocalEnvironment(len(args))
	for i, arg := range args {
		local.Bindings()[i].SetValue(arg)
	}
	topLevel := environment.NewNamespace()
	env := environment.NewEnvironmentFrameWithParent(local, topLevel.Runtime())
	tpl := machine.NewNativeTemplate(len(args), 0, false)
	return machine.NewMachineContext(
		ctx,
		machine.NewMachineContinuation(nil, tpl, env),
	)
}

func TestFloorDivide(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name  string
		n0    int64
		n1    int64
		wantQ int64
		wantR int64
	}{
		{"7/2", 7, 2, 3, 1},
		{"-7/2", -7, 2, -4, 1},
		{"7/-2", 7, -2, -4, -1},
		{"-7/-2", -7, -2, 3, -1},
		{"exact division 10/5", 10, 5, 2, 0},
		{"exact division -10/5", -10, 5, -2, 0},
		{"zero dividend", 0, 3, 0, 0},
		{"zero dividend negative divisor", 0, -3, 0, 0},
		{"dividend equals divisor", 5, 5, 1, 0},
		{"large values", math.MaxInt64 - 1, 2, (math.MaxInt64 - 1) / 2, 0},
		{"one/one", 1, 1, 1, 0},
		{"negative one", -1, 1, -1, 0},
		{"large negative", -math.MaxInt64, 2, -math.MaxInt64/2 - 1, 1},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			q, r := FloorDivide(tc.n0, tc.n1)
			c.Assert(q, qt.Equals, tc.wantQ)
			c.Assert(r, qt.Equals, tc.wantR)
			// Invariant: n0 == q*n1 + r
			c.Assert(tc.n0, qt.Equals, q*tc.n1+r, qt.Commentf("invariant n0 == q*n1 + r"))
		})
	}
}

func TestGcdInt(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		a    int64
		b    int64
		want int64
	}{
		{"standard gcd(12,8)", 12, 8, 4},
		{"gcd(0,5)", 0, 5, 5},
		{"gcd(5,0)", 5, 0, 5},
		{"gcd(0,0)", 0, 0, 0},
		{"same values", 7, 7, 7},
		{"coprime", 13, 17, 1},
		{"one is multiple of other", 15, 5, 5},
		{"reverse order", 8, 12, 4},
		{"large values", 1000000007, 999999937, 1},
		{"negative first", -12, 8, -4},
		{"negative second", 12, -8, 4},
		{"both negative", -12, -8, -4},
		{"gcd(1,anything)", 1, 123456, 1},
		{"gcd(anything,1)", 123456, 1, 1},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := GcdInt(tc.a, tc.b)
			c.Assert(result, qt.Equals, tc.want)
		})
	}
}

func TestExtractInteger(t *testing.T) {
	c := qt.New(t)

	t.Run("success cases", func(t *testing.T) {
		tcs := []struct {
			name       string
			input      values.Value
			wantInt64  int64
			wantBigNil bool // true if bigInt should be nil
			wantInex   bool
		}{
			{
				"integer positive",
				values.NewInteger(42),
				42, true, false,
			},
			{
				"integer zero",
				values.NewInteger(0),
				0, true, false,
			},
			{
				"integer negative",
				values.NewInteger(-100),
				-100, true, false,
			},
			{
				"integer max int64",
				values.NewInteger(math.MaxInt64),
				math.MaxInt64, true, false,
			},
			{
				"integer min int64",
				values.NewInteger(math.MinInt64),
				math.MinInt64, true, false,
			},
			{
				"float integer-valued positive",
				values.NewFloat(7.0),
				7, true, true,
			},
			{
				"float integer-valued zero",
				values.NewFloat(0.0),
				0, true, true,
			},
			{
				"float integer-valued negative",
				values.NewFloat(-5.0),
				-5, true, true,
			},
		}

		for _, tc := range tcs {
			t.Run(tc.name, func(t *testing.T) {
				i64, bi, inexact, err := ExtractInteger(tc.input, "test")
				c.Assert(err, qt.IsNil)
				c.Assert(inexact, qt.Equals, tc.wantInex)
				if tc.wantBigNil {
					c.Assert(bi, qt.IsNil)
					c.Assert(i64, qt.Equals, tc.wantInt64)
				} else {
					c.Assert(bi, qt.IsNotNil)
				}
			})
		}
	})

	t.Run("big integer fits int64", func(t *testing.T) {
		// BigInteger that fits in int64 still returns via bigInt path
		bi := values.NewBigIntegerFromInt64(42)
		i64, bigVal, inexact, err := ExtractInteger(bi, "test")
		c.Assert(err, qt.IsNil)
		c.Assert(inexact, qt.IsFalse)
		// ExtractInteger returns via the bigInt path for BigInteger
		c.Assert(i64, qt.Equals, int64(0))
		c.Assert(bigVal, qt.IsNotNil)
		c.Assert(bigVal.Int64(), qt.Equals, int64(42))
	})

	t.Run("big integer does not fit int64", func(t *testing.T) {
		huge := new(big.Int).SetUint64(math.MaxUint64)
		bi := values.NewBigInteger(huge)
		i64, bigVal, inexact, err := ExtractInteger(bi, "test")
		c.Assert(err, qt.IsNil)
		c.Assert(inexact, qt.IsFalse)
		c.Assert(i64, qt.Equals, int64(0))
		c.Assert(bigVal, qt.IsNotNil)
		c.Assert(bigVal.Cmp(huge), qt.Equals, 0)
	})

	t.Run("float large does not fit int64", func(t *testing.T) {
		// A float value larger than MaxInt64 but still integer-valued
		largeFloat := values.NewFloat(1e19)
		i64, bigVal, inexact, err := ExtractInteger(largeFloat, "test")
		c.Assert(err, qt.IsNil)
		c.Assert(inexact, qt.IsTrue)
		c.Assert(i64, qt.Equals, int64(0))
		c.Assert(bigVal, qt.IsNotNil)
	})

	t.Run("error cases", func(t *testing.T) {
		errTcs := []struct {
			name  string
			input values.Value
		}{
			{"float non-integer", values.NewFloat(3.5)},
			{"float NaN", values.NewFloat(math.NaN())},
			{"float +Inf", values.NewFloat(math.Inf(1))},
			{"float -Inf", values.NewFloat(math.Inf(-1))},
			{"string", values.NewString("hello")},
			{"boolean", values.TrueValue},
			{"symbol", values.NewSymbol("x")},
		}

		for _, tc := range errTcs {
			t.Run(tc.name, func(t *testing.T) {
				_, _, _, err := ExtractInteger(tc.input, "test")
				c.Assert(err, qt.IsNotNil)
				c.Assert(errors.Is(err, werr.ErrNotAnInteger), qt.IsTrue)
			})
		}
	})
}

// gcdCombiner is the combiner used by the gcd primitive.
func gcdCombiner(acc, val int64) (int64, bool) {
	result := GcdInt(acc, val)
	if result < 0 {
		result = -result
	}
	return result, false
}

// lcmCombiner is the combiner used by the lcm primitive.
func lcmCombiner(acc, val int64) (int64, bool) {
	if acc == 0 || val == 0 {
		return 0, false
	}
	g := GcdInt(acc, val)
	if g < 0 {
		g = -g
	}
	// Check for overflow: lcm = |acc * val| / gcd
	// Use the division-first approach to avoid overflow
	result := acc / g * val
	if result < 0 {
		result = -result
	}
	// Check if the multiplication overflowed
	if result/val != acc/g {
		return 0, true
	}
	return result, false
}

func TestIntegerFold_GCD(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		args values.Value
		want values.Value
	}{
		{
			"empty list returns identity 0",
			values.EmptyList,
			values.NewInteger(0),
		},
		{
			"single element",
			values.List(values.NewInteger(12)),
			values.NewInteger(12),
		},
		{
			"single negative element",
			values.List(values.NewInteger(-12)),
			values.NewInteger(12),
		},
		{
			"two elements gcd(12,8)=4",
			values.List(values.NewInteger(12), values.NewInteger(8)),
			values.NewInteger(4),
		},
		{
			"three elements gcd(12,8,6)=2",
			values.List(
				values.NewInteger(12),
				values.NewInteger(8),
				values.NewInteger(6),
			),
			values.NewInteger(2),
		},
		{
			"with zero gcd(0,5)=5",
			values.List(values.NewInteger(0), values.NewInteger(5)),
			values.NewInteger(5),
		},
		{
			"both zero gcd(0,0)=0",
			values.List(values.NewInteger(0), values.NewInteger(0)),
			values.NewInteger(0),
		},
		{
			"negative values gcd(-12,-8)=4",
			values.List(values.NewInteger(-12), values.NewInteger(-8)),
			values.NewInteger(4),
		},
		{
			"coprime gcd(7,13)=1",
			values.List(values.NewInteger(7), values.NewInteger(13)),
			values.NewInteger(1),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.args)
			err := IntegerFold(mc, FoldOpGCD, 0, gcdCombiner)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestIntegerFold_GCD_Inexact(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		args values.Value
		want values.Value
	}{
		{
			"inexact float gcd(12.0,8)=4.0",
			values.List(values.NewFloat(12.0), values.NewInteger(8)),
			values.NewFloat(4.0),
		},
		{
			"all inexact gcd(12.0,8.0)=4.0",
			values.List(values.NewFloat(12.0), values.NewFloat(8.0)),
			values.NewFloat(4.0),
		},
		{
			"single inexact element",
			values.List(values.NewFloat(7.0)),
			values.NewFloat(7.0),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.args)
			err := IntegerFold(mc, FoldOpGCD, 0, gcdCombiner)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestIntegerFold_LCM(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		args values.Value
		want values.Value
	}{
		{
			"empty list returns identity 1",
			values.EmptyList,
			values.NewInteger(1),
		},
		{
			"single element",
			values.List(values.NewInteger(5)),
			values.NewInteger(5),
		},
		{
			"single negative element",
			values.List(values.NewInteger(-5)),
			values.NewInteger(5),
		},
		{
			"two elements lcm(4,6)=12",
			values.List(values.NewInteger(4), values.NewInteger(6)),
			values.NewInteger(12),
		},
		{
			"three elements lcm(4,6,10)=60",
			values.List(
				values.NewInteger(4),
				values.NewInteger(6),
				values.NewInteger(10),
			),
			values.NewInteger(60),
		},
		{
			"with zero lcm(0,5)=0",
			values.List(values.NewInteger(0), values.NewInteger(5)),
			values.NewInteger(0),
		},
		{
			"coprime lcm(7,13)=91",
			values.List(values.NewInteger(7), values.NewInteger(13)),
			values.NewInteger(91),
		},
		{
			"one is multiple lcm(5,15)=15",
			values.List(values.NewInteger(5), values.NewInteger(15)),
			values.NewInteger(15),
		},
		{
			"negative values lcm(-4,-6)=12",
			values.List(values.NewInteger(-4), values.NewInteger(-6)),
			values.NewInteger(12),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.args)
			err := IntegerFold(mc, FoldOpLCM, 1, lcmCombiner)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestIntegerFold_LCM_BigInt(t *testing.T) {
	c := qt.New(t)

	// BigInteger that does not fit int64 triggers the big path
	huge := new(big.Int).SetUint64(math.MaxUint64)
	bigVal := values.NewBigInteger(huge)

	tcs := []struct {
		name    string
		args    values.Value
		checkFn func(values.Value)
	}{
		{
			"big integer single element",
			values.List(bigVal),
			func(v values.Value) {
				bi, ok := v.(*values.BigInteger)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigInteger, got %T", v))
				c.Assert(bi.BigInt().Cmp(huge), qt.Equals, 0)
			},
		},
		{
			"big integer with small integer",
			values.List(bigVal, values.NewInteger(2)),
			func(v values.Value) {
				bi, ok := v.(*values.BigInteger)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigInteger, got %T", v))
				// lcm(MaxUint64, 2) = MaxUint64 (since MaxUint64 is odd, gcd=1)
				// Actually MaxUint64 = 2^64 - 1, which is odd, so gcd(MaxUint64, 2) = 1
				// lcm = MaxUint64 * 2
				expected := new(big.Int).Mul(huge, big.NewInt(2))
				c.Assert(bi.BigInt().Cmp(expected), qt.Equals, 0)
			},
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.args)
			err := IntegerFold(mc, FoldOpLCM, 1, lcmCombiner)
			c.Assert(err, qt.IsNil)
			tc.checkFn(mc.GetValue())
		})
	}
}

func TestIntegerFold_LCM_Inexact(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		args values.Value
		want values.Value
	}{
		{
			"inexact float lcm(4.0,6)=12.0",
			values.List(values.NewFloat(4.0), values.NewInteger(6)),
			values.NewFloat(12.0),
		},
		{
			"all inexact lcm(4.0,6.0)=12.0",
			values.List(values.NewFloat(4.0), values.NewFloat(6.0)),
			values.NewFloat(12.0),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.args)
			err := IntegerFold(mc, FoldOpLCM, 1, lcmCombiner)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestIntegerFold_Errors(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		op       FoldOp
		identity int64
		args     values.Value
		sentinel error
	}{
		{
			"gcd non-integer argument string",
			FoldOpGCD, 0,
			values.List(values.NewString("hello")),
			werr.ErrNotAnInteger,
		},
		{
			"lcm non-integer argument boolean",
			FoldOpLCM, 1,
			values.List(values.TrueValue),
			werr.ErrNotAnInteger,
		},
		{
			"gcd non-integer float NaN",
			FoldOpGCD, 0,
			values.List(values.NewFloat(math.NaN())),
			werr.ErrNotAnInteger,
		},
		{
			"gcd non-integer float +Inf",
			FoldOpGCD, 0,
			values.List(values.NewFloat(math.Inf(1))),
			werr.ErrNotAnInteger,
		},
		{
			"gcd non-integer float -Inf",
			FoldOpGCD, 0,
			values.List(values.NewFloat(math.Inf(-1))),
			werr.ErrNotAnInteger,
		},
		{
			"gcd non-integer float 3.5",
			FoldOpGCD, 0,
			values.List(values.NewFloat(3.5)),
			werr.ErrNotAnInteger,
		},
		{
			"lcm non-integer float 2.7",
			FoldOpLCM, 1,
			values.List(values.NewFloat(2.7)),
			werr.ErrNotAnInteger,
		},
		{
			"gcd non-list argument",
			FoldOpGCD, 0,
			values.NewInteger(42),
			werr.ErrNotAList,
		},
		{
			"lcm non-list argument",
			FoldOpLCM, 1,
			values.NewString("hello"),
			werr.ErrNotAList,
		},
		{
			"gcd mixed valid and invalid",
			FoldOpGCD, 0,
			values.List(values.NewInteger(12), values.NewString("bad")),
			werr.ErrNotAnInteger,
		},
		{
			"lcm NaN after valid integer",
			FoldOpLCM, 1,
			values.List(values.NewInteger(4), values.NewFloat(math.NaN())),
			werr.ErrNotAnInteger,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.args)
			err := IntegerFold(mc, tc.op, tc.identity, gcdCombiner)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, tc.sentinel), qt.IsTrue,
				qt.Commentf("expected sentinel %v, got %v", tc.sentinel, err))
		})
	}
}

func TestExtractIntegerArg(t *testing.T) {
	c := qt.New(t)

	t.Run("success cases", func(t *testing.T) {
		tcs := []struct {
			name        string
			input       values.Value
			wantVal     int64
			wantInexact bool
		}{
			{"integer 42", values.NewInteger(42), 42, false},
			{"integer 0", values.NewInteger(0), 0, false},
			{"integer negative", values.NewInteger(-7), -7, false},
			{"big integer fits int64", values.NewBigIntegerFromInt64(100), 100, false},
			{"float integer-valued", values.NewFloat(5.0), 5, true},
			{"float negative integer-valued", values.NewFloat(-3.0), -3, true},
			{"float zero", values.NewFloat(0.0), 0, true},
		}

		for _, tc := range tcs {
			t.Run(tc.name, func(t *testing.T) {
				val, inexact, err := extractIntegerArg(tc.input, "test")
				c.Assert(err, qt.IsNil)
				c.Assert(val, qt.Equals, tc.wantVal)
				c.Assert(inexact, qt.Equals, tc.wantInexact)
			})
		}
	})

	t.Run("big integer needs big path", func(t *testing.T) {
		huge := new(big.Int).SetUint64(math.MaxUint64)
		bi := values.NewBigInteger(huge)
		_, _, err := extractIntegerArg(bi, "test")
		c.Assert(err, qt.IsNotNil)
		c.Assert(errors.Is(err, errNeedsBigInt), qt.IsTrue)
	})

	t.Run("error cases", func(t *testing.T) {
		errTcs := []struct {
			name  string
			input values.Value
		}{
			{"float NaN", values.NewFloat(math.NaN())},
			{"float +Inf", values.NewFloat(math.Inf(1))},
			{"float -Inf", values.NewFloat(math.Inf(-1))},
			{"float non-integer", values.NewFloat(1.5)},
			{"string", values.NewString("nope")},
			{"boolean", values.FalseValue},
		}

		for _, tc := range errTcs {
			t.Run(tc.name, func(t *testing.T) {
				_, _, err := extractIntegerArg(tc.input, "test")
				c.Assert(err, qt.IsNotNil)
			})
		}
	})
}

func TestOpName(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		op   FoldOp
		want string
	}{
		{"gcd", FoldOpGCD, "gcd"},
		{"lcm", FoldOpLCM, "lcm"},
		{"unknown", FoldOp(99), "unknown"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(opName(tc.op), qt.Equals, tc.want)
		})
	}
}

func TestIntegerFold_GCD_BigInt(t *testing.T) {
	c := qt.New(t)

	huge := new(big.Int).SetUint64(math.MaxUint64)
	bigVal := values.NewBigInteger(huge)

	tcs := []struct {
		name    string
		args    values.Value
		checkFn func(values.Value)
	}{
		{
			"big integer gcd with small",
			// gcd(MaxUint64, 3) -- MaxUint64 = 2^64-1 = 3 * (2^64-1)/3
			// 2^64-1 = 18446744073709551615, 18446744073709551615 / 3 = 6148914691236517205
			// So gcd(MaxUint64, 3) = 3
			values.List(bigVal, values.NewInteger(3)),
			func(v values.Value) {
				bi, ok := v.(*values.BigInteger)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigInteger, got %T", v))
				c.Assert(bi.BigInt().Cmp(big.NewInt(3)), qt.Equals, 0)
			},
		},
		{
			"big integer single element",
			values.List(bigVal),
			func(v values.Value) {
				bi, ok := v.(*values.BigInteger)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigInteger, got %T", v))
				c.Assert(bi.BigInt().Cmp(huge), qt.Equals, 0)
			},
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.args)
			err := IntegerFold(mc, FoldOpGCD, 0, gcdCombiner)
			c.Assert(err, qt.IsNil)
			tc.checkFn(mc.GetValue())
		})
	}
}

func TestIntegerFold_MinInt64(t *testing.T) {
	c := qt.New(t)

	// math.MinInt64 triggers the big path because abs(MinInt64) overflows int64
	args := values.List(values.NewInteger(math.MinInt64), values.NewInteger(2))
	mc := makeMC(args)
	err := IntegerFold(mc, FoldOpGCD, 0, gcdCombiner)
	c.Assert(err, qt.IsNil)

	result := mc.GetValue()
	bi, ok := result.(*values.BigInteger)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigInteger for MinInt64 case, got %T", result))
	c.Assert(bi.BigInt().Cmp(big.NewInt(2)), qt.Equals, 0)
}
