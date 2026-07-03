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

package math

import (
	"math"
	"math/big"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// roundMode selects the rounding direction for floor/ceiling/truncate/round.
// The exact (Rational) and big.Float (BigFloat) paths share these modes so that
// rounding never round-trips through float64, which would drop digits beyond
// float64 precision and overflow the int64 cast on large magnitudes.
type roundMode int

const (
	roundFloor roundMode = iota
	roundCeiling
	roundTruncate
	roundNearestEven
)

// realNumberOp and makeRealNumberPrimitive for floor/ceiling/truncate/round.
// floatOp is the float64-native rounding used for Float inputs; mode drives the
// exact (Rational) and big.Float (BigFloat) paths.
type realNumberOp struct {
	name    string
	mode    roundMode
	floatOp func(float64) float64
}

func makeRealNumberPrimitive(op realNumberOp) func(machine.CallContext) error {
	return func(mc machine.CallContext) error {
		o := mc.Arg(0)
		switch v := o.(type) {
		case *values.Integer:
			mc.SetValue(v) // rounding an integer is identity
		case *values.BigInteger:
			mc.SetValue(v) // rounding an integer is identity, and stays exact
		case *values.Float:
			mc.SetValue(values.NewFloat(op.floatOp(v.Value)))
		case *values.BigFloat:
			mc.SetValue(roundBigFloat(v, op.mode))
		case *values.Rational:
			mc.SetValue(roundRational(v, op.mode))
		default:
			return werr.WrapForeignErrorf(werr.ErrNotAReal, "%s: expected a real number but got %T", op.name, o)
		}
		return nil
	}
}

// roundRational rounds an exact rational to an exact integer per mode.
// R7RS §6.2.6: floor/ceiling/truncate/round on an exact input return an exact
// integer.
func roundRational(v *values.Rational, mode roundMode) values.Value {
	return exactIntValue(roundRatToInt(v.Rat(), mode))
}

// roundBigFloat rounds an inexact BigFloat to an integer-valued BigFloat per
// mode, computing at the input's precision instead of truncating to float64.
func roundBigFloat(v *values.BigFloat, mode roundMode) values.Value {
	bf := v.BigFloatValue()
	if bf.IsInf() {
		return v // floor/ceiling/truncate/round of ±inf is ±inf
	}
	// A BigFloat is an exact dyadic rational, so round it exactly then re-widen.
	r, _ := bf.Rat(nil)
	rounded := new(big.Float).SetPrec(bf.Prec()).SetInt(roundRatToInt(r, mode))
	return values.NewBigFloat(rounded)
}

// roundRatToInt rounds a big.Rat to a big.Int per mode. den is always positive
// for a normalized big.Rat, so DivMod yields the floor and a modulus in
// [0, den); the other modes adjust from there. roundNearestEven implements
// R7RS round-to-even.
func roundRatToInt(r *big.Rat, mode roundMode) *big.Int {
	num := r.Num()
	den := r.Denom()
	f := new(big.Int)
	m := new(big.Int)
	f.DivMod(num, den, m) // f = floor(num/den); 0 <= m < den

	switch mode {
	case roundCeiling:
		if m.Sign() != 0 {
			f.Add(f, big.NewInt(1))
		}
	case roundTruncate:
		// Toward zero: floor overshoots by one for negatives with a fraction.
		if num.Sign() < 0 && m.Sign() != 0 {
			f.Add(f, big.NewInt(1))
		}
	case roundNearestEven:
		twoM := new(big.Int).Lsh(m, 1)
		cmp := twoM.Cmp(den)
		if cmp > 0 || (cmp == 0 && f.Bit(0) == 1) {
			f.Add(f, big.NewInt(1))
		}
	}
	return f
}

// exactIntValue wraps a big.Int as the narrowest exact integer value.
func exactIntValue(i *big.Int) values.Value {
	if i.IsInt64() {
		return values.NewInteger(i.Int64())
	}
	return values.NewBigInteger(i)
}

var PrimCeiling = makeRealNumberPrimitive(realNumberOp{
	name:    "ceiling",
	mode:    roundCeiling,
	floatOp: math.Ceil,
})

var PrimFloor = makeRealNumberPrimitive(realNumberOp{
	name:    "floor",
	mode:    roundFloor,
	floatOp: math.Floor,
})

var PrimTruncate = makeRealNumberPrimitive(realNumberOp{
	name:    "truncate",
	mode:    roundTruncate,
	floatOp: math.Trunc,
})

var PrimRound = makeRealNumberPrimitive(realNumberOp{
	name:    "round",
	mode:    roundNearestEven,
	floatOp: math.RoundToEven,
})

// divResult selects which values a real division primitive returns.
type divResult int

const (
	divBoth      divResult = iota // return quotient and remainder as multiple values
	divQuotient                   // return quotient only
	divRemainder                  // return remainder only
)

// realDivision implements the shared logic for floor and truncate division
// families (R7RS §6.2.6). The roundFn parameter selects the rounding mode
// (math.Floor or math.Trunc), and result selects which values to return.
func realDivision(mc machine.CallContext, name string, roundFn func(float64) float64, result divResult) error {
	n0, exact0, err := helpers.ExtractReal(mc.Arg(0), name)
	if err != nil {
		return err
	}
	n1, exact1, err := helpers.ExtractReal(mc.Arg(1), name)
	if err != nil {
		return err
	}

	if n1 == 0 {
		return werr.WrapForeignErrorf(werr.ErrDivisionByZero, "%s: division by zero", name)
	}

	q := roundFn(n0 / n1)
	exact := exact0 && exact1

	switch result {
	case divBoth:
		r := n0 - q*n1
		if exact {
			mc.SetValues(values.NewInteger(int64(q)), values.NewInteger(int64(r)))
		} else {
			mc.SetValues(values.NewFloat(q), values.NewFloat(r))
		}
	case divQuotient:
		if exact {
			mc.SetValue(values.NewInteger(int64(q)))
		} else {
			mc.SetValue(values.NewFloat(q))
		}
	case divRemainder:
		r := n0 - q*n1
		if exact {
			mc.SetValue(values.NewInteger(int64(r)))
		} else {
			mc.SetValue(values.NewFloat(r))
		}
	}
	return nil
}

// PrimFloorDiv implements the (floor/) primitive.
//
// R7RS §6.2.6: Returns two values: floor quotient and floor remainder.
func PrimFloorDiv(mc machine.CallContext) error {
	return realDivision(mc, "floor/", math.Floor, divBoth)
}

// PrimFloorQuotient implements the (floor-quotient) primitive.
//
// R7RS §6.2.6: Returns the floor quotient for any real numbers.
func PrimFloorQuotient(mc machine.CallContext) error {
	return realDivision(mc, "floor-quotient", math.Floor, divQuotient)
}

// PrimFloorRemainder implements the (floor-remainder) primitive.
//
// R7RS §6.2.6: Returns the floor remainder for any real numbers.
func PrimFloorRemainder(mc machine.CallContext) error {
	return realDivision(mc, "floor-remainder", math.Floor, divRemainder)
}

// PrimTruncateDiv implements the truncate/ primitive.
//
// R7RS §6.2.6: Returns two values: truncate quotient and truncate remainder.
func PrimTruncateDiv(mc machine.CallContext) error {
	return realDivision(mc, "truncate/", math.Trunc, divBoth)
}

// PrimTruncateQuotient implements the truncate-quotient primitive.
//
// R7RS §6.2.6: Returns the truncate quotient for any real numbers.
func PrimTruncateQuotient(mc machine.CallContext) error {
	return realDivision(mc, "truncate-quotient", math.Trunc, divQuotient)
}

// PrimTruncateRemainder implements the truncate-remainder primitive.
//
// R7RS §6.2.6: Returns the truncate remainder for any real numbers.
func PrimTruncateRemainder(mc machine.CallContext) error {
	return realDivision(mc, "truncate-remainder", math.Trunc, divRemainder)
}

// PrimFiniteQ implements the (finite?) primitive.
func PrimFiniteQ(mc machine.CallContext) error {
	n, ok := mc.Arg(0).(values.Number)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "finite?: expected a number but got %T", mc.Arg(0))
	}
	mc.SetValue(values.BoolToBoolean(n.IsFinite()))
	return nil
}

// PrimInfiniteQ implements the (infinite?) primitive.
func PrimInfiniteQ(mc machine.CallContext) error {
	n, ok := mc.Arg(0).(values.Number)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "infinite?: expected a number but got %T", mc.Arg(0))
	}
	mc.SetValue(values.BoolToBoolean(!n.IsFinite() && !n.IsNaN()))
	return nil
}

// PrimNanQ implements the nan? primitive.
func PrimNanQ(mc machine.CallContext) error {
	n, ok := mc.Arg(0).(values.Number)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "nan?: expected a number but got %T", mc.Arg(0))
	}
	mc.SetValue(values.BoolToBoolean(n.IsNaN()))
	return nil
}
