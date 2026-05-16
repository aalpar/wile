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

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// realNumberOp and makeRealNumberPrimitive for floor/ceiling/truncate/round
type realNumberOp struct {
	name         string
	integerOp    func(*values.Integer) values.Value
	bigIntegerOp func(*values.BigInteger) values.Value
	floatOp      func(float64) float64
	rationalOp   func(*values.Rational) values.Value
}

func makeRealNumberPrimitive(op realNumberOp) func(machine.CallContext) error {
	return func(mc machine.CallContext) error {
		o := mc.Arg(0)
		switch v := o.(type) {
		case *values.Integer:
			mc.SetValue(op.integerOp(v))
		case *values.BigInteger:
			if op.bigIntegerOp != nil {
				mc.SetValue(op.bigIntegerOp(v))
			} else {
				mc.SetValue(values.NewFloat(op.floatOp(v.ToInexact().(*values.Float).Value)))
			}
		case *values.Float:
			mc.SetValue(values.NewFloat(op.floatOp(v.Value)))
		case *values.BigFloat:
			mc.SetValue(values.NewBigFloatFromFloat64(op.floatOp(v.Float64Truncated())))
		case *values.Rational:
			mc.SetValue(op.rationalOp(v))
		default:
			return werr.WrapForeignErrorf(werr.ErrNotAReal, "%s: expected a real number but got %T", op.name, o)
		}
		return nil
	}
}

func integerPassthrough(v *values.Integer) values.Value {
	return v
}

// rationalToInteger returns an exact integer for exact rational inputs.
// R7RS §6.2.6: floor, ceiling, truncate, round return integers.
// When the input is exact (like a rational), the result must also be exact.
func rationalToInteger(f func(float64) float64) func(*values.Rational) values.Value {
	return func(v *values.Rational) values.Value {
		return values.NewInteger(int64(f(v.Float64Truncated())))
	}
}

var PrimCeiling = makeRealNumberPrimitive(realNumberOp{
	name:       "ceiling",
	integerOp:  integerPassthrough,
	floatOp:    math.Ceil,
	rationalOp: rationalToInteger(math.Ceil),
})

var PrimFloor = makeRealNumberPrimitive(realNumberOp{
	name:       "floor",
	integerOp:  integerPassthrough,
	floatOp:    math.Floor,
	rationalOp: rationalToInteger(math.Floor),
})

var PrimTruncate = makeRealNumberPrimitive(realNumberOp{
	name:       "truncate",
	integerOp:  integerPassthrough,
	floatOp:    math.Trunc,
	rationalOp: rationalToInteger(math.Trunc),
})

var PrimRound = makeRealNumberPrimitive(realNumberOp{
	name:       "round",
	integerOp:  integerPassthrough,
	floatOp:    math.RoundToEven,
	rationalOp: rationalToInteger(math.RoundToEven),
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
