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

package primitives

import (
	"context"
	"math"
	"math/big"

	"wile/machine"
	"wile/values"
)

type realNumberOp struct {
	name         string
	integerOp    func(*values.Integer) values.Value
	bigIntegerOp func(*values.BigInteger) values.Value
	floatOp      func(float64) float64
	rationalOp   func(*values.Rational) values.Value
}

func makeRealNumberPrimitive(op realNumberOp) func(context.Context, *machine.MachineContext) error {
	return func(_ context.Context, mc *machine.MachineContext) error {
		o := mc.Arg(0)
		switch v := o.(type) {
		case *values.Integer:
			mc.SetValue(op.integerOp(v))
		case *values.BigInteger:
			if op.bigIntegerOp != nil {
				mc.SetValue(op.bigIntegerOp(v))
			} else {
				// Default: convert to float
				mc.SetValue(values.NewFloat(op.floatOp(v.ToInexact().(*values.Float).Datum())))
			}
		case *values.Float:
			mc.SetValue(values.NewFloat(op.floatOp(v.Value)))
		case *values.Rational:
			mc.SetValue(op.rationalOp(v))
		default:
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a real number but got %T", op.name, o)
		}
		return nil
	}
}

func integerPassthrough(v *values.Integer) values.Value { return v }
func rationalToFloat(f func(float64) float64) func(*values.Rational) values.Value {
	return func(v *values.Rational) values.Value {
		return values.NewFloat(f(v.Float64()))
	}
}

var PrimCeiling = makeRealNumberPrimitive(realNumberOp{
	name:       "ceiling",
	integerOp:  integerPassthrough,
	floatOp:    math.Ceil,
	rationalOp: rationalToFloat(math.Ceil),
})

var PrimFloor = makeRealNumberPrimitive(realNumberOp{
	name:       "floor",
	integerOp:  integerPassthrough,
	floatOp:    math.Floor,
	rationalOp: rationalToFloat(math.Floor),
})

var PrimTruncate = makeRealNumberPrimitive(realNumberOp{
	name:       "truncate",
	integerOp:  integerPassthrough,
	floatOp:    math.Trunc,
	rationalOp: rationalToFloat(math.Trunc),
})

var PrimRound = makeRealNumberPrimitive(realNumberOp{
	name:       "round",
	integerOp:  integerPassthrough,
	floatOp:    math.Round,
	rationalOp: rationalToFloat(math.Round),
})

var PrimAbs = makeRealNumberPrimitive(realNumberOp{
	name: "abs",
	integerOp: func(v *values.Integer) values.Value {
		if v.Value < 0 {
			return values.NewInteger(-v.Value)
		}
		return v
	},
	bigIntegerOp: func(v *values.BigInteger) values.Value {
		if v.IsNegative() {
			return v.Negate()
		}
		return v
	},
	floatOp: math.Abs,
	rationalOp: func(v *values.Rational) values.Value {
		return values.NewRationalFromRat(new(big.Rat).Abs(v.Rat()))
	},
})
