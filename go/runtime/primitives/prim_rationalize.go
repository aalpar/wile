package primitives

import (
	"context"
	"math/big"

	"skeme/machine"
	"skeme/values"
)

// PrimRationalize implements the (rationalize) primitive.
// Returns simplest rational within tolerance.
func PrimRationalize(_ context.Context, mc *machine.MachineContext) error {
	xArg := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	yArg := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	var x, y *big.Rat
	var xExact, yExact bool

	switch v := xArg.(type) {
	case *values.Integer:
		x = big.NewRat(v.Value, 1)
		xExact = true
	case *values.Rational:
		x = v.Rat()
		xExact = true
	case *values.Float:
		x = new(big.Rat).SetFloat64(v.Value)
		if x == nil {
			return values.NewForeignError("rationalize: x cannot be infinity or NaN")
		}
		xExact = false
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "rationalize: expected a real number for x but got %T", xArg)
	}

	switch v := yArg.(type) {
	case *values.Integer:
		y = big.NewRat(v.Value, 1)
		yExact = true
	case *values.Rational:
		y = v.Rat()
		yExact = true
	case *values.Float:
		y = new(big.Rat).SetFloat64(v.Value)
		if y == nil {
			return values.NewForeignError("rationalize: y cannot be infinity or NaN")
		}
		yExact = false
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "rationalize: expected a real number for y but got %T", yArg)
	}

	if y.Sign() < 0 {
		y = new(big.Rat).Abs(y)
	}

	result := rationalizeSternBrocot(x, y)

	if xExact && yExact {
		if result.IsInt() {
			num := result.Num()
			if num.IsInt64() {
				mc.SetValue(values.NewInteger(num.Int64()))
			} else {
				mc.SetValue(values.NewRationalFromRat(result))
			}
		} else {
			mc.SetValue(values.NewRationalFromRat(result))
		}
	} else {
		f, _ := result.Float64()
		mc.SetValue(values.NewFloat(f))
	}
	return nil
}

func rationalizeSternBrocot(x, y *big.Rat) *big.Rat {
	if y.Sign() == 0 {
		return new(big.Rat).Set(x)
	}

	lo := new(big.Rat).Sub(x, y)
	hi := new(big.Rat).Add(x, y)

	if lo.Sign() > 0 {
		return rationalizePositive(lo, hi)
	} else if hi.Sign() < 0 {
		negHi := new(big.Rat).Neg(hi)
		negLo := new(big.Rat).Neg(lo)
		result := rationalizePositive(negHi, negLo)
		return new(big.Rat).Neg(result)
	} else {
		return big.NewRat(0, 1)
	}
}

func rationalizePositive(lo, hi *big.Rat) *big.Rat {
	aNum, aDenom := big.NewInt(0), big.NewInt(1)
	bNum, bDenom := big.NewInt(1), big.NewInt(0)

	one := big.NewRat(1, 1)
	for {
		if lo.Cmp(one) > 0 {
			k := floorRat(lo)
			lo.Sub(lo, k)
			hi.Sub(hi, k)
			kInt := k.Num()
			aNum.Add(aNum, new(big.Int).Mul(kInt, bNum))
			aDenom.Add(aDenom, new(big.Int).Mul(kInt, bDenom))
		}

		if hi.Cmp(one) >= 0 {
			result := new(big.Rat)
			num := new(big.Int).Add(aNum, bNum)
			denom := new(big.Int).Add(aDenom, bDenom)
			result.SetFrac(num, denom)
			return result
		}

		lo.Inv(lo)
		hi.Inv(hi)
		lo, hi = hi, lo
		aNum, bNum = bNum, aNum
		aDenom, bDenom = bDenom, aDenom
	}
}

func floorRat(r *big.Rat) *big.Rat {
	q := new(big.Int).Div(r.Num(), r.Denom())
	return new(big.Rat).SetInt(q)
}
