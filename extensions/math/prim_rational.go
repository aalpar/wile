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
	"math/big"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// PrimNumerator implements the numerator primitive.
func PrimNumerator(mc machine.CallContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(v)
	case *values.Rational:
		num := v.Num()
		if num.IsInt64() {
			mc.SetValue(values.NewInteger(num.Int64()))
		} else {
			mc.SetValue(values.NewRationalFromBigInt(num, big.NewInt(1)))
		}
	case *values.Float:
		// R7RS §6.2.6: inexact input → inexact output
		r := new(big.Rat).SetFloat64(v.Value)
		if r == nil {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "numerator: cannot get numerator of infinity or NaN")
		}
		num := r.Num()
		f, _ := new(big.Float).SetInt(num).Float64()
		mc.SetValue(values.NewFloat(f))
	case *values.BigFloat:
		// R7RS §6.2.6: inexact input → inexact output
		r, _ := v.BigFloatValue().Rat(nil)
		if r == nil {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "numerator: cannot get numerator of infinity or NaN")
		}
		num := r.Num()
		f := new(big.Float).SetInt(num)
		mc.SetValue(values.NewBigFloat(f))
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "numerator: expected a rational number but got %T", o)
	}
	return nil
}

// PrimDenominator implements the (denominator) primitive.
func PrimDenominator(mc machine.CallContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(values.NewInteger(1))
	case *values.Rational:
		denom := v.Denom()
		if denom.IsInt64() {
			mc.SetValue(values.NewInteger(denom.Int64()))
		} else {
			mc.SetValue(values.NewRationalFromBigInt(denom, big.NewInt(1)))
		}
	case *values.Float:
		// R7RS §6.2.6: inexact input → inexact output
		r := new(big.Rat).SetFloat64(v.Value)
		if r == nil {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "denominator: cannot get denominator of infinity or NaN")
		}
		denom := r.Denom()
		f, _ := new(big.Float).SetInt(denom).Float64()
		mc.SetValue(values.NewFloat(f))
	case *values.BigFloat:
		// R7RS §6.2.6: inexact input → inexact output
		r, _ := v.BigFloatValue().Rat(nil)
		if r == nil {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "denominator: cannot get denominator of infinity or NaN")
		}
		denom := r.Denom()
		f := new(big.Float).SetInt(denom)
		mc.SetValue(values.NewBigFloat(f))
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "denominator: expected a rational number but got %T", o)
	}
	return nil
}

// PrimRationalize implements the (rationalize) primitive.
func PrimRationalize(mc machine.CallContext) error {
	xArg := mc.Arg(0)
	yArg := mc.Arg(1)

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
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "rationalize: x cannot be infinity or NaN")
		}
		xExact = false
	default:
		return werr.WrapForeignErrorf(werr.ErrNotAReal, "rationalize: expected a real number for x but got %T", xArg)
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
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "rationalize: y cannot be infinity or NaN")
		}
		yExact = false
	default:
		return werr.WrapForeignErrorf(werr.ErrNotAReal, "rationalize: expected a real number for y but got %T", yArg)
	}

	if y.Sign() < 0 {
		y = new(big.Rat).Abs(y)
	}

	result := rationalizeSternBrocot(x, y)

	if !xExact || !yExact {
		f, _ := result.Float64()
		mc.SetValue(values.NewFloat(f))
		return nil
	}
	if !result.IsInt() {
		mc.SetValue(values.NewRationalFromRat(result))
		return nil
	}
	num := result.Num()
	if num.IsInt64() {
		mc.SetValue(values.NewInteger(num.Int64()))
		return nil
	}
	mc.SetValue(values.NewRationalFromRat(result))
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
	}
	if hi.Sign() < 0 {
		negHi := new(big.Rat).Neg(hi)
		negLo := new(big.Rat).Neg(lo)
		result := rationalizePositive(negHi, negLo)
		return new(big.Rat).Neg(result)
	}
	return big.NewRat(0, 1)
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

// PrimExactIntegerSqrt implements the (exact-integer-sqrt) primitive.
//
// R7RS §6.2.6: Returns two non-negative exact integers s and r where
// n = s² + r and n < (s+1)².
func PrimExactIntegerSqrt(mc machine.CallContext) error {
	o := mc.Arg(0)

	switch v := o.(type) {
	case *values.Integer:
		if v.Value < 0 {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "exact-integer-sqrt: expected a non-negative integer")
		}
		// big.Int.Sqrt, not a corrected float64 seed. The two int64 correction
		// loops this replaced overflowed: for v >= 3037000499² the product
		// (s+1)*(s+1) wraps negative, so the loop walked past the true root
		// and returned a pair that satisfies n = s² + r only MODULO 2^64 —
		// (exact-integer-sqrt 9223372030926249001) answered
		// (14564942733 26161807488), violating R7RS §6.2.6 in true arithmetic.
		// At MaxInt64 the wrapped guard is true for every int64 and it never
		// returned at all. Same computation as the *BigInteger arm below,
		// which was correct at these magnitudes all along; the two now agree
		// by running one algorithm rather than by being kept in step.
		//
		// Both outputs fit int64 for any non-negative int64 input: s is at
		// most 3037000499 and r is at most 2s.
		bv := big.NewInt(v.Value)
		s := new(big.Int).Sqrt(bv)
		r := new(big.Int).Sub(bv, new(big.Int).Mul(s, s))
		mc.SetValues(values.NewInteger(s.Int64()), values.NewInteger(r.Int64()))
		return nil

	case *values.BigInteger:
		if v.BigInt().Sign() < 0 {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "exact-integer-sqrt: expected a non-negative integer")
		}
		// Use big.Int.Sqrt which computes floor(sqrt(n))
		s := new(big.Int).Sqrt(v.BigInt())
		// Compute remainder: r = n - s²
		sSquared := new(big.Int).Mul(s, s)
		r := new(big.Int).Sub(v.BigInt(), sSquared)
		mc.SetValues(values.NewBigInteger(s), values.NewBigInteger(r))
		return nil

	default:
		return werr.WrapForeignErrorf(werr.ErrNotAnInteger, "exact-integer-sqrt: expected an exact integer but got %T", o)
	}
}
