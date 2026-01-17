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
	"math/cmplx"

	"wile/machine"
	"wile/values"
)

// PrimExpt implements the (expt) primitive.
// Returns base raised to the exponent power.
func PrimExpt(_ context.Context, mc *machine.MachineContext) error {
	base := mc.Arg(0)
	exp := mc.Arg(1)
	baseNum, ok := base.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "expt: expected a number but got %T", base)
	}
	expNum, ok := exp.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "expt: expected a number but got %T", exp)
	}

	// Handle integer exponent cases for exactness preservation
	if expInt, ok := expNum.(*values.Integer); ok {
		e := expInt.Value

		// Integer base with integer exponent
		if baseInt, ok := baseNum.(*values.Integer); ok {
			if e >= 0 {
				result := int64(1)
				b := baseInt.Value
				for e > 0 {
					if e%2 == 1 {
						result *= b
					}
					b *= b
					e /= 2
				}
				mc.SetValue(values.NewInteger(result))
				return nil
			}
			// Negative exponent: return exact Rational 1/base^|exp|
			result := int64(1)
			b := baseInt.Value
			absE := -e
			for absE > 0 {
				if absE%2 == 1 {
					result *= b
				}
				b *= b
				absE /= 2
			}
			mc.SetValue(values.NewRational(1, result))
			return nil
		}

		// BigInteger base with integer exponent
		if baseBig, ok := baseNum.(*values.BigInteger); ok {
			if e >= 0 {
				result := new(big.Int).Exp(baseBig.BigInt(), big.NewInt(e), nil)
				mc.SetValue(values.NewBigInteger(result))
				return nil
			}
			// Negative exponent: return exact Rational 1/base^|exp|
			absE := -e
			denom := new(big.Int).Exp(baseBig.BigInt(), big.NewInt(absE), nil)
			mc.SetValue(values.NewRationalFromBigInt(big.NewInt(1), denom))
			return nil
		}

		// Rational base with integer exponent
		if baseRat, ok := baseNum.(*values.Rational); ok {
			num := baseRat.Num()
			denom := baseRat.Denom()
			if e >= 0 {
				// (num/denom)^e = num^e / denom^e
				numResult := new(big.Int).Exp(num, big.NewInt(e), nil)
				denomResult := new(big.Int).Exp(denom, big.NewInt(e), nil)
				result := values.NewRationalFromBigInt(numResult, denomResult)
				// Simplify to Integer if possible
				if result.IsInteger() {
					mc.SetValue(values.NewInteger(result.NumInt64()))
					return nil
				}
				mc.SetValue(result)
				return nil
			}
			// Negative exponent: (num/denom)^-e = denom^|e| / num^|e|
			absE := -e
			numResult := new(big.Int).Exp(denom, big.NewInt(absE), nil)
			denomResult := new(big.Int).Exp(num, big.NewInt(absE), nil)
			result := values.NewRationalFromBigInt(numResult, denomResult)
			// Simplify to Integer if possible
			if result.IsInteger() {
				mc.SetValue(values.NewInteger(result.NumInt64()))
				return nil
			}
			mc.SetValue(result)
			return nil
		}
	}

	// General case: use float/complex math
	switch b := baseNum.(type) {
	case *values.Complex:
		switch e := expNum.(type) {
		case *values.Complex:
			mc.SetValue(values.NewComplex(cmplx.Pow(b.Value, e.Value)))
		case *values.Float:
			mc.SetValue(values.NewComplex(cmplx.Pow(b.Value, complex(e.Value, 0))))
		case *values.Integer:
			mc.SetValue(values.NewComplex(cmplx.Pow(b.Value, complex(float64(e.Value), 0))))
		case *values.Rational:
			mc.SetValue(values.NewComplex(cmplx.Pow(b.Value, complex(e.Float64(), 0))))
		}
	default:
		var bf float64
		switch v := baseNum.(type) {
		case *values.Integer:
			bf = float64(v.Value)
		case *values.BigInteger:
			bf, _ = new(big.Float).SetInt(v.BigInt()).Float64()
		case *values.Float:
			bf = v.Value
		case *values.Rational:
			bf = v.Float64()
		}
		var ef float64
		switch v := expNum.(type) {
		case *values.Integer:
			ef = float64(v.Value)
		case *values.Float:
			ef = v.Value
		case *values.Rational:
			ef = v.Float64()
		case *values.Complex:
			mc.SetValue(values.NewComplex(cmplx.Pow(complex(bf, 0), v.Value)))
			return nil
		}
		mc.SetValue(values.NewFloat(math.Pow(bf, ef)))
	}
	return nil
}
