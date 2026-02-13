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

package core

import (
	"context"
	"math"
	"math/big"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
)

// PrimAdd implements the + primitive.
func PrimAdd(_ context.Context, mc *machine.MachineContext) error {
	return helpers.NumericFoldVariadic(mc, "+", values.NewInteger(0),
		func(acc, val values.Number) values.Number { return acc.Add(val) })
}

// PrimSub implements the - primitive.
func PrimSub(_ context.Context, mc *machine.MachineContext) error {
	return helpers.NumericFoldWithFirst(mc, "-",
		func(val values.Number) values.Number { return values.NewInteger(0).Subtract(val) },
		func(acc, val values.Number) values.Number { return acc.Subtract(val) })
}

// PrimMul implements the * primitive.
func PrimMul(_ context.Context, mc *machine.MachineContext) error {
	return helpers.NumericFoldVariadic(mc, "*", values.NewInteger(1),
		func(acc, val values.Number) values.Number { return acc.Multiply(val) })
}

// PrimDiv implements the / primitive.
func PrimDiv(_ context.Context, mc *machine.MachineContext) error {
	return helpers.NumericFoldWithFirst(mc, "/",
		func(val values.Number) values.Number { return values.NewInteger(1).Divide(val) },
		func(acc, val values.Number) values.Number { return acc.Divide(val) })
}

// integerEqualsFloat compares an exact integer to an inexact float.
// Returns true only if the float exactly represents the integer value.
//
// R7RS §6.2.5: Numeric equality must not lose precision. An exact integer
// and an inexact float are equal only if the float exactly represents
// the integer's value.
func integerEqualsFloat(i *values.Integer, f *values.Float) bool {
	// NaN is not equal to anything
	if math.IsNaN(f.Value) {
		return false
	}
	// Infinity cannot equal any integer
	if math.IsInf(f.Value, 0) {
		return false
	}
	// Non-integer floats cannot equal integers
	if f.Value != math.Trunc(f.Value) {
		return false
	}
	// For integers within float64's exact range (|n| <= 2^53), direct compare
	const maxExactFloat64Int = int64(1) << 53
	if i.Value >= -maxExactFloat64Int && i.Value <= maxExactFloat64Int {
		return float64(i.Value) == f.Value
	}
	// For larger integers, convert float to big.Rat and compare exactly
	r := new(big.Rat).SetFloat64(f.Value)
	if r == nil || !r.IsInt() {
		return false
	}
	return r.Num().Int64() == i.Value
}

// bigIntegerEqualsFloat compares a BigInteger to a Float.
// Returns true only if the float exactly represents the BigInteger value.
func bigIntegerEqualsFloat(bi *values.BigInteger, f *values.Float) bool {
	if math.IsNaN(f.Value) || math.IsInf(f.Value, 0) {
		return false
	}
	if f.Value != math.Trunc(f.Value) {
		return false
	}
	r := new(big.Rat).SetFloat64(f.Value)
	if r == nil || !r.IsInt() {
		return false
	}
	return bi.BigInt().Cmp(r.Num()) == 0
}

// numericEquals compares two numbers for equality.
//
// R7RS §6.2.5: The = procedure returns #t if its arguments are numerically
// equal. For IEEE 754 floats: infinities of the same sign are equal,
// NaN is not equal to anything (including itself).
func numericEquals(a, b values.Number) bool {
	// Handle Float specially due to IEEE 754 infinity and NaN
	af, aIsFloat := a.(*values.Float)
	bf, bIsFloat := b.(*values.Float)
	if aIsFloat && bIsFloat {
		// NaN != NaN per IEEE 754
		if math.IsNaN(af.Value) || math.IsNaN(bf.Value) {
			return false
		}
		// Direct comparison handles infinities correctly
		return af.Value == bf.Value
	}

	// Handle Integer vs Float specially to preserve precision
	intA, ok := a.(*values.Integer)
	if ok {
		floatB, ok := b.(*values.Float)
		if ok {
			return integerEqualsFloat(intA, floatB)
		}
	}
	intB, ok := b.(*values.Integer)
	if ok {
		floatA, ok := a.(*values.Float)
		if ok {
			return integerEqualsFloat(intB, floatA)
		}
	}

	// Handle BigInteger vs Float
	bigA, ok := a.(*values.BigInteger)
	if ok {
		floatB, ok := b.(*values.Float)
		if ok {
			return bigIntegerEqualsFloat(bigA, floatB)
		}
	}
	bigB, ok := b.(*values.BigInteger)
	if ok {
		floatA, ok := a.(*values.Float)
		if ok {
			return bigIntegerEqualsFloat(bigB, floatA)
		}
	}

	// For other types, use subtraction
	return a.Subtract(b).IsZero()
}

// PrimNumEq implements the = primitive.
//
// R7RS §6.2.6: Returns #t if its arguments are numerically equal.
func PrimNumEq(_ context.Context, mc *machine.MachineContext) error {
	return helpers.NumericChainCompare(mc, "=", func(prev, curr values.Number) bool {
		return !numericEquals(prev, curr)
	})
}

// PrimNumLt implements the < primitive.
//
// R7RS §6.2.6: Ordering comparisons require real arguments.
func PrimNumLt(_ context.Context, mc *machine.MachineContext) error {
	return helpers.NumericChainCompareReal(mc, "<", func(prev, curr values.Number) bool {
		return !prev.LessThan(curr)
	})
}

// PrimNumGt implements the > primitive.
//
// R7RS §6.2.6: Ordering comparisons require real arguments.
func PrimNumGt(_ context.Context, mc *machine.MachineContext) error {
	return helpers.NumericChainCompareReal(mc, ">", func(prev, curr values.Number) bool {
		return !curr.LessThan(prev)
	})
}

// PrimNumLe implements the <= primitive.
//
// R7RS §6.2.6: Returns #t if its arguments are monotonically nondecreasing.
// IEEE 754: Any comparison with NaN returns #f.
func PrimNumLe(_ context.Context, mc *machine.MachineContext) error {
	return helpers.NumericChainCompareReal(mc, "<=", func(prev, curr values.Number) bool {
		// NaN fails all comparisons per IEEE 754
		if prev.IsNaN() || curr.IsNaN() {
			return true
		}
		return curr.LessThan(prev)
	})
}

// PrimNumGe implements the >= primitive.
//
// R7RS §6.2.6: Returns #t if its arguments are monotonically nonincreasing.
// IEEE 754: Any comparison with NaN returns #f.
func PrimNumGe(_ context.Context, mc *machine.MachineContext) error {
	return helpers.NumericChainCompareReal(mc, ">=", func(prev, curr values.Number) bool {
		// NaN fails all comparisons per IEEE 754
		if prev.IsNaN() || curr.IsNaN() {
			return true
		}
		return prev.LessThan(curr)
	})
}

// PrimAbs implements the abs primitive.
// R7RS §6.2.6: abs is only defined for real numbers.
func PrimAbs(_ context.Context, mc *machine.MachineContext) error {
	n, err := helpers.RequireArg[values.Number](mc, 0, values.ErrNotANumber, "abs")
	if err != nil {
		return err
	}
	// Reject complex numbers (abs is only defined for real numbers)
	_, isComplex := n.(values.ComplexNumber)
	if isComplex {
		return values.WrapForeignErrorf(values.ErrNotAReal, "abs: argument must be a real number, got complex")
	}
	mc.SetValue(n.Abs())
	return nil
}

// PrimMin implements the min primitive.
func PrimMin(_ context.Context, mc *machine.MachineContext) error {
	return helpers.NumericExtremum(mc, "min", func(candidate, current values.Number) bool {
		return candidate.LessThan(current)
	})
}

// PrimMax implements the max primitive.
func PrimMax(_ context.Context, mc *machine.MachineContext) error {
	return helpers.NumericExtremum(mc, "max", func(candidate, current values.Number) bool {
		return current.LessThan(candidate)
	})
}

// integerDivisionOp is a helper for integer division operations (quotient, remainder, modulo).
// It handles both regular integers and big integers, preserving exactness.
func integerDivisionOp(
	mc *machine.MachineContext,
	name string,
	regularOp func(int64, int64) int64,
	bigOp func(*big.Int, *big.Int, *big.Int) *big.Int,
) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)

	// Extract integer values, tracking inexactness
	v0, big0, inexact0, err := helpers.ExtractInteger(o0, name)
	if err != nil {
		return err
	}
	v1, big1, inexact1, err := helpers.ExtractInteger(o1, name)
	if err != nil {
		return err
	}

	inexact := inexact0 || inexact1

	// Handle BigInteger case
	if big0 != nil || big1 != nil {
		b0 := big0
		if b0 == nil {
			b0 = big.NewInt(v0)
		}
		b1 := big1
		if b1 == nil {
			b1 = big.NewInt(v1)
		}
		if b1.Sign() == 0 {
			return values.WrapForeignErrorf(values.ErrDivisionByZero, "%s: division by zero", name)
		}
		result := bigOp(new(big.Int), b0, b1)
		if inexact {
			f, _ := new(big.Float).SetInt(result).Float64()
			mc.SetValue(values.NewFloat(f))
		} else {
			mc.SetValue(values.NewBigInteger(result))
		}
		return nil
	}

	// Regular integer case
	if v1 == 0 {
		return values.WrapForeignErrorf(values.ErrDivisionByZero, "%s: division by zero", name)
	}
	result := regularOp(v0, v1)
	if inexact {
		mc.SetValue(values.NewFloat(float64(result)))
	} else {
		mc.SetValue(values.NewInteger(result))
	}
	return nil
}

// PrimQuotient implements the (quotient) primitive.
// Returns truncated integer quotient.
// Accepts exact and inexact integers per R7RS.
func PrimQuotient(_ context.Context, mc *machine.MachineContext) error {
	return integerDivisionOp(mc, "quotient",
		func(a, b int64) int64 {
			return a / b
		},
		(*big.Int).Quo)
}

// PrimRemainder implements the (remainder) primitive.
// Returns remainder with sign of dividend.
// Accepts exact and inexact integers per R7RS.
func PrimRemainder(_ context.Context, mc *machine.MachineContext) error {
	return integerDivisionOp(mc, "remainder",
		func(a, b int64) int64 {
			return a % b
		},
		(*big.Int).Rem)
}

// moduloInt computes the Scheme modulo for int64 values.
// The result has the same sign as the divisor (R7RS §6.2.6).
func moduloInt(a, b int64) int64 {
	result := a % b
	if (result < 0 && b > 0) || (result > 0 && b < 0) {
		result += b
	}
	return result
}

// moduloBig computes the Scheme modulo for big.Int values.
// The result has the same sign as the divisor (R7RS §6.2.6).
func moduloBig(z, x, y *big.Int) *big.Int {
	z.Rem(x, y)
	if (z.Sign() < 0 && y.Sign() > 0) || (z.Sign() > 0 && y.Sign() < 0) {
		z.Add(z, y)
	}
	return z
}

// PrimModulo implements the modulo primitive.
// Returns the modulo of two integers with the sign of the divisor.
// Accepts exact and inexact integers per R7RS.
func PrimModulo(_ context.Context, mc *machine.MachineContext) error {
	return integerDivisionOp(mc, "modulo", moduloInt, moduloBig)
}

// PrimGcd implements the gcd primitive.
func PrimGcd(_ context.Context, mc *machine.MachineContext) error {
	return helpers.IntegerFold(mc, helpers.FoldOpGCD, 0, func(a, b int64) (int64, bool) {
		return helpers.GcdInt(a, b), false
	})
}

// PrimLcm implements the lcm primitive.
func PrimLcm(_ context.Context, mc *machine.MachineContext) error {
	return helpers.IntegerFold(mc, helpers.FoldOpLCM, 1, func(acc, val int64) (int64, bool) {
		g := helpers.GcdInt(acc, val)
		if g == 0 {
			return 0, false // lcm(0, 0) = 0
		}
		q := acc / g
		prod := q * val
		if val != 0 && prod/val != q {
			return 0, true // overflow
		}
		return prod, false
	})
}

// PrimExact implements the (exact) primitive.
// Converts an inexact number to an exact representation.
//
// R7RS §6.2.6: The exact procedure returns an exact representation
// of z that is numerically closest to the argument.
func PrimExact(_ context.Context, mc *machine.MachineContext) error {
	n, err := helpers.RequireArg[values.Number](mc, 0, values.ErrNotANumber, "exact")
	if err != nil {
		return err
	}
	mc.SetValue(n.ToExact())
	return nil
}

// PrimInexact implements the (inexact) primitive.
// Converts exact number to inexact.
//
// R7RS §6.2.6: The inexact procedure returns an inexact representation
// of z that is numerically closest to the argument.
func PrimInexact(_ context.Context, mc *machine.MachineContext) error {
	n, err := helpers.RequireArg[values.Number](mc, 0, values.ErrNotANumber, "inexact")
	if err != nil {
		return err
	}
	mc.SetValue(n.ToInexact())
	return nil
}
