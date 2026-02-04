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
	if intA, ok := a.(*values.Integer); ok {
		if floatB, ok := b.(*values.Float); ok {
			return integerEqualsFloat(intA, floatB)
		}
	}
	if intB, ok := b.(*values.Integer); ok {
		if floatA, ok := a.(*values.Float); ok {
			return integerEqualsFloat(intB, floatA)
		}
	}

	// Handle BigInteger vs Float
	if bigA, ok := a.(*values.BigInteger); ok {
		if floatB, ok := b.(*values.Float); ok {
			return bigIntegerEqualsFloat(bigA, floatB)
		}
	}
	if bigB, ok := b.(*values.BigInteger); ok {
		if floatA, ok := a.(*values.Float); ok {
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

// isNonRealComplex returns true if n is a complex number with non-zero imaginary part.
// R7RS §6.2.6: ordering comparisons (<, >, <=, >=) require real arguments.
func isNonRealComplex(n values.Number) bool {
	switch v := n.(type) {
	case *values.Complex:
		return !v.IsReal()
	case *values.BigComplex:
		return !v.IsReal()
	default:
		return false
	}
}

// PrimNumLt implements the < primitive.
//
// R7RS §6.2.6: Ordering comparisons require real arguments.
func PrimNumLt(_ context.Context, mc *machine.MachineContext) error {
	var complexErr error
	err := helpers.NumericChainCompare(mc, "<", func(prev, curr values.Number) bool {
		if isNonRealComplex(prev) || isNonRealComplex(curr) {
			complexErr = values.WrapForeignErrorf(values.ErrNotANumber, "<: requires real arguments")
			return true
		}
		return !prev.LessThan(curr)
	})
	if complexErr != nil {
		return complexErr
	}
	return err
}

// PrimNumGt implements the > primitive.
//
// R7RS §6.2.6: Ordering comparisons require real arguments.
func PrimNumGt(_ context.Context, mc *machine.MachineContext) error {
	var complexErr error
	err := helpers.NumericChainCompare(mc, ">", func(prev, curr values.Number) bool {
		if isNonRealComplex(prev) || isNonRealComplex(curr) {
			complexErr = values.WrapForeignErrorf(values.ErrNotANumber, ">: requires real arguments")
			return true
		}
		return !curr.LessThan(prev)
	})
	if complexErr != nil {
		return complexErr
	}
	return err
}

// PrimNumLe implements the <= primitive.
//
// R7RS §6.2.6: Returns #t if its arguments are monotonically nondecreasing.
// IEEE 754: Any comparison with NaN returns #f.
func PrimNumLe(_ context.Context, mc *machine.MachineContext) error {
	var complexErr error
	err := helpers.NumericChainCompare(mc, "<=", func(prev, curr values.Number) bool {
		if isNonRealComplex(prev) || isNonRealComplex(curr) {
			complexErr = values.WrapForeignErrorf(values.ErrNotANumber, "<=: requires real arguments")
			return true
		}
		// NaN fails all comparisons per IEEE 754
		if helpers.IsNaN(prev) || helpers.IsNaN(curr) {
			return true // fails the comparison
		}
		return curr.LessThan(prev)
	})
	if complexErr != nil {
		return complexErr
	}
	return err
}

// PrimNumGe implements the >= primitive.
//
// R7RS §6.2.6: Returns #t if its arguments are monotonically nonincreasing.
// IEEE 754: Any comparison with NaN returns #f.
func PrimNumGe(_ context.Context, mc *machine.MachineContext) error {
	var complexErr error
	err := helpers.NumericChainCompare(mc, ">=", func(prev, curr values.Number) bool {
		if isNonRealComplex(prev) || isNonRealComplex(curr) {
			complexErr = values.WrapForeignErrorf(values.ErrNotANumber, ">=: requires real arguments")
			return true
		}
		// NaN fails all comparisons per IEEE 754
		if helpers.IsNaN(prev) || helpers.IsNaN(curr) {
			return true // fails the comparison
		}
		return prev.LessThan(curr)
	})
	if complexErr != nil {
		return complexErr
	}
	return err
}

// PrimAbs implements the abs primitive.
// R7RS §6.2.6: For a complex number, abs returns its magnitude.
func PrimAbs(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(v.Abs())
	case *values.BigInteger:
		if v.IsNegative() {
			mc.SetValue(v.Negate())
		} else {
			mc.SetValue(v)
		}
	case *values.Float:
		mc.SetValue(values.NewFloat(math.Abs(v.Value)))
	case *values.Rational:
		mc.SetValue(values.NewRationalFromRat(new(big.Rat).Abs(v.Rat())))
	case *values.BigFloat:
		if v.IsNegative() {
			mc.SetValue(v.Negate())
		} else {
			mc.SetValue(v)
		}
	case *values.Complex:
		// R7RS §6.2.6: For complex numbers, abs returns the magnitude
		mc.SetValue(values.NewFloat(v.Magnitude()))
	case *values.BigComplex:
		// R7RS §6.2.6: For complex numbers, abs returns the magnitude
		mc.SetValue(v.Magnitude())
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "abs: expected a number but got %T", o)
	}
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

// extractInteger extracts an integer value from Integer, BigInteger, or Float (if integral).
// Returns (int64Value, bigIntValue, isInexact, error).
// If bigIntValue is non-nil, use that; otherwise use int64Value.
func extractInteger(v values.Value, name string) (int64, *big.Int, bool, error) {
	switch n := v.(type) {
	case *values.Integer:
		return n.Value, nil, false, nil
	case *values.BigInteger:
		return 0, n.BigInt(), false, nil
	case *values.Float:
		// Check if it's an integer value
		if math.IsInf(n.Value, 0) || math.IsNaN(n.Value) {
			return 0, nil, false, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %v", name, n.Value)
		}
		if math.Floor(n.Value) != n.Value {
			return 0, nil, false, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %v", name, n.Value)
		}
		// Check if it fits in int64
		if n.Value >= -9223372036854775808 && n.Value <= 9223372036854775807 {
			return int64(n.Value), nil, true, nil
		}
		// Large float needs BigInt
		bf := new(big.Float).SetFloat64(n.Value)
		bi, _ := bf.Int(nil)
		return 0, bi, true, nil
	default:
		return 0, nil, false, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %T", name, v)
	}
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
	v0, big0, inexact0, err := extractInteger(o0, name)
	if err != nil {
		return err
	}
	v1, big1, inexact1, err := extractInteger(o1, name)
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
			return values.NewForeignError(name + ": division by zero")
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
		return values.NewForeignError(name + ": division by zero")
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

// PrimModulo implements the modulo primitive.
// Returns the modulo of two integers with the sign of the divisor.
// Accepts exact and inexact integers per R7RS.
func PrimModulo(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)

	// Extract integer values, tracking inexactness
	v0, big0, inexact0, err := extractInteger(o0, "modulo")
	if err != nil {
		return err
	}
	v1, big1, inexact1, err := extractInteger(o1, "modulo")
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
			return values.NewForeignError("modulo: division by zero")
		}
		result := new(big.Int).Rem(b0, b1)
		// Adjust result to have the same sign as b1 (Scheme semantics)
		if (result.Sign() < 0 && b1.Sign() > 0) || (result.Sign() > 0 && b1.Sign() < 0) {
			result.Add(result, b1)
		}
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
		return values.NewForeignError("modulo: division by zero")
	}
	result := v0 % v1
	// Adjust result to have the same sign as v1 (Scheme semantics)
	if (result < 0 && v1 > 0) || (result > 0 && v1 < 0) {
		result += v1
	}
	if inexact {
		mc.SetValue(values.NewFloat(float64(result)))
	} else {
		mc.SetValue(values.NewInteger(result))
	}
	return nil
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

// floatToExact converts a float64 to its exact representation.
// Returns BigInteger if the float is integral, Rational otherwise.
func floatToExact(f float64) values.Number {
	r := new(big.Rat).SetFloat64(f)
	if r.IsInt() {
		num := r.Num()
		if num.IsInt64() {
			return values.NewBigIntegerFromInt64(num.Int64())
		}
		return values.NewBigInteger(new(big.Int).Set(num))
	}
	return values.NewRationalFromRat(r)
}

// numberToExact converts a Number to its exact representation.
// Exact numbers pass through; inexact numbers are converted.
func numberToExact(n values.Number) values.Number {
	switch v := n.(type) {
	case *values.Integer:
		return values.NewBigIntegerFromInt64(v.Value)
	case *values.BigInteger:
		return v
	case *values.Rational:
		return v
	case *values.Float:
		return floatToExact(v.Value)
	case *values.BigFloat:
		r, _ := v.BigFloatValue().Rat(nil)
		if r.IsInt() {
			return values.NewBigInteger(new(big.Int).Set(r.Num()))
		}
		return values.NewRationalFromRat(r)
	default:
		return values.NewBigIntegerFromInt64(0)
	}
}

// PrimExact implements the (exact) primitive.
// Converts an inexact number to an exact representation.
//
// R7RS §6.2.6: The exact procedure returns an exact representation
// of z that is numerically closest to the argument.
func PrimExact(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Integer, *values.Rational, *values.BigInteger:
		mc.SetValue(v)
	case *values.Float:
		// Convert float to rational
		r := new(big.Rat).SetFloat64(v.Value)
		if r == nil {
			return values.NewForeignError("exact: cannot convert infinity or NaN to exact")
		}
		// If denominator is 1, return Integer instead of Rational
		if r.IsInt() {
			num := r.Num()
			if num.IsInt64() {
				mc.SetValue(values.NewInteger(num.Int64()))
			} else {
				mc.SetValue(values.NewBigInteger(num))
			}
		} else {
			mc.SetValue(values.NewRationalFromRat(r))
		}
	case *values.Complex:
		// R7RS §6.2.6: exact on inexact complex converts both parts
		realPart := floatToExact(v.Real())
		imagPart := floatToExact(v.Imag())
		mc.SetValue(values.NewBigComplex(realPart, imagPart))
	case *values.BigComplex:
		// Already exact if parts are exact; otherwise convert
		if v.IsExact() {
			mc.SetValue(v)
		} else {
			realPart := numberToExact(v.Real())
			imagPart := numberToExact(v.Imag())
			mc.SetValue(values.NewBigComplex(realPart, imagPart))
		}
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "exact: expected a number but got %T", o)
	}
	return nil
}

// PrimInexact implements the (inexact) primitive.
// Converts exact number to inexact.
//
// R7RS §6.2.6: The inexact procedure returns an inexact representation
// of z that is numerically closest to the argument.
func PrimInexact(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(values.NewFloat(float64(v.Value)))
	case *values.Float:
		mc.SetValue(v)
	case *values.Rational:
		mc.SetValue(values.NewFloat(v.Float64()))
	case *values.Complex:
		mc.SetValue(v)
	case *values.BigInteger:
		mc.SetValue(v.ToInexact())
	case *values.BigFloat:
		mc.SetValue(v)
	case *values.BigComplex:
		mc.SetValue(v.ToInexact())
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "inexact: expected a number but got %T", o)
	}
	return nil
}
