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

	"wile/machine"
	"wile/registry/helpers"
	"wile/values"
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
	// For mixed types or non-Float, use subtraction
	// (works correctly except for Float infinities, already handled)
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
func PrimNumLt(_ context.Context, mc *machine.MachineContext) error {
	return helpers.NumericChainCompare(mc, "<", func(prev, curr values.Number) bool {
		return !prev.LessThan(curr)
	})
}

// PrimNumGt implements the > primitive.
func PrimNumGt(_ context.Context, mc *machine.MachineContext) error {
	return helpers.NumericChainCompare(mc, ">", func(prev, curr values.Number) bool {
		return !curr.LessThan(prev)
	})
}

// PrimNumLe implements the <= primitive.
//
// R7RS §6.2.6: Returns #t if its arguments are monotonically nondecreasing.
// IEEE 754: Any comparison with NaN returns #f.
func PrimNumLe(_ context.Context, mc *machine.MachineContext) error {
	return helpers.NumericChainCompare(mc, "<=", func(prev, curr values.Number) bool {
		// NaN fails all comparisons per IEEE 754
		if helpers.IsNaN(prev) || helpers.IsNaN(curr) {
			return true // fails the comparison
		}
		return curr.LessThan(prev)
	})
}

// PrimNumGe implements the >= primitive.
//
// R7RS §6.2.6: Returns #t if its arguments are monotonically nonincreasing.
// IEEE 754: Any comparison with NaN returns #f.
func PrimNumGe(_ context.Context, mc *machine.MachineContext) error {
	return helpers.NumericChainCompare(mc, ">=", func(prev, curr values.Number) bool {
		// NaN fails all comparisons per IEEE 754
		if helpers.IsNaN(prev) || helpers.IsNaN(curr) {
			return true // fails the comparison
		}
		return prev.LessThan(curr)
	})
}

// PrimAbs implements the abs primitive.
func PrimAbs(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Integer:
		if v.Value < 0 {
			mc.SetValue(values.NewInteger(-v.Value))
		} else {
			mc.SetValue(v)
		}
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
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "abs: expected a real number but got %T", o)
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

// PrimQuotient implements the (quotient) primitive.
// Returns truncated integer quotient.
// Accepts exact and inexact integers per R7RS.
func PrimQuotient(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)

	// Extract integer values, tracking inexactness
	v0, big0, inexact0, err := extractInteger(o0, "quotient")
	if err != nil {
		return err
	}
	v1, big1, inexact1, err := extractInteger(o1, "quotient")
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
			return values.NewForeignError("quotient: division by zero")
		}
		result := new(big.Int).Quo(b0, b1)
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
		return values.NewForeignError("quotient: division by zero")
	}
	result := v0 / v1
	if inexact {
		mc.SetValue(values.NewFloat(float64(result)))
	} else {
		mc.SetValue(values.NewInteger(result))
	}
	return nil
}

// PrimRemainder implements the (remainder) primitive.
// Returns remainder with sign of dividend.
// Accepts exact and inexact integers per R7RS.
func PrimRemainder(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)

	// Extract integer values, tracking inexactness
	v0, big0, inexact0, err := extractInteger(o0, "remainder")
	if err != nil {
		return err
	}
	v1, big1, inexact1, err := extractInteger(o1, "remainder")
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
			return values.NewForeignError("remainder: division by zero")
		}
		result := new(big.Int).Rem(b0, b1)
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
		return values.NewForeignError("remainder: division by zero")
	}
	result := v0 % v1
	if inexact {
		mc.SetValue(values.NewFloat(float64(result)))
	} else {
		mc.SetValue(values.NewInteger(result))
	}
	return nil
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
	return helpers.IntegerFold(mc, helpers.FoldOpGCD, 0, helpers.GcdInt)
}

// PrimLcm implements the lcm primitive.
func PrimLcm(_ context.Context, mc *machine.MachineContext) error {
	return helpers.IntegerFold(mc, helpers.FoldOpLCM, 1, func(acc, val int64) int64 {
		g := helpers.GcdInt(acc, val)
		if g == 0 {
			return 0 // lcm(0, 0) = 0
		}
		return acc / g * val
	})
}

// PrimExact implements the (exact) primitive.
// Converts an inexact number to an exact representation.
func PrimExact(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Integer, *values.Rational:
		mc.SetValue(v)
	case *values.Float:
		// Convert float to rational
		r := new(big.Rat).SetFloat64(v.Value)
		if r == nil {
			return values.NewForeignError("exact: cannot convert infinity or NaN to exact")
		}
		mc.SetValue(values.NewRationalFromRat(r))
	case *values.Complex:
		return values.NewForeignError("exact: complex numbers not supported")
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "exact: expected a number but got %T", o)
	}
	return nil
}

// PrimInexact implements the (inexact) primitive.
// Converts exact number to inexact.
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
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "inexact: expected a number but got %T", o)
	}
	return nil
}
