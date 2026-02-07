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

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
)

// FoldOp represents the type of fold operation for integers.
type FoldOp int

const (
	FoldOpGCD FoldOp = iota
	FoldOpLCM
)

// opName returns the Scheme name for the fold operation.
func opName(op FoldOp) string {
	switch op {
	case FoldOpGCD:
		return "gcd"
	case FoldOpLCM:
		return "lcm"
	default:
		return "unknown"
	}
}

// extractIntegerArg extracts an integer value from a Scheme value.
// Returns the int64 value, whether it was inexact, and any error.
// Accepts Integer, BigInteger, or Float (if it represents a whole number).
//
// R7RS §6.2.6: gcd and lcm accept either exact or inexact integer arguments.
func extractIntegerArg(v values.Value, name string) (int64, bool, error) {
	switch n := v.(type) {
	case *values.Integer:
		return n.Value, false, nil
	case *values.BigInteger:
		// For BigIntegers that fit in int64, we can still process them
		// But we'll handle the big path separately
		if n.BigInt().IsInt64() {
			return n.BigInt().Int64(), false, nil
		}
		// Signal that we need the big.Int path by returning a special error
		return 0, false, errNeedsBigInt
	case *values.Float:
		// Check if the float represents an integer
		if math.IsNaN(n.Value) || math.IsInf(n.Value, 0) {
			return 0, false, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %v", name, n.Value)
		}
		if n.Value != math.Trunc(n.Value) {
			return 0, false, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %v", name, n.Value)
		}
		return int64(n.Value), true, nil
	default:
		return 0, false, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %T", name, v)
	}
}

// errNeedsBigInt is a sentinel error indicating we need to use the big.Int path.
var errNeedsBigInt = values.NewForeignError("needs big int")

// IntegerFold is a helper for integer fold operations (gcd, lcm).
// Takes rest args at index 0, applies absolute value, then folds with combiner.
//
// R7RS §6.2.6: gcd and lcm accept either exact or inexact integer arguments
// and always return an integer. If any argument is inexact, the result is inexact.
//
// The fold pattern combines a list into a single value using a binary operation:
//
//	fold(f, identity, [a, b, c]) = f(f(f(identity, a), b), c)
//
// See SRFI-1 (List Library) for the canonical Scheme definition of fold:
//
//	https://srfi.schemers.org/srfi-1/srfi-1.html
func IntegerFold(
	mc *machine.MachineContext,
	op FoldOp,
	identity int64,
	combiner func(acc, val int64) (int64, bool),
) error {
	name := opName(op)
	o := mc.Arg(0)
	if values.IsEmptyList(o) {
		mc.SetValue(values.NewInteger(identity))
		return nil
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "%s: expected a list but got %T", name, o)
	}

	// First pass: check types and detect if we need big.Int path
	hasBigInt := false
	hasInexact := false
	current := pr
	for {
		switch v := current.Car().(type) {
		case *values.BigInteger:
			if !v.BigInt().IsInt64() {
				hasBigInt = true
			}
		case *values.Integer:
			if v.Value == math.MinInt64 {
				hasBigInt = true
			}
		case *values.Float:
			// Check if it represents an integer
			if math.IsNaN(v.Value) || math.IsInf(v.Value, 0) {
				return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %v", name, v.Value)
			}
			if v.Value != math.Trunc(v.Value) {
				return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %v", name, v.Value)
			}
			hasInexact = true
		default:
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %T", name, current.Car())
		}
		next, ok := current.Cdr().(*values.Pair)
		if !ok {
			break
		}
		current = next
	}

	if hasBigInt {
		return integerFoldBig(mc, op, identity, pr, hasInexact)
	}

	// All integers are small, use int64 path
	firstVal, inexact, err := extractIntegerArg(pr.Car(), name)
	if err != nil {
		return err
	}
	hasInexact = hasInexact || inexact
	result := firstVal
	if result < 0 {
		result = -result
	}
	rest, ok := pr.Cdr().(*values.Pair)
	if !ok {
		if hasInexact {
			mc.SetValue(values.NewFloat(float64(result)))
		} else {
			mc.SetValue(values.NewInteger(result))
		}
		return nil
	}
	v, err := rest.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, next values.Value) error {
		val, inexact, err := extractIntegerArg(next, name)
		if err != nil {
			return err
		}
		hasInexact = hasInexact || inexact
		if val < 0 {
			val = -val
		}
		combined, overflow := combiner(result, val)
		if overflow {
			return errNeedsBigInt
		}
		result = combined
		return nil
	})
	if err != nil {
		if errors.Is(err, errNeedsBigInt) {
			return integerFoldBig(mc, op, identity, pr, hasInexact)
		}
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: not a proper list", name)
	}
	if hasInexact {
		mc.SetValue(values.NewFloat(float64(result)))
	} else {
		mc.SetValue(values.NewInteger(result))
	}
	return nil
}

// integerFoldBig handles gcd/lcm with BigInteger support using big.Int.
// Also handles inexact results when any argument was inexact.
func integerFoldBig(
	mc *machine.MachineContext,
	op FoldOp,
	_ int64,
	pr *values.Pair,
	hasInexact bool,
) error {
	name := opName(op)
	// Get the first value
	var result *big.Int
	switch v := pr.Car().(type) {
	case *values.Integer:
		result = big.NewInt(v.Value)
	case *values.BigInteger:
		result = new(big.Int).Set(v.BigInt())
	case *values.Float:
		// Float should have been validated as integer in caller
		result = big.NewInt(int64(v.Value))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %T", name, pr.Car())
	}
	result.Abs(result)

	rest, ok := pr.Cdr().(*values.Pair)
	if !ok {
		if hasInexact {
			f, _ := new(big.Float).SetInt(result).Float64()
			mc.SetValue(values.NewFloat(f))
		} else {
			mc.SetValue(values.NewBigInteger(result))
		}
		return nil
	}

	v, err := rest.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, next values.Value) error {
		var val *big.Int
		switch n := next.(type) {
		case *values.Integer:
			val = big.NewInt(n.Value)
		case *values.BigInteger:
			val = new(big.Int).Set(n.BigInt())
		case *values.Float:
			// Float should have been validated as integer in caller
			val = big.NewInt(int64(n.Value))
		default:
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %T", name, next)
		}
		val.Abs(val)

		switch op {
		case FoldOpGCD:
			result.GCD(nil, nil, result, val)
		case FoldOpLCM:
			// lcm(a, b) = |a * b| / gcd(a, b)
			g := new(big.Int).GCD(nil, nil, result, val)
			if g.Sign() == 0 {
				result.SetInt64(0)
			} else {
				result.Div(result, g)
				result.Mul(result, val)
			}
		}
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: not a proper list", name)
	}

	// Return result with appropriate exactness
	if hasInexact {
		f, _ := new(big.Float).SetInt(result).Float64()
		mc.SetValue(values.NewFloat(f))
	} else {
		mc.SetValue(values.NewBigInteger(result))
	}
	return nil
}

// FloorDivide performs floor division, returning quotient and remainder.
func FloorDivide(n0, n1 int64) (q, r int64) {
	q = n0 / n1
	r = n0 % n1
	// Floor division: quotient rounds toward negative infinity
	// If remainder is non-zero and signs differ, adjust
	if r != 0 && (n0 < 0) != (n1 < 0) {
		q--
		r += n1
	}
	return q, r
}

// GcdInt returns the greatest common divisor of two integers.
func GcdInt(a, b int64) int64 {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}
