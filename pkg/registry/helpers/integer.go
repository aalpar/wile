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

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
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
			return 0, false, werr.WrapForeignErrorf(werr.ErrNotAnInteger, "%s: expected an integer but got %v", name, n.Value)
		}
		if n.Value != math.Trunc(n.Value) {
			return 0, false, werr.WrapForeignErrorf(werr.ErrNotAnInteger, "%s: expected an integer but got %v", name, n.Value)
		}
		if floatExceedsInt64(n.Value) || n.Value == math.MinInt64 {
			// Promote rather than saturate, the same signal the BigInteger
			// arm above raises for a value that does not fit an int64 — and,
			// for MinInt64, one that fits but cannot be negated, which the
			// fold's absolute value requires.
			return 0, false, errNeedsBigInt
		}
		return int64(n.Value), true, nil
	default:
		return 0, false, werr.WrapForeignErrorf(werr.ErrNotAnInteger, "%s: expected an integer but got %T", name, v)
	}
}

// errNeedsBigInt is a sentinel error indicating we need to use the big.Int path.
var errNeedsBigInt = werr.NewStaticError("needs big int")

// floatExceedsInt64 reports whether an integral float64 lies outside the int64
// range, so that a direct int64(f) conversion would saturate rather than
// convert. Go leaves out-of-range float-to-integer conversion
// implementation-defined; on the platforms this builds for it saturates, which
// turns an unchecked cast into a silently wrong answer.
//
// The upper bound is the power of two, not math.MaxInt64: 2^63-1 is not
// representable as a float64, so the untyped constant rounds UP to 2^63 in a
// float comparison and `f <= math.MaxInt64` admits exactly one value it must
// reject. -2^63 IS representable and converts exactly, so the lower bound is
// the strict one. Same guard, same reason, as integerToFloat64WithAccuracy in
// pkg/values/integer.go.
func floatExceedsInt64(f float64) bool {
	const twoPow63 = 9223372036854775808.0 // 2^63 = MaxInt64 + 1
	return f >= twoPow63 || f < -twoPow63
}

// bigIntFromIntegralFloat converts a finite, integral float64 to a big.Int
// without the saturation an int64 cast would introduce.
//
// Precondition: f is finite. Every caller validates NaN and infinity before
// reaching here; big.Float.SetFloat64 panics on NaN.
func bigIntFromIntegralFloat(f float64) *big.Int {
	q, _ := new(big.Float).SetFloat64(f).Int(nil)
	return q
}

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
	mc machine.CallContext,
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
	pr, ok := o.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "%s: expected a list but got %T", name, o)
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
				return werr.WrapForeignErrorf(werr.ErrNotAnInteger, "%s: expected an integer but got %v", name, v.Value)
			}
			if v.Value != math.Trunc(v.Value) {
				return werr.WrapForeignErrorf(werr.ErrNotAnInteger, "%s: expected an integer but got %v", name, v.Value)
			}
			// This pass is the gate that chooses the int64 or the big.Int
			// fold. An integral float above the int64 range has to open the
			// big gate here; otherwise every later conversion saturates and
			// the fold answers on MaxInt64 instead of the argument.
			//
			// The MinInt64 disjunct is the exact twin of the *values.Integer
			// case just above, and it is NOT covered by floatExceedsInt64:
			// -2^63 is exactly representable and converts exactly, so the
			// range test rightly lets it through. The fold then takes an
			// absolute value, and MinInt64 negates to ITSELF — which is how
			// (gcd -9223372036854775808.0 6) answered -2.0, a negative gcd
			// that R7RS 6.2.6 forbids outright. Two different hazards, one
			// gate.
			if floatExceedsInt64(v.Value) || v.Value == math.MinInt64 {
				hasBigInt = true
			}
			hasInexact = true
		default:
			return werr.WrapForeignErrorf(werr.ErrNotAnInteger, "%s: expected an integer but got %T", name, current.Car())
		}
		cdr := current.Cdr()
		if values.IsEmptyList(cdr) {
			break
		}
		next, ok := cdr.(values.Tuple)
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
	if errors.Is(err, errNeedsBigInt) {
		// Unreachable while the first pass above classifies correctly; kept
		// so the two callers of extractIntegerArg agree on what the sentinel
		// means, rather than one of them leaking it to the user as an error.
		return integerFoldBig(mc, op, identity, pr, hasInexact)
	}
	if err != nil {
		return err
	}
	hasInexact = hasInexact || inexact
	result := firstVal
	if result < 0 {
		result = -result
	}
	restTuple, ok := pr.Cdr().(values.Tuple)
	if !ok {
		if hasInexact {
			mc.SetValue(values.NewFloat(float64(result)))
		} else {
			mc.SetValue(values.NewInteger(result))
		}
		return nil
	}
	err = ForEachList(mc.Context(), restTuple, name, func(_ context.Context, _ int, _ bool, next values.Value) error {
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
	if errors.Is(err, errNeedsBigInt) {
		return integerFoldBig(mc, op, identity, pr, hasInexact)
	}
	if err != nil {
		return err
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
	mc machine.CallContext,
	op FoldOp,
	_ int64,
	pr values.Tuple,
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
		// Float was validated finite and integral by the caller's first pass.
		// It may still exceed int64, which is one of the reasons this path
		// was taken at all, so convert through big.Float rather than casting.
		result = bigIntFromIntegralFloat(v.Value)
	default:
		return werr.WrapForeignErrorf(werr.ErrNotAnInteger, "%s: expected an integer but got %T", name, pr.Car())
	}
	result.Abs(result)

	restTuple, ok := pr.Cdr().(values.Tuple)
	if !ok {
		if hasInexact {
			f, _ := new(big.Float).SetInt(result).Float64()
			mc.SetValue(values.NewFloat(f))
		} else {
			mc.SetValue(values.NewBigInteger(result))
		}
		return nil
	}

	err := ForEachList(mc.Context(), restTuple, name, func(_ context.Context, _ int, _ bool, next values.Value) error {
		var val *big.Int
		switch n := next.(type) {
		case *values.Integer:
			val = big.NewInt(n.Value)
		case *values.BigInteger:
			val = new(big.Int).Set(n.BigInt())
		case *values.Float:
			// Same as the first element above: validated finite and integral,
			// not validated in-range.
			val = bigIntFromIntegralFloat(n.Value)
		default:
			return werr.WrapForeignErrorf(werr.ErrNotAnInteger, "%s: expected an integer but got %T", name, next)
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

	// Return result with appropriate exactness
	if hasInexact {
		f, _ := new(big.Float).SetInt(result).Float64()
		mc.SetValue(values.NewFloat(f))
	} else {
		mc.SetValue(values.NewBigInteger(result))
	}
	return nil
}

// ExtractInteger extracts an integer value from Integer, BigInteger, or Float (if integral).
// Returns (int64Value, bigIntValue, isInexact, error).
// If bigIntValue is non-nil, use that; otherwise use int64Value.
//
// R7RS §6.2.6: Integer operations accept exact and inexact integer arguments.
func ExtractInteger(v values.Value, name string) (int64, *big.Int, bool, error) {
	switch n := v.(type) {
	case *values.Integer:
		return n.Value, nil, false, nil
	case *values.BigInteger:
		return 0, n.BigInt(), false, nil
	case *values.Float:
		// Check if it's an integer value
		if math.IsInf(n.Value, 0) || math.IsNaN(n.Value) {
			return 0, nil, false, werr.WrapForeignErrorf(werr.ErrNotAnInteger, "%s: expected an integer but got %v", name, n.Value)
		}
		if math.Floor(n.Value) != n.Value {
			return 0, nil, false, werr.WrapForeignErrorf(werr.ErrNotAnInteger, "%s: expected an integer but got %v", name, n.Value)
		}
		// Check if it fits in int64
		if !floatExceedsInt64(n.Value) {
			return int64(n.Value), nil, true, nil
		}
		// Large float needs BigInt
		return 0, bigIntFromIntegralFloat(n.Value), true, nil
	default:
		return 0, nil, false, werr.WrapForeignErrorf(werr.ErrNotAnInteger, "%s: expected an integer but got %T", name, v)
	}
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
