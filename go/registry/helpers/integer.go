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
	"math/big"

	"wile/machine"
	"wile/values"
)

// FoldOp represents the type of fold operation for integers.
type FoldOp int

const (
	FoldOpGCD FoldOp = iota
	FoldOpLCM
)

// IntegerFold is a helper for integer fold operations (gcd, lcm).
// Takes rest args at index 0, applies absolute value, then folds with combiner.
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
	combiner func(acc, val int64) int64,
) error {
	o := mc.Arg(0)
	pr, ok := o.(*values.Pair)
	if !ok {
		if values.IsEmptyList(o) {
			mc.SetValue(values.NewInteger(identity))
			return nil
		}
		return values.WrapForeignErrorf(values.ErrNotAPair, "%d: expected a list but got %T", op, o)
	}
	if values.IsEmptyList(pr) {
		mc.SetValue(values.NewInteger(identity))
		return nil
	}

	// Check if we have any BigIntegers - if so, use big.Int path
	hasBigInt := false
	current := pr
	for !values.IsEmptyList(current) {
		switch current.Car().(type) {
		case *values.BigInteger:
			hasBigInt = true
		case *values.Integer:
			// ok
		default:
			return values.WrapForeignErrorf(values.ErrNotANumber, "%d: expected an integer but got %T", op, current.Car())
		}
		next, ok := current.Cdr().(*values.Pair)
		if !ok {
			break
		}
		current = next
	}

	if hasBigInt {
		return integerFoldBig(mc, op, identity, pr)
	}

	// All integers are small, use int64 path
	first, ok := pr.Car().(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "%d: expected an integer but got %T", op, pr.Car())
	}
	result := first.Value
	if result < 0 {
		result = -result
	}
	rest, ok := pr.Cdr().(*values.Pair)
	if !ok {
		mc.SetValue(values.NewInteger(result))
		return nil
	}
	v, err := rest.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, next values.Value) error {
		n, ok := next.(*values.Integer)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "%d: expected an integer but got %T", op, next)
		}
		val := n.Value
		if val < 0 {
			val = -val
		}
		result = combiner(result, val)
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "%d: not a proper list", op)
	}
	mc.SetValue(values.NewInteger(result))
	return nil
}

// integerFoldBig handles gcd/lcm with BigInteger support using big.Int.
func integerFoldBig(
	mc *machine.MachineContext,
	op FoldOp,
	_ int64,
	pr *values.Pair,
) error {
	// Get the first value
	var result *big.Int
	switch v := pr.Car().(type) {
	case *values.Integer:
		result = big.NewInt(v.Value)
	case *values.BigInteger:
		result = new(big.Int).Set(v.BigInt())
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "%d: expected an integer but got %T", op, pr.Car())
	}
	result.Abs(result)

	rest, ok := pr.Cdr().(*values.Pair)
	if !ok {
		mc.SetValue(values.NewBigInteger(result))
		return nil
	}

	v, err := rest.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, next values.Value) error {
		var val *big.Int
		switch n := next.(type) {
		case *values.Integer:
			val = big.NewInt(n.Value)
		case *values.BigInteger:
			val = new(big.Int).Set(n.BigInt())
		default:
			return values.WrapForeignErrorf(values.ErrNotANumber, "%d: expected an integer but got %T", op, next)
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
		return values.WrapForeignErrorf(values.ErrNotAList, "%d: not a proper list", op)
	}

	// Return BigInteger for the result
	mc.SetValue(values.NewBigInteger(result))
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
