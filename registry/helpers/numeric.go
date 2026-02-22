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

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
)

// NumericFoldVariadic is a helper for variadic arithmetic operations (+ and *).
// It takes a rest parameter at index 0 and folds with the binary operation.
// Returns identity for empty list, first arg for single element.
func NumericFoldVariadic(
	mc *machine.MachineContext,
	name string,
	identity values.Number,
	binOp func(acc, val values.Number) values.Number,
) error {
	o := mc.Arg(0)
	if values.IsEmptyList(o) {
		mc.SetValue(identity)
		return nil
	}
	pr, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a list but got %T", name, o)
	}
	o = pr.Car()
	nbr, ok := o.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, o)
	}
	rest, ok := pr.Cdr().(values.Tuple)
	if !ok {
		mc.SetValue(nbr)
		return nil
	}
	// Fast path: two-element list (e.g., (+ a b)).
	if val, ok := values.Single(rest); ok {
		v, ok := val.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, val)
		}
		mc.SetValue(binOp(nbr, v))
		return nil
	}
	v, err := rest.ForEach(mc.Context(), func(_ context.Context, _ int, _ bool, o values.Value) error {
		v, ok := o.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, o)
		}
		nbr = binOp(nbr, v)
		return nil
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "%s: error processing arguments", name)
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a list but got %s", name, v.SchemeString())
	}
	mc.SetValue(nbr)
	return nil
}

// NumericFoldWithFirst is a helper for arithmetic operations with required first arg (- and /).
// First arg at index 0, rest at index 1. Applies unaryOp for single arg case.
func NumericFoldWithFirst(
	mc *machine.MachineContext,
	name string,
	unaryOp func(val values.Number) values.Number,
	binOp func(acc, val values.Number) values.Number,
) error {
	o0 := mc.Arg(0)
	nbr0, ok := o0.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, o0)
	}
	o1 := mc.Arg(1)
	if values.IsEmptyList(o1) {
		mc.SetValue(unaryOp(nbr0))
		return nil
	}
	pr, ok := o1.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a list but got %T", name, o1)
	}
	o2 := pr.Car()
	nbr2, ok := o2.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, o2)
	}
	acc := binOp(nbr0, nbr2)
	if values.IsEmptyList(pr.Cdr()) {
		mc.SetValue(acc)
		return nil
	}
	rest, ok := pr.Cdr().(values.Tuple)
	if !ok {
		mc.SetValue(acc)
		return nil
	}
	v, err := rest.ForEach(mc.Context(), func(_ context.Context, _ int, _ bool, o values.Value) error {
		v, ok := o.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, o)
		}
		acc = binOp(acc, v)
		return nil
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "%s: error processing arguments", name)
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a list but got %s", name, v.SchemeString())
	}
	mc.SetValue(acc)
	return nil
}

// NumericChainCompare is a helper for numeric chain comparison primitives.
// First arg at index 0, rest at index 1. Returns true if all consecutive
// pairs satisfy the comparator, false otherwise.
func NumericChainCompare(
	mc *machine.MachineContext,
	name string,
	fails func(prev, curr values.Number) bool,
) error {
	o0 := mc.Arg(0)
	prev, ok := o0.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, o0)
	}
	rest := mc.Arg(1)
	if values.IsEmptyList(rest) {
		mc.SetValue(values.TrueValue)
		return nil
	}
	pr, ok := rest.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a list but got %T", name, rest)
	}
	// Fast path: single-element rest list (the common 2-argument case).
	if val, ok := values.Single(pr); ok {
		curr, ok := val.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, val)
		}
		mc.SetValue(values.BoolToBoolean(!fails(prev, curr)))
		return nil
	}
	v, err := pr.ForEach(mc.Context(), func(_ context.Context, _ int, _ bool, v values.Value) error {
		curr, ok := v.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, v)
		}
		if fails(prev, curr) {
			return values.ErrCannotCompare
		}
		prev = curr
		return nil
	})
	if errors.Is(err, values.ErrCannotCompare) {
		mc.SetValue(values.FalseValue)
		return nil
	}
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a proper list", name)
	}
	mc.SetValue(values.TrueValue)
	return nil
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

// NumericChainCompareReal is a helper for ordering comparison primitives (<, >, <=, >=).
// It wraps NumericChainCompare with complex-number rejection: any non-real complex
// argument causes an immediate error rather than a boolean #f result.
//
// The fails callback should return true when the comparison fails (i.e. the pair
// does not satisfy the ordering relation), false when it succeeds.
func NumericChainCompareReal(
	mc *machine.MachineContext,
	name string,
	fails func(prev, curr values.Number) bool,
) error {
	// Fast path: 2-argument case avoids the wrapper closure allocation.
	o0 := mc.Arg(0)
	prev, ok := o0.(values.Number)
	if ok {
		rest := mc.Arg(1)
		if !values.IsEmptyList(rest) {
			if pr, ok := rest.(values.Tuple); ok {
				if val, ok := values.Single(pr); ok {
					curr, ok := val.(values.Number)
					if !ok {
						return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, val)
					}
					if isNonRealComplex(prev) || isNonRealComplex(curr) {
						return values.WrapForeignErrorf(values.ErrNotANumber, "%s: requires real arguments", name)
					}
					mc.SetValue(values.BoolToBoolean(!fails(prev, curr)))
					return nil
				}
			}
		}
	}
	// General path: 1 or 3+ arguments.
	var complexErr error
	err := NumericChainCompare(mc, name, func(prev, curr values.Number) bool {
		if isNonRealComplex(prev) || isNonRealComplex(curr) {
			complexErr = values.WrapForeignErrorf(
				values.ErrNotANumber, "%s: requires real arguments", name,
			)
			return true
		}
		return fails(prev, curr)
	})
	if complexErr != nil {
		return complexErr
	}
	return err
}

// NumericExtremum is a helper for min/max primitives.
// First arg at index 0, rest at index 1. Returns the extremum value
// where isBetter returns true if candidate should replace current.
// Per R7RS, if any argument is inexact, the result is inexact.
func NumericExtremum(
	mc *machine.MachineContext,
	name string,
	isBetter func(candidate, current values.Number) bool,
) error {
	first := mc.Arg(0)
	best, ok := first.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, first)
	}

	// Track if any argument is inexact
	hasInexact := !best.IsExact()

	// Check if first is NaN
	if best.IsNaN() {
		mc.SetValue(best)
		return nil
	}

	rest := mc.Arg(1)
	if values.IsEmptyList(rest) {
		mc.SetValue(MaybeToInexact(best, hasInexact))
		return nil
	}
	pr, ok := rest.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a list but got %T", name, rest)
	}
	// Fast path: single-element rest list (the common 2-argument case).
	if val, ok := values.Single(pr); ok {
		curr, ok := val.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, val)
		}
		if !curr.IsExact() {
			hasInexact = true
		}
		if curr.IsNaN() {
			mc.SetValue(curr)
			return nil
		}
		if isBetter(curr, best) {
			best = curr
		}
		mc.SetValue(MaybeToInexact(best, hasInexact))
		return nil
	}

	foundNaN := false
	v, err := pr.ForEach(mc.Context(), func(_ context.Context, _ int, _ bool, v values.Value) error {
		curr, ok := v.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, v)
		}

		if !curr.IsExact() {
			hasInexact = true
		}

		// Check for NaN - if any argument is NaN, result is NaN
		if curr.IsNaN() {
			foundNaN = true
			best = curr
			return nil
		}

		if !foundNaN && isBetter(curr, best) {
			best = curr
		}
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: not a proper list", name)
	}

	// If NaN was found, return it directly
	if foundNaN {
		mc.SetValue(best)
		return nil
	}

	mc.SetValue(MaybeToInexact(best, hasInexact))
	return nil
}

// MaybeToInexact converts an exact number to inexact if needed.
// If the number is already inexact or hasInexact is false, returns it unchanged.
func MaybeToInexact(n values.Number, hasInexact bool) values.Value {
	if !hasInexact || !n.IsExact() {
		return n
	}
	return n.ToInexact()
}
