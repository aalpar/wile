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

	"wile/machine"
	"wile/values"
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
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "%s: expected a pair but got %T", name, o)
	}
	if values.IsEmptyList(pr) {
		mc.SetValue(identity)
		return nil
	}
	o = pr.Car()
	nbr, ok := o.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, o)
	}
	pr, ok = pr.Cdr().(*values.Pair)
	if !ok {
		mc.SetValue(nbr)
		return nil
	}
	v, err := pr.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, o values.Value) error {
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
	pr, ok := o1.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "%s: expected a pair but got %T", name, o1)
	}
	if values.IsEmptyList(pr) {
		mc.SetValue(unaryOp(nbr0))
		return nil
	}
	o2 := pr.Car()
	nbr2, ok := o2.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, o2)
	}
	acc := binOp(nbr0, nbr2)
	pr, ok = pr.Cdr().(*values.Pair)
	if !ok {
		mc.SetValue(acc)
		return nil
	}
	v, err := pr.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, o values.Value) error {
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

// IsNaN returns true if the number is a NaN float.
func IsNaN(n values.Number) bool {
	f, ok := n.(*values.Float)
	return ok && math.IsNaN(f.Value)
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
	pr, ok := rest.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "%s: expected a pair but got %T", name, rest)
	}
	v, err := pr.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, v values.Value) error {
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
	hasInexact := IsInexact(best)

	// Check if first is NaN
	if f, ok := best.(*values.Float); ok && math.IsNaN(f.Value) {
		mc.SetValue(best)
		return nil
	}

	rest := mc.Arg(1)
	pr, ok := rest.(*values.Pair)
	if !ok {
		if values.IsEmptyList(rest) {
			mc.SetValue(MaybeToInexact(best, hasInexact))
			return nil
		}
		return values.WrapForeignErrorf(values.ErrNotAPair, "%s: expected a pair but got %T", name, rest)
	}

	foundNaN := false
	v, err := pr.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, v values.Value) error {
		curr, ok := v.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, v)
		}

		if IsInexact(curr) {
			hasInexact = true
		}

		// Check for NaN - if any argument is NaN, result is NaN
		if f, ok := curr.(*values.Float); ok && math.IsNaN(f.Value) {
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

// IsInexact returns true if the number is inexact (Float, BigFloat, or Complex)
func IsInexact(n values.Number) bool {
	switch n.(type) {
	case *values.Float, *values.BigFloat, *values.Complex:
		return true
	default:
		return false
	}
}

// MaybeToInexact converts an exact number to inexact (Float) if needed.
// If the number is already inexact or hasInexact is false, returns it unchanged.
func MaybeToInexact(n values.Number, hasInexact bool) values.Value {
	if !hasInexact {
		return n
	}
	// If already inexact, return as-is
	if IsInexact(n) {
		return n
	}
	// Convert exact to inexact
	switch v := n.(type) {
	case *values.Integer:
		return values.NewFloat(float64(v.Value))
	case *values.BigInteger:
		f, _ := v.ToInexact().(*values.Float)
		return f
	case *values.Rational:
		return values.NewFloat(v.Float64())
	default:
		return n
	}
}
