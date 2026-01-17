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

	"wile/machine"
	"wile/values"
)

// numericExtremum is a helper for min/max primitives.
// First arg at index 0, rest at index 1. Returns the extremum value
// where isBetter returns true if candidate should replace current.
// Per R7RS, if any argument is inexact, the result is inexact.
func numericExtremum(
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
	hasInexact := isInexact(best)

	// Check if first is NaN
	if f, ok := best.(*values.Float); ok && math.IsNaN(f.Value) {
		mc.SetValue(best)
		return nil
	}

	rest := mc.Arg(1)
	pr, ok := rest.(*values.Pair)
	if !ok {
		if values.IsEmptyList(rest) {
			mc.SetValue(maybeToInexact(best, hasInexact))
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

		if isInexact(curr) {
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

	mc.SetValue(maybeToInexact(best, hasInexact))
	return nil
}

// isInexact returns true if the number is inexact (Float or Complex)
func isInexact(n values.Number) bool {
	switch n.(type) {
	case *values.Float, *values.Complex:
		return true
	default:
		return false
	}
}

// maybeToInexact converts an exact number to inexact (Float) if needed.
// If the number is already inexact or hasInexact is false, returns it unchanged.
func maybeToInexact(n values.Number, hasInexact bool) values.Value {
	if !hasInexact {
		return n
	}
	// If already inexact, return as-is
	if isInexact(n) {
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
