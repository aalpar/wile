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

	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
)

// Type predicates using the helper factory

// PrimBooleanQ implements the boolean? predicate.
var PrimBooleanQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Boolean)
	return ok
})

// PrimStringQ implements the string? predicate.
var PrimStringQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.String)
	return ok
})

// PrimSymbolQ implements the symbol? predicate.
var PrimSymbolQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Symbol)
	return ok
})

// PrimVectorQ implements the vector? predicate.
var PrimVectorQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Vector)
	return ok
})

// PrimCharQ implements the char? predicate.
var PrimCharQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Character)
	return ok
})

// PrimNumberQ implements the number? predicate.
var PrimNumberQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(values.Number)
	return ok
})

// PrimComplexQ implements the complex? predicate.
// In Scheme, all numbers are complex (complex is the top of the numeric tower).
var PrimComplexQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(values.Number)
	return ok
})

// PrimBytevectorQ implements the bytevector? predicate.
var PrimBytevectorQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.ByteVector)
	return ok
})

// PrimProcedureQ implements the procedure? predicate.
// R7RS §6.1: Returns #t for all procedure types including case-lambda closures.
var PrimProcedureQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	switch o.(type) {
	case *machine.MachineClosure, *machine.CaseLambdaClosure:
		return true
	default:
		return false
	}
})

// Manual predicate implementations

// PrimVoidQ implements the void? predicate.
// Returns #t if the argument is the void value.
func PrimVoidQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	mc.SetValue(schemeutil.BoolToBoolean(o.IsVoid()))
	return nil
}

// PrimNullQ implements the null? predicate.
// Returns #t if the argument is the empty list '().
func PrimNullQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	mc.SetValue(schemeutil.BoolToBoolean(values.IsEmptyList(o)))
	return nil
}

// PrimPairQ implements the pair? predicate.
// Returns #t if the argument is a pair (cons cell).
// EmptyList is not a *Pair (it's a separate type), so the type assertion
// handles (pair? '()) -> #f at the type level per R7RS §6.4.
func PrimPairQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.Pair)
	mc.SetValue(schemeutil.BoolToBoolean(ok))
	return nil
}

// PrimListQ implements the list? predicate.
// Returns #t if the argument is a proper list, #f otherwise.
// R7RS: list? operates on runtime values, not syntax objects.
// (list? #'()) => #f, (list? '()) => #t
func PrimListQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	// Check for values.EmptyList specifically (not syntax empty list)
	if o == values.EmptyList {
		mc.SetValue(values.TrueValue)
		return nil
	}
	// Check for *values.Pair only (not syntax pairs)
	pr, ok := o.(*values.Pair)
	if !ok {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(schemeutil.BoolToBoolean(pr.IsList()))
	return nil
}

// PrimIntegerQ implements the integer? predicate.
//
// R7RS §6.2.6: Returns #t if the argument is an integer (exact or inexact).
// Inexact integers are floating-point numbers with zero fractional part.
func PrimIntegerQ(_ context.Context, mc *machine.MachineContext) error {
	n, ok := mc.Arg(0).(values.Number)
	if !ok {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(schemeutil.BoolToBoolean(n.IsInteger()))
	return nil
}

// PrimRealQ implements the real? predicate.
//
// R7RS §6.2.6: Returns #t if the argument is a real number.
// Rationals (including integers and BigInteger) are a subset of reals.
func PrimRealQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case values.RealNumber:
		_ = v
		mc.SetValue(values.TrueValue)
	case values.ComplexNumber:
		mc.SetValue(schemeutil.BoolToBoolean(v.IsReal()))
	default:
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimRationalQ implements the rational? predicate.
//
// R7RS §6.2.6: Returns #t if the argument is a rational number.
// Integers (including BigInteger) are a subset of rationals.
func PrimRationalQ(_ context.Context, mc *machine.MachineContext) error {
	n, ok := mc.Arg(0).(values.Number)
	if !ok {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(schemeutil.BoolToBoolean(n.IsRational()))
	return nil
}

// PrimExactQ implements the exact? predicate.
//
// R7RS §6.2.6: Returns #t if the number is exact, #f otherwise.
var PrimExactQ = helpers.MakeNumericPredicate[values.Number](
	"exact?", values.ErrNotANumber, values.Number.IsExact,
)

// PrimInexactQ implements the inexact? predicate.
//
// R7RS §6.2.6: Returns #t if the number is inexact, #f otherwise.
var PrimInexactQ = helpers.MakeNumericPredicate[values.Number](
	"inexact?", values.ErrNotANumber, func(n values.Number) bool {
		return !n.IsExact()
	},
)

// PrimExactIntegerQ implements the exact-integer? predicate.
//
// R7RS §6.2.6: Returns #t if the argument is both exact and an integer.
func PrimExactIntegerQ(_ context.Context, mc *machine.MachineContext) error {
	n, ok := mc.Arg(0).(values.Number)
	if !ok {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(schemeutil.BoolToBoolean(n.IsExact() && n.IsInteger()))
	return nil
}

// PrimZeroQ implements the zero? predicate.
// Returns #t if the number is zero, #f otherwise.
var PrimZeroQ = helpers.MakeNumericPredicate[values.Number](
	"zero?", values.ErrNotANumber, values.Number.IsZero,
)

// PrimPositiveQ implements the positive? predicate.
//
// R7RS §6.2.6: Returns #t if the real number is positive.
var PrimPositiveQ = helpers.MakeNumericPredicate[values.RealNumber](
	"positive?", values.ErrNotANumber, values.RealNumber.IsPositive,
)

// PrimNegativeQ implements the negative? predicate.
//
// R7RS §6.2.6: Returns #t if the real number is negative.
var PrimNegativeQ = helpers.MakeNumericPredicate[values.RealNumber](
	"negative?", values.ErrNotANumber, values.RealNumber.IsNegative,
)

// parityCheck is a helper for implementing parity predicates (odd? and even?).
// It accepts the predicate name, a test for regular integers, and a test for big integers.
func parityCheck(
	mc *machine.MachineContext,
	name string,
	regularTest func(int64) bool,
	bigTest func(*big.Int) bool,
) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(schemeutil.BoolToBoolean(regularTest(v.Value)))
	case *values.BigInteger:
		mc.SetValue(schemeutil.BoolToBoolean(bigTest(v.BigInt())))
	case *values.Float:
		// Must be an integer value (no fractional part)
		if math.IsInf(v.Value, 0) || math.IsNaN(v.Value) {
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %v", name, v.Value)
		}
		if math.Floor(v.Value) != v.Value {
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %v", name, v.Value)
		}
		// Convert to big.Int for reliable parity check on large floats
		bf := new(big.Float).SetFloat64(v.Value)
		bi, _ := bf.Int(nil)
		mc.SetValue(schemeutil.BoolToBoolean(bigTest(bi)))
	case *values.BigFloat:
		// Must be an integer value
		if !v.BigFloatValue().IsInt() {
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %v", name, v.BigFloatValue())
		}
		bi, _ := v.BigFloatValue().Int(nil)
		mc.SetValue(schemeutil.BoolToBoolean(bigTest(bi)))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %T", name, o)
	}
	return nil
}

// PrimOddQ implements the odd? predicate.
//
// R7RS §6.2.6: Returns #t if the integer is odd, #f otherwise.
// Accepts any integer, including inexact integers (e.g., 3.0).
func PrimOddQ(_ context.Context, mc *machine.MachineContext) error {
	return parityCheck(mc, "odd?",
		func(n int64) bool {
			return n%2 != 0
		},
		func(n *big.Int) bool {
			return n.Bit(0) == 1
		})
}

// PrimEvenQ implements the even? predicate.
//
// R7RS §6.2.6: Returns #t if the integer is even, #f otherwise.
// Accepts any integer, including inexact integers (e.g., 4.0).
func PrimEvenQ(_ context.Context, mc *machine.MachineContext) error {
	return parityCheck(mc, "even?",
		func(n int64) bool {
			return n%2 == 0
		},
		func(n *big.Int) bool {
			return n.Bit(0) == 0
		})
}
