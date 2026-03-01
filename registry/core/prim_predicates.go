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
	"math"
	"math/big"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
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
// R7RS §6.1: Returns #t for all callable types — lambdas, case-lambdas,
// parameter objects (R7RS §4.2.6), and composable continuations.
var PrimProcedureQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(values.Callable)
	return ok
})

// Manual predicate implementations

// PrimVoidQ implements the void? predicate.
// Returns #t if the argument is the void value.
func PrimVoidQ(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	mc.SetValue(values.BoolToBoolean(o.IsVoid()))
	return nil
}

// PrimNullQ implements the null? predicate.
// Returns #t if the argument is the empty list '().
func PrimNullQ(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	mc.SetValue(values.BoolToBoolean(values.IsEmptyList(o)))
	return nil
}

// PrimPairQ implements the pair? predicate.
// Returns #t if the argument is a pair (cons cell).
// EmptyList is not a *Pair (it's a separate type), so the type assertion
// handles (pair? '()) -> #f at the type level per R7RS §6.4.
func PrimPairQ(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.Pair)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}

// PrimListQ implements the list? predicate.
// Returns #t if the argument is a proper list, #f otherwise.
// R7RS: list? operates on runtime values, not syntax objects.
// (list? #'()) => #f, (list? '()) => #t
func PrimListQ(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	// Runtime list types only — not syntax pairs.
	switch t := o.(type) {
	case *values.Pair:
		mc.SetValue(values.BoolToBoolean(t.IsList()))
	default:
		mc.SetValue(values.BoolToBoolean(values.IsEmptyList(o)))
	}
	return nil
}

// PrimIntegerQ implements the integer? predicate.
//
// R7RS §6.2.6: Returns #t if the argument is an integer (exact or inexact).
// Inexact integers are floating-point numbers with zero fractional part.
func PrimIntegerQ(mc *machine.MachineContext) error {
	n, ok := mc.Arg(0).(values.Number)
	if !ok {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(values.BoolToBoolean(n.IsInteger()))
	return nil
}

// PrimRealQ implements the real? predicate.
//
// R7RS §6.2.6: Returns #t if the argument is a real number.
// Rationals (including integers and BigInteger) are a subset of reals.
func PrimRealQ(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case values.RealNumber:
		_ = v
		mc.SetValue(values.TrueValue)
	case values.ComplexNumber:
		mc.SetValue(values.BoolToBoolean(v.IsReal()))
	default:
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimRationalQ implements the rational? predicate.
//
// R7RS §6.2.6: Returns #t if the argument is a rational number.
// Integers (including BigInteger) are a subset of rationals.
func PrimRationalQ(mc *machine.MachineContext) error {
	n, ok := mc.Arg(0).(values.Number)
	if !ok {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(values.BoolToBoolean(n.IsRational()))
	return nil
}

// PrimExactQ implements the exact? predicate.
//
// R7RS §6.2.6: Returns #t if the number is exact, #f otherwise.
var PrimExactQ = helpers.MakeNumericPredicate[values.Number](
	"exact?", werr.ErrNotANumber, values.Number.IsExact,
)

// PrimInexactQ implements the inexact? predicate.
//
// R7RS §6.2.6: Returns #t if the number is inexact, #f otherwise.
var PrimInexactQ = helpers.MakeNumericPredicate[values.Number](
	"inexact?", werr.ErrNotANumber, func(n values.Number) bool {
		return !n.IsExact()
	},
)

// PrimExactIntegerQ implements the exact-integer? predicate.
//
// R7RS §6.2.6: Returns #t if the argument is both exact and an integer.
func PrimExactIntegerQ(mc *machine.MachineContext) error {
	n, ok := mc.Arg(0).(values.Number)
	if !ok {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(values.BoolToBoolean(n.IsExact() && n.IsInteger()))
	return nil
}

// PrimZeroQ implements the zero? predicate.
// Returns #t if the number is zero, #f otherwise.
var PrimZeroQ = helpers.MakeNumericPredicate[values.Number](
	"zero?", werr.ErrNotANumber, values.Number.IsZero,
)

// PrimPositiveQ implements the positive? predicate.
//
// R7RS §6.2.6: Returns #t if the real number is positive.
// Real-valued complex numbers (zero imaginary part) are accepted.
func PrimPositiveQ(mc *machine.MachineContext) error {
	return realSignPredicate(mc, "positive?", values.RealNumber.IsPositive)
}

// PrimNegativeQ implements the negative? predicate.
//
// R7RS §6.2.6: Returns #t if the real number is negative.
// Real-valued complex numbers (zero imaginary part) are accepted.
func PrimNegativeQ(mc *machine.MachineContext) error {
	return realSignPredicate(mc, "negative?", values.RealNumber.IsNegative)
}

// realSignPredicate implements positive? and negative? with support for
// real-valued complex numbers per R7RS §6.2.6.
func realSignPredicate(mc *machine.MachineContext, name string, test func(values.RealNumber) bool) error {
	o := mc.Arg(0)
	c, ok := o.(values.ComplexNumber)
	if ok && c.IsReal() {
		o = c.RealPart()
	}
	r, ok := o.(values.RealNumber)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "%s: expected a real number but got %T", name, mc.Arg(0))
	}
	mc.SetValue(values.BoolToBoolean(test(r)))
	return nil
}

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
		mc.SetValue(values.BoolToBoolean(regularTest(v.Value)))
	case *values.BigInteger:
		mc.SetValue(values.BoolToBoolean(bigTest(v.BigInt())))
	case *values.Float:
		// Must be an integer value (no fractional part)
		if math.IsInf(v.Value, 0) || math.IsNaN(v.Value) {
			return werr.WrapForeignErrorf(werr.ErrNotANumber, "%s: expected an integer but got %v", name, v.Value)
		}
		if math.Floor(v.Value) != v.Value {
			return werr.WrapForeignErrorf(werr.ErrNotANumber, "%s: expected an integer but got %v", name, v.Value)
		}
		// Convert to big.Int for reliable parity check on large floats
		bf := new(big.Float).SetFloat64(v.Value)
		bi, _ := bf.Int(nil)
		mc.SetValue(values.BoolToBoolean(bigTest(bi)))
	case *values.BigFloat:
		// Must be an integer value
		if !v.BigFloatValue().IsInt() {
			return werr.WrapForeignErrorf(werr.ErrNotANumber, "%s: expected an integer but got %v", name, v.BigFloatValue())
		}
		bi, _ := v.BigFloatValue().Int(nil)
		mc.SetValue(values.BoolToBoolean(bigTest(bi)))
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "%s: expected an integer but got %T", name, o)
	}
	return nil
}

// PrimOddQ implements the odd? predicate.
//
// R7RS §6.2.6: Returns #t if the integer is odd, #f otherwise.
// Accepts any integer, including inexact integers (e.g., 3.0).
func PrimOddQ(mc *machine.MachineContext) error {
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
func PrimEvenQ(mc *machine.MachineContext) error {
	return parityCheck(mc, "even?",
		func(n int64) bool {
			return n%2 == 0
		},
		func(n *big.Int) bool {
			return n.Bit(0) == 0
		})
}
