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

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
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
// R7RS §6.1: Returns #t for all callable types: lambdas, case-lambdas, Go
// primitives (foreign closures), parameter objects (R7RS §4.2.6), and both
// captured and composable continuations.
var PrimProcedureQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(values.Callable)
	return ok
})

// PrimVoidQ implements the void? predicate.
// Returns #t if the argument is the void value.
var PrimVoidQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	return o.IsVoid()
})

// PrimNullQ implements the null? predicate.
// Returns #t if the argument is the empty list '().
var PrimNullQ = helpers.MakeTypePredicate(values.IsEmptyList)

// PrimPairQ implements the pair? predicate.
// Returns #t if the argument is a pair (cons cell).
// EmptyList is not a *Pair (it's a separate type), so the type assertion
// handles (pair? '()) -> #f at the type level per R7RS §6.4.
var PrimPairQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Pair)
	return ok
})

// PrimIntegerQ implements the integer? predicate.
//
// R7RS §6.2.6: Returns #t if the argument is an integer (exact or inexact).
// Inexact integers are floating-point numbers with zero fractional part.
//
// MakeNumericPredicate is deliberately NOT the factory here: it raises on a
// non-number, and R7RS requires #f.
var PrimIntegerQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	n, ok := o.(values.Number)
	return ok && n.IsInteger()
})

// PrimRealQ implements the real? predicate.
//
// R7RS §6.2.6: Returns #t if the argument is a real number.
// Rationals (including integers and BigInteger) are a subset of reals.
var PrimRealQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	switch v := o.(type) {
	case values.RealNumber:
		return true
	case values.ComplexNumber:
		return v.IsReal()
	default:
		return false
	}
})

// PrimRationalQ implements the rational? predicate.
//
// R7RS §6.2.6: Returns #t if the argument is a rational number.
// Integers (including BigInteger) are a subset of rationals.
var PrimRationalQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	n, ok := o.(values.Number)
	return ok && n.IsRational()
})

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

// parityCheck is a helper for implementing parity predicates (odd? and even?).
// It accepts the predicate name, a test for regular integers, and a test for big integers.
func parityCheck(
	mc machine.CallContext,
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
			return werr.WrapForeignErrorf(werr.ErrNotAnInteger, "%s: expected an integer but got %v", name, v.Value)
		}
		if math.Floor(v.Value) != v.Value {
			return werr.WrapForeignErrorf(werr.ErrNotAnInteger, "%s: expected an integer but got %v", name, v.Value)
		}
		// Convert to big.Int for reliable parity check on large floats
		bf := new(big.Float).SetFloat64(v.Value)
		bi, _ := bf.Int(nil)
		mc.SetValue(values.BoolToBoolean(bigTest(bi)))
	case *values.BigFloat:
		// Must be an integer value
		if !v.BigFloatValue().IsInt() {
			return werr.WrapForeignErrorf(werr.ErrNotAnInteger, "%s: expected an integer but got %v", name, v.BigFloatValue())
		}
		bi, _ := v.BigFloatValue().Int(nil)
		mc.SetValue(values.BoolToBoolean(bigTest(bi)))
	default:
		return werr.WrapForeignErrorf(werr.ErrNotAnInteger, "%s: expected an integer but got %T", name, o)
	}
	return nil
}

// PrimOddQ implements the odd? predicate.
//
// R7RS §6.2.6: Returns #t if the integer is odd, #f otherwise.
// Accepts any integer, including inexact integers (e.g., 3.0).
func PrimOddQ(mc machine.CallContext) error {
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
func PrimEvenQ(mc machine.CallContext) error {
	return parityCheck(mc, "even?",
		func(n int64) bool {
			return n%2 == 0
		},
		func(n *big.Int) bool {
			return n.Bit(0) == 0
		})
}
