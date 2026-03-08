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
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimEqQ implements the eq? predicate (R7RS §6.1).
// Returns #t if both arguments are identical: pointer equality for most types,
// string key comparison for symbols (R7RS §6.5).
func PrimEqQ(mc *machine.MachineContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)
	mc.SetValue(values.BoolToBoolean(helpers.EqIdentity(o0, o1)))
	return nil
}

// PrimEqvQ implements the eqv? predicate (R7RS).
// Returns #t if both arguments are operationally equivalent:
// - Same object (pointer equality), OR
// - Both are numbers of the same type with the same value, OR
// - Both are characters with the same value
// Unlike eq?, eqv? treats equivalent numbers/characters as equal even if
// they are different objects. Unlike equal?, eqv? does not recurse into
// pairs, vectors, or strings.
func PrimEqvQ(mc *machine.MachineContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)
	mc.SetValue(values.BoolToBoolean(helpers.Eqv(o0, o1)))
	return nil
}

// PrimEqualQ implements the equal? predicate for structural equality.
// Returns #t if both arguments have the same structure and values.
func PrimEqualQ(mc *machine.MachineContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)
	mc.SetValue(values.BoolToBoolean(values.EqualTo(o0, o1)))
	return nil
}

// PrimNot implements the not primitive.
// Returns #t if the argument is #f, #f otherwise.
func PrimNot(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	// In Scheme, only #f is false; everything else is true
	mc.SetValue(values.BoolToBoolean(!values.ValueToBool(o)))
	return nil
}

// PrimBooleanEq implements the boolean=? primitive.
// R7RS §6.3: (boolean=? boolean1 boolean2 boolean3 ...)
// Returns #t if all arguments are booleans and all are the same value.
func PrimBooleanEq(mc *machine.MachineContext) error {
	return helpers.ChainEquality(mc, "boolean=?",
		func(v values.Value) error {
			if v != values.TrueValue && v != values.FalseValue {
				return werr.WrapForeignErrorf(werr.ErrNotABoolean, "boolean=?: expected a boolean but got %T", v)
			}
			return nil
		},
		func(a, b values.Value) bool {
			return a == b
		},
	)
}

// PrimSymbolEq implements the symbol=? primitive.
// R7RS §6.5: (symbol=? symbol1 symbol2 symbol3 ...)
// Returns #t if all arguments are symbols and all are the same symbol.
func PrimSymbolEq(mc *machine.MachineContext) error {
	return helpers.ChainEquality(mc, "symbol=?",
		func(v values.Value) error {
			_, err := helpers.RequireType[*values.Symbol](v, werr.ErrNotASymbol, "symbol=?")
			return err
		},
		func(a, b values.Value) bool {
			return a.(*values.Symbol).Key == b.(*values.Symbol).Key
		},
	)
}
