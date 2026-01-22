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

	"wile/machine"
	"wile/registry/helpers"
	"wile/values"
)

// PrimCharToInteger implements the (char->integer) primitive.
// Returns the Unicode code point of the character as an integer.
func PrimCharToInteger(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char->integer: expected a character but got %T", o)
	}
	mc.SetValue(values.NewInteger(int64(ch.Value)))
	return nil
}

// PrimIntegerToChar implements the (integer->char) primitive.
// Converts a Unicode code point (integer) to a character.
func PrimIntegerToChar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	n, ok := o.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "integer->char: expected an integer but got %T", o)
	}
	mc.SetValue(values.NewCharacter(rune(n.Value)))
	return nil
}

// PrimCharEqVariadic implements the variadic char=? primitive.
func PrimCharEqVariadic(_ context.Context, mc *machine.MachineContext) error {
	return helpers.CharCompareVariadic(mc, "char=?", func(a, b rune) bool { return a == b })
}

// PrimCharLtVariadic implements the variadic char<? primitive.
func PrimCharLtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return helpers.CharCompareVariadic(mc, "char<?", func(a, b rune) bool { return a < b })
}

// PrimCharGtVariadic implements the variadic char>? primitive.
func PrimCharGtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return helpers.CharCompareVariadic(mc, "char>?", func(a, b rune) bool { return a > b })
}

// PrimCharLeVariadic implements the variadic char<=? primitive.
func PrimCharLeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return helpers.CharCompareVariadic(mc, "char<=?", func(a, b rune) bool { return a <= b })
}

// PrimCharGeVariadic implements the variadic char>=? primitive.
func PrimCharGeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return helpers.CharCompareVariadic(mc, "char>=?", func(a, b rune) bool { return a >= b })
}
