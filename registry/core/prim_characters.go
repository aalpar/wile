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

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
)

// PrimCharToInteger implements the (char->integer) primitive.
// Returns the Unicode code point of the character as an integer.
func PrimCharToInteger(_ context.Context, mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Character](mc, 0, values.ErrNotACharacter, "char->integer")
	if err != nil {
		return err
	}
	mc.SetValue(values.NewInteger(int64(ch.Value)))
	return nil
}

// PrimIntegerToChar implements the (integer->char) primitive.
// Converts a Unicode code point (integer) to a character.
//
// R7RS §6.6: The argument must be a valid Unicode scalar value,
// i.e., an integer in [0, #xD7FF] ∪ [#xE000, #x10FFFF].
func PrimIntegerToChar(_ context.Context, mc *machine.MachineContext) error {
	n, err := helpers.RequireArg[*values.Integer](mc, 0, values.ErrNotANumber, "integer->char")
	if err != nil {
		return err
	}
	v := n.Value
	if v < 0 || v > 0x10FFFF || (v >= 0xD800 && v <= 0xDFFF) {
		return values.WrapForeignErrorf(values.ErrInvalidArgument, "integer->char: value %d is not a valid Unicode scalar value", v)
	}
	mc.SetValue(values.NewCharacter(rune(v)))
	return nil
}

// charCompareSpecs defines the five R7RS §6.6 character comparison predicates.
// Each entry pairs a primitive name with its comparison function.
var charCompareSpecs = []struct {
	name string
	cmp  func(rune, rune) bool
}{
	{"char=?", func(a, b rune) bool { return a == b }},
	{"char<?", func(a, b rune) bool { return a < b }},
	{"char>?", func(a, b rune) bool { return a > b }},
	{"char<=?", func(a, b rune) bool { return a <= b }},
	{"char>=?", func(a, b rune) bool { return a >= b }},
}

// makeCharComparePrimitive returns a ForeignFunction that performs a variadic
// character comparison using the given comparator.
func makeCharComparePrimitive(name string, cmp func(rune, rune) bool) machine.ForeignFunction {
	return func(_ context.Context, mc *machine.MachineContext) error {
		return helpers.CharCompareVariadic(mc, name, cmp)
	}
}
