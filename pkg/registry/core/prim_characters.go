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
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// PrimCharToInteger implements the (char->integer) primitive.
// Returns the Unicode code point of the character as an integer.
var PrimCharToInteger = helpers.MakeUnaryAccessor(werr.ErrNotACharacter, "char->integer", func(ch *values.Character) values.Value {
	return values.NewInteger(int64(ch.Value))
})

// PrimIntegerToChar implements the (integer->char) primitive.
// Converts a Unicode code point (integer) to a character.
//
// R7RS §6.6: The argument must be a valid Unicode scalar value,
// i.e., an integer in [0, #xD7FF] ∪ [#xE000, #x10FFFF].
func PrimIntegerToChar(mc machine.CallContext) error {
	n, err := helpers.RequireArg[*values.Integer](mc, 0, werr.ErrNotAnInteger, "integer->char")
	if err != nil {
		return err
	}
	v := n.Value
	if v < 0 || v > 0x10FFFF || (v >= 0xD800 && v <= 0xDFFF) {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "integer->char: value %d is not a valid Unicode scalar value", v)
	}
	mc.SetValue(values.NewCharacter(rune(v)))
	return nil
}

// charCompareSpecs defines the five R7RS §6.6 character comparison predicates.
// Each entry has a primitive name, comparison function, and doc string.
var charCompareSpecs = []struct {
	name string
	cmp  func(rune, rune) bool
	doc  string
}{
	{"char=?", func(a, b rune) bool { return a == b }, "Returns #t if all character arguments have the same Unicode code point.\n\nExamples:\n  (char=? #\\a #\\a)  => #t\n  (char=? #\\a #\\b)  => #f"},
	{"char<?", func(a, b rune) bool { return a < b }, "Returns #t if character arguments are monotonically increasing by Unicode code point.\n\nExamples:\n  (char<? #\\a #\\b)  => #t\n  (char<? #\\b #\\a)  => #f"},
	{"char>?", func(a, b rune) bool { return a > b }, "Returns #t if character arguments are monotonically decreasing by Unicode code point.\n\nExamples:\n  (char>? #\\b #\\a)  => #t\n  (char>? #\\a #\\b)  => #f"},
	{"char<=?", func(a, b rune) bool { return a <= b }, "Returns #t if character arguments are monotonically non-decreasing by Unicode code point.\n\nExamples:\n  (char<=? #\\a #\\a)  => #t\n  (char<=? #\\a #\\b)  => #t"},
	{"char>=?", func(a, b rune) bool { return a >= b }, "Returns #t if character arguments are monotonically non-increasing by Unicode code point.\n\nExamples:\n  (char>=? #\\b #\\a)  => #t\n  (char>=? #\\a #\\a)  => #t"},
}

// makeCharComparePrimitive returns a ForeignFunction that performs a variadic
// character comparison using the given comparator.
func makeCharComparePrimitive(name string, cmp func(rune, rune) bool) machine.ForeignFunction {
	return func(mc machine.CallContext) error {
		return helpers.CharCompareVariadic(mc, name, cmp)
	}
}
