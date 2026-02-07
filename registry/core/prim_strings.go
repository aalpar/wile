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
	"strings"
	"unicode/utf8"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
)

// PrimString implements the string primitive.
// (string char ...) returns a newly allocated string composed of the given characters.
func PrimString(_ context.Context, mc *machine.MachineContext) error {
	args := mc.Arg(0)

	var sb strings.Builder
	for args != values.EmptyList {
		tuple, ok := args.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "string: expected a list of characters")
		}
		ch, ok := tuple.Car().(*values.Character)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotACharacter, "string: expected a character but got %T", tuple.Car())
		}
		sb.WriteRune(ch.Value)
		args = tuple.Cdr()
	}

	// R7RS §6.7: string returns a "newly allocated string"
	mc.SetValue(values.NewMutableString(sb.String()))
	return nil
}

// PrimMakeString implements the make-string primitive.
// (make-string k) creates a string of k unspecified characters.
// (make-string k char) creates a string of k copies of char.
func PrimMakeString(_ context.Context, mc *machine.MachineContext) error {
	k := mc.Arg(0)
	kInt, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInteger, "make-string: expected an integer but got %T", k)
	}
	if kInt.Value < 0 {
		return values.NewForeignError("make-string: length must be non-negative")
	}

	fillChar := rune(0) // default fill character (NUL)
	rest := mc.Arg(1)
	if rest != values.EmptyList {
		tuple, ok := rest.(values.Tuple)
		if ok {
			ch, ok := tuple.Car().(*values.Character)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotACharacter, "make-string: expected a character but got %T", tuple.Car())
			}
			fillChar = ch.Value
		}
	}

	// Use NewMutableString since make-string returns a mutable string per R7RS §6.7
	q := values.NewMutableString(strings.Repeat(string(fillChar), int(kInt.Value)))
	mc.SetValue(q)
	return nil
}

// PrimStringLength implements string-length.
// Returns the number of characters (runes) in the string.
func PrimStringLength(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	s, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-length: expected a string but got %T", o)
	}
	mc.SetValue(values.NewInteger(int64(utf8.RuneCountInString(s.Value))))
	return nil
}

// PrimStringRef implements the string-ref primitive.
// Returns the character at the given index in the string.
func PrimStringRef(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	k := mc.Arg(1)
	s, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-ref: expected a string but got %T", o)
	}
	idx, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "string-ref: expected an integer but got %T", k)
	}
	runes := []rune(s.Value)
	if idx.Value < 0 || idx.Value >= int64(len(runes)) {
		return values.NewForeignError("string-ref: index out of bounds")
	}
	mc.SetValue(values.NewCharacter(runes[idx.Value]))
	return nil
}

// PrimStringSet implements the string-set! primitive.
// Stores char in element k of string.
// R7RS §6.7: (string-set! string k char)
func PrimStringSet(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	k := mc.Arg(1)
	c := mc.Arg(2)
	s, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-set!: expected a string but got %T", o)
	}
	idx, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "string-set!: expected an integer but got %T", k)
	}
	char, ok := c.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "string-set!: expected a character but got %T", c)
	}
	length := s.Len()
	if idx.Value < 0 || idx.Value >= int64(length) {
		return values.NewForeignError("string-set!: index out of bounds")
	}
	s.SetChar(int(idx.Value), char.Value)
	mc.SetValue(values.Void)
	return nil
}

// PrimStringToList implements the string->list primitive.
// R7RS §6.7: (string->list string [start [end]])
// Converts a string (or substring) to a list of characters.
func PrimStringToList(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	rest := mc.Arg(1)

	s, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string->list: expected a string but got %T", o)
	}

	runes := s.Runes()
	length := len(runes)

	start64, end64, err := helpers.ParseOptionalStartEnd(rest, int64(length), "string->list")
	if err != nil {
		return err
	}
	start, end := int(start64), int(end64)

	// Validate indices
	if start < 0 || end > length || start > end {
		return values.NewForeignError("string->list: invalid indices")
	}

	// Build the list from the substring
	var result values.Value = values.EmptyList
	for i := end - 1; i >= start; i-- {
		result = values.NewCons(values.NewCharacter(runes[i]), result)
	}
	mc.SetValue(result)
	return nil
}

// PrimListToString implements the (list->string) primitive.
// Converts a list of characters to a string.
func PrimListToString(ctx context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	if values.IsEmptyList(o) {
		mc.SetValue(values.NewString(""))
		return nil
	}
	tuple, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "list->string: expected a list but got %T", o)
	}
	var runes []rune
	v, err := tuple.ForEach(ctx, func(_ context.Context, _ int, _ bool, v values.Value) error {
		ch, ok := v.(*values.Character)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotACharacter, "list->string: expected a character but got %T", v)
		}
		runes = append(runes, ch.Value)
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "list->string: expected a proper list")
	}
	mc.SetValue(values.NewString(string(runes)))
	return nil
}

// PrimSymbolToString implements the symbol->string primitive.
// Converts a symbol to a string.
func PrimSymbolToString(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	sym, ok := o.(*values.Symbol)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASymbol, "symbol->string: expected a symbol but got %T", o)
	}
	mc.SetValue(values.NewString(sym.Key))
	return nil
}

// PrimStringToSymbol implements the string->symbol primitive.
// Converts a string to an interned symbol.
func PrimStringToSymbol(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	s, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string->symbol: expected a string but got %T", o)
	}
	sym := values.NewSymbol(s.Value)
	// Intern the symbol
	sym = mc.EnvironmentFrame().InternSymbol(sym)
	mc.SetValue(sym)
	return nil
}

// PrimStringAppend implements the (string-append) primitive.
// Concatenates strings.
func PrimStringAppend(ctx context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	tuple, ok := o.(values.Tuple)
	if !ok {
		if values.IsEmptyList(o) {
			mc.SetValue(values.NewString(""))
			return nil
		}
		return values.WrapForeignErrorf(values.ErrNotAList, "string-append: expected a list but got %T", o)
	}
	var sb strings.Builder
	v, err := tuple.ForEach(ctx, func(_ context.Context, _ int, _ bool, v values.Value) error {
		s, ok := v.(*values.String)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAString, "string-append: expected a string but got %T", v)
		}
		sb.WriteString(s.Value)
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "string-append: not a proper list")
	}
	mc.SetValue(values.NewString(sb.String()))
	return nil
}

// PrimSubstring implements the substring primitive.
// Returns a substring between the given start and end indices.
func PrimSubstring(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	start := mc.Arg(1)
	end := mc.Arg(2)
	s, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "substring: expected a string but got %T", o)
	}
	startIdx, ok := start.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "substring: expected an integer but got %T", start)
	}
	endIdx, ok := end.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "substring: expected an integer but got %T", end)
	}
	runes := []rune(s.Value)
	if startIdx.Value < 0 || endIdx.Value > int64(len(runes)) || startIdx.Value > endIdx.Value {
		return values.NewForeignError("substring: invalid indices")
	}
	mc.SetValue(values.NewString(string(runes[startIdx.Value:endIdx.Value])))
	return nil
}

// PrimStringCopy implements the string-copy primitive.
// R7RS §6.7: (string-copy string [start [end]])
// Returns a newly allocated copy of the given string (or substring).
// The returned string is mutable and distinct from the original.
func PrimStringCopy(_ context.Context, mc *machine.MachineContext) error {
	s := mc.Arg(0)
	rest := mc.Arg(1)

	str, ok := s.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-copy: expected a string but got %T", s)
	}

	runes := str.Runes()
	length := len(runes)

	start64, end64, err := helpers.ParseOptionalStartEnd(rest, int64(length), "string-copy")
	if err != nil {
		return err
	}
	start, end := int(start64), int(end64)

	// Validate indices
	if start < 0 || end > length || start > end {
		return values.NewForeignError("string-copy: invalid indices")
	}

	// Use NewMutableString to ensure the copy is not interned and can be mutated
	mc.SetValue(values.NewMutableString(string(runes[start:end])))
	return nil
}

// PrimStringEqVariadic implements the variadic string=? primitive.
func PrimStringEqVariadic(_ context.Context, mc *machine.MachineContext) error {
	return helpers.StringCompareVariadic(mc, "string=?", func(a, b string) bool { return a == b })
}

// PrimStringLtVariadic implements the variadic string<? primitive.
func PrimStringLtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return helpers.StringCompareVariadic(mc, "string<?", func(a, b string) bool { return a < b })
}

// PrimStringGtVariadic implements the variadic string>? primitive.
func PrimStringGtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return helpers.StringCompareVariadic(mc, "string>?", func(a, b string) bool { return a > b })
}

// PrimStringLeVariadic implements the variadic string<=? primitive.
func PrimStringLeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return helpers.StringCompareVariadic(mc, "string<=?", func(a, b string) bool { return a <= b })
}

// PrimStringGeVariadic implements the variadic string>=? primitive.
func PrimStringGeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return helpers.StringCompareVariadic(mc, "string>=?", func(a, b string) bool { return a >= b })
}
