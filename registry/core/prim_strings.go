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
	"github.com/aalpar/wile/werr"
)

// PrimString implements the string primitive.
// (string char ...) returns a newly allocated string composed of the given characters.
func PrimString(mc machine.CallContext) error {
	args := mc.Arg(0)

	var sb strings.Builder
	for !values.IsEmptyList(args) {
		tuple, ok := args.(values.Tuple)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAList, "string: expected a list of characters")
		}
		ch, ok := tuple.Car().(*values.Character)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotACharacter, "string: expected a character but got %T", tuple.Car())
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
func PrimMakeString(mc machine.CallContext) error {
	kInt, err := helpers.RequireArg[*values.Integer](mc, 0, werr.ErrNotAnInteger, "make-string")
	if err != nil {
		return err
	}
	if kInt.Value < 0 {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "make-string: length must be non-negative")
	}
	ch, err := helpers.OptionalArg[*values.Character](mc.Arg(1), values.NewCharacter(0), werr.ErrNotACharacter, "make-string")
	if err != nil {
		return err
	}

	// Use NewMutableString since make-string returns a mutable string per R7RS §6.7
	q := values.NewMutableString(strings.Repeat(string(ch.Value), int(kInt.Value)))
	mc.SetValue(q)
	return nil
}

// PrimStringLength implements string-length.
// Returns the number of characters (runes) in the string.
func PrimStringLength(mc machine.CallContext) error {
	s, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "string-length")
	if err != nil {
		return err
	}
	mc.SetValue(values.NewInteger(int64(utf8.RuneCountInString(s.Value))))
	return nil
}

// PrimStringRef implements the string-ref primitive.
// Returns the character at the given index in the string.
func PrimStringRef(mc machine.CallContext) error {
	s, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "string-ref")
	if err != nil {
		return err
	}
	runes := []rune(s.Value)
	idx, err := helpers.RequireIndex(mc, 1, len(runes), "string-ref")
	if err != nil {
		return err
	}
	mc.SetValue(values.NewCharacter(runes[idx]))
	return nil
}

// PrimStringSet implements the string-set! primitive.
// Stores char in element k of string.
// R7RS §6.7: (string-set! string k char)
func PrimStringSet(mc machine.CallContext) error {
	s, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "string-set!")
	if err != nil {
		return err
	}
	char, err := helpers.RequireArg[*values.Character](mc, 2, werr.ErrNotACharacter, "string-set!")
	if err != nil {
		return err
	}
	idx, err := helpers.RequireIndex(mc, 1, s.Len(), "string-set!")
	if err != nil {
		return err
	}

	// Check immutability and handle error
	err = s.SetChar(idx, char.Value)
	if err != nil {
		return err
	}

	mc.SetValue(values.Void)
	return nil
}

// PrimStringToList implements the string->list primitive.
// R7RS §6.7: (string->list string [start [end]])
// Converts a string (or substring) to a list of characters.
func PrimStringToList(mc machine.CallContext) error {
	s, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "string->list")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)

	runes := s.Runes()

	start, end, err := helpers.ParseSubrange(rest, len(runes), "string->list")
	if err != nil {
		return err
	}

	elems := make([]values.Value, end-start)
	for i := start; i < end; i++ {
		elems[i-start] = values.NewCharacter(runes[i])
	}
	mc.SetValue(values.List(elems...))
	return nil
}

// PrimListToString implements the (list->string) primitive.
// Converts a list of characters to a string.
func PrimListToString(mc machine.CallContext) error {
	o := mc.Arg(0)
	if values.IsEmptyList(o) {
		// R7RS §6.7: list->string returns a newly allocated mutable string
		mc.SetValue(values.NewMutableString(""))
		return nil
	}
	tuple, ok := o.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "list->string: expected a list but got %T", o)
	}
	var runes []rune
	err := helpers.ForEachList(mc.Context(), tuple, "list->string", func(_ context.Context, _ int, _ bool, v values.Value) error {
		ch, ok := v.(*values.Character)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotACharacter, "list->string: expected a character but got %T", v)
		}
		runes = append(runes, ch.Value)
		return nil
	})
	if err != nil {
		return err
	}
	// R7RS §6.7: list->string returns a newly allocated mutable string
	mc.SetValue(values.NewMutableString(string(runes)))
	return nil
}

// PrimSymbolToString implements the symbol->string primitive.
// Converts a symbol to an immutable string.
// R7RS §6.5: The string returned by symbol->string is immutable.
func PrimSymbolToString(mc machine.CallContext) error {
	sym, err := helpers.RequireArg[*values.Symbol](mc, 0, werr.ErrNotASymbol, "symbol->string")
	if err != nil {
		return err
	}
	mc.SetValue(values.NewString(sym.Key))
	return nil
}

// PrimStringToSymbol implements the string->symbol primitive.
// Converts a string to a symbol.
func PrimStringToSymbol(mc machine.CallContext) error {
	s, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "string->symbol")
	if err != nil {
		return err
	}
	sym := values.NewSymbol(s.Value)
	mc.SetValue(sym)
	return nil
}

// PrimStringAppend implements the (string-append) primitive.
// Concatenates strings.
func PrimStringAppend(mc machine.CallContext) error {
	o := mc.Arg(0)
	if values.IsEmptyList(o) {
		// R7RS §6.7: string-append returns a newly allocated mutable string,
		// even in the zero-argument / empty-list case.
		mc.SetValue(values.NewMutableString(""))
		return nil
	}
	tuple, ok := o.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "string-append: expected a list but got %T", o)
	}
	var sb strings.Builder
	err := helpers.ForEachList(mc.Context(), tuple, "string-append", func(_ context.Context, _ int, _ bool, v values.Value) error {
		s, ok := v.(*values.String)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAString, "string-append: expected a string but got %T", v)
		}
		sb.WriteString(s.Value)
		return nil
	})
	if err != nil {
		return err
	}
	// R7RS §6.7: string-append returns a newly allocated mutable string
	mc.SetValue(values.NewMutableString(sb.String()))
	return nil
}

// PrimSubstring implements the substring primitive.
// Returns a substring between the given start and end indices.
func PrimSubstring(mc machine.CallContext) error {
	s, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "substring")
	if err != nil {
		return err
	}
	startIdx, err := helpers.RequireArg[*values.Integer](mc, 1, werr.ErrNotAnInteger, "substring")
	if err != nil {
		return err
	}
	endIdx, err := helpers.RequireArg[*values.Integer](mc, 2, werr.ErrNotAnInteger, "substring")
	if err != nil {
		return err
	}
	runes := []rune(s.Value)
	err = helpers.ValidateStartEnd(startIdx.Value, endIdx.Value, int64(len(runes)), "substring")
	if err != nil {
		return err
	}
	// R7RS §6.7: substring is equivalent to string-copy, returns mutable string.
	mc.SetValue(values.NewMutableString(string(runes[startIdx.Value:endIdx.Value])))
	return nil
}

// PrimStringCopy implements the string-copy primitive.
// R7RS §6.7: (string-copy string [start [end]])
// Returns a newly allocated copy of the given string (or substring).
// The returned string is mutable and distinct from the original.
func PrimStringCopy(mc machine.CallContext) error {
	str, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "string-copy")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)

	runes := str.Runes()

	start, end, err := helpers.ParseSubrange(rest, len(runes), "string-copy")
	if err != nil {
		return err
	}

	// Use NewMutableString to ensure the copy is not interned and can be mutated
	mc.SetValue(values.NewMutableString(string(runes[start:end])))
	return nil
}

// stringCompareSpecs defines the five R7RS §6.7 string comparison predicates.
// Each entry has a primitive name, comparison function, and doc string.
var stringCompareSpecs = []struct {
	name string
	cmp  func(string, string) bool
	doc  string
}{
	{"string=?", func(a, b string) bool { return a == b }, "Returns #t if all string arguments have the same sequence of characters.\n\nExamples:\n  (string=? \"abc\" \"abc\")  => #t\n  (string=? \"abc\" \"def\")  => #f"},
	{"string<?", func(a, b string) bool { return a < b }, "Returns #t if string arguments are monotonically increasing in lexicographic order.\n\nExamples:\n  (string<? \"abc\" \"def\")  => #t\n  (string<? \"def\" \"abc\")  => #f"},
	{"string>?", func(a, b string) bool { return a > b }, "Returns #t if string arguments are monotonically decreasing in lexicographic order.\n\nExamples:\n  (string>? \"def\" \"abc\")  => #t\n  (string>? \"abc\" \"def\")  => #f"},
	{"string<=?", func(a, b string) bool { return a <= b }, "Returns #t if string arguments are monotonically non-decreasing in lexicographic order.\n\nExamples:\n  (string<=? \"abc\" \"abc\")  => #t\n  (string<=? \"abc\" \"def\")  => #t"},
	{"string>=?", func(a, b string) bool { return a >= b }, "Returns #t if string arguments are monotonically non-increasing in lexicographic order.\n\nExamples:\n  (string>=? \"def\" \"abc\")  => #t\n  (string>=? \"abc\" \"abc\")  => #t"},
}

// makeStringComparePrimitive returns a ForeignFunction that performs a variadic
// string comparison using the given comparator.
func makeStringComparePrimitive(name string, cmp func(string, string) bool) machine.ForeignFunction {
	return func(mc machine.CallContext) error {
		return helpers.StringCompareVariadic(mc, name, cmp)
	}
}
