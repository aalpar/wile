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

// String primitives: case-insensitive comparison, Unicode case mapping,
// string-copy!, string-fill!, string-map, string-for-each

package all

import (
	"context"
	"sync"

	"golang.org/x/text/cases"
	"golang.org/x/text/language"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
)

var (
	// caseFolderOnce ensures the case folder is initialized exactly once
	caseFolderOnce sync.Once
	// caseFolder is the Unicode case folder for string-ci comparisons
	caseFolder cases.Caser
)

// getCaseFolded returns the case-folded version of a string.
// Uses lazy initialization of the case folder via sync.Once.
// R7RS §6.7: Case-insensitive comparisons should use case folding.
func getCaseFolded(s string) string {
	caseFolderOnce.Do(func() {
		caseFolder = cases.Fold()
	})
	return caseFolder.String(s)
}

// PrimStringCopyTo implements the string-copy! primitive.
// R7RS §6.7: (string-copy! to at from [start [end]])
func PrimStringCopyTo(_ context.Context, mc *machine.MachineContext) error {
	toArg := mc.Arg(0)
	rest := mc.Arg(1)

	to, err := helpers.RequireType[*values.String](toArg, values.ErrNotAString, "string-copy!")
	if err != nil {
		return err
	}

	// Extract required arguments: at from
	tuple, ok := rest.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "string-copy!: improper argument list")
	}
	if tuple.IsEmptyList() {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "string-copy!: expected at least 3 arguments")
	}

	atVal, err := helpers.RequireType[*values.Integer](tuple.Car(), values.ErrNotANumber, "string-copy!")
	if err != nil {
		return err
	}
	at := int(atVal.Value)

	tuple2, ok := tuple.Cdr().(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "string-copy!: improper argument list")
	}
	if tuple2.IsEmptyList() {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "string-copy!: expected at least 3 arguments")
	}

	from, err := helpers.RequireType[*values.String](tuple2.Car(), values.ErrNotAString, "string-copy!")
	if err != nil {
		return err
	}

	// Extract optional [start [end]] from remaining arguments
	start, end, err := helpers.ParseSubrange(tuple2.Cdr(), from.Len(), "string-copy!")
	if err != nil {
		return err
	}

	toLen := to.Len()
	copyLen := end - start
	if at < 0 || at+copyLen > toLen {
		return values.WrapForeignErrorf(values.ErrIndexOutOfRange, "string-copy!: destination index out of bounds")
	}

	// Perform the copy
	toRunes := to.Runes()
	fromRunes := from.Runes()
	copy(toRunes[at:], fromRunes[start:end])

	// SetValue checks immutability
	err = to.SetValue(string(toRunes))
	if err != nil {
		return err
	}

	mc.SetValue(values.Void)
	return nil
}

// PrimStringFill implements the string-fill! primitive.
// R7RS §6.7: (string-fill! string fill [start [end]])
func PrimStringFill(_ context.Context, mc *machine.MachineContext) error {
	s, err := helpers.RequireArg[*values.String](mc, 0, values.ErrNotAString, "string-fill!")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)

	// Extract required argument: fill
	tuple, ok := rest.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "string-fill!: improper argument list")
	}
	if tuple.IsEmptyList() {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "string-fill!: expected at least 2 arguments")
	}

	char, err := helpers.RequireType[*values.Character](tuple.Car(), values.ErrNotACharacter, "string-fill!")
	if err != nil {
		return err
	}

	// Extract optional [start [end]] from remaining arguments
	start, end, err := helpers.ParseSubrange(tuple.Cdr(), s.Len(), "string-fill!")
	if err != nil {
		return err
	}

	// Fill checks immutability
	err = s.Fill(char.Value, start, end)
	if err != nil {
		return err
	}

	mc.SetValue(values.Void)
	return nil
}

// PrimStringMap implements the string-map primitive.
// R7RS §6.7: (string-map proc string1 string2 ...)
func PrimStringMap(_ context.Context, mc *machine.MachineContext) error {
	proc := mc.Arg(0)
	stringsVal := mc.Arg(1)

	mcls, err := helpers.RequireType[*machine.MachineClosure](proc, values.ErrNotAProcedure, "string-map")
	if err != nil {
		return err
	}

	if values.IsEmptyList(stringsVal) {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "string-map: expected at least one string")
	}

	// Collect all strings into a slice
	var strs []*values.String
	current := stringsVal
	for !values.IsEmptyList(current) {
		tuple, ok := current.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "string-map: improper argument list")
		}
		s, err := helpers.RequireType[*values.String](tuple.Car(), values.ErrNotAString, "string-map")
		if err != nil {
			return err
		}
		strs = append(strs, s)
		current = tuple.Cdr()
	}

	if len(strs) == 0 {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "string-map: expected at least one string")
	}

	// Convert all strings to rune slices and find minimum length
	runeSlices := make([][]rune, len(strs))
	minLen := -1
	for i, s := range strs {
		runeSlices[i] = s.Runes()
		if minLen < 0 || len(runeSlices[i]) < minLen {
			minLen = len(runeSlices[i])
		}
	}

	// Apply proc to each position
	result := make([]rune, minLen)
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)

	for i := 0; i < minLen; i++ {
		// Collect one character from each string
		args := make(values.Vector, len(strs))
		for j := range strs {
			args[j] = values.NewCharacter(runeSlices[j][i])
		}

		// Apply proc to collected arguments
		_, err := sub.Apply(mcls, args...)
		if err != nil {
			return err
		}
		err = sub.Run()
		if err != nil {
			return err
		}

		// Get the result character
		resultVal := sub.GetValue()
		char, ok := resultVal.(*values.Character)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotACharacter, "string-map: procedure must return a character but got %T", resultVal)
		}
		result[i] = char.Value
	}

	// R7RS §6.7: string-map returns a newly allocated mutable string
	mc.SetValue(values.NewMutableString(string(result)))
	return nil
}

// PrimStringForEach implements the string-for-each primitive.
// R7RS §6.7: (string-for-each proc string1 string2 ...)
func PrimStringForEach(_ context.Context, mc *machine.MachineContext) error {
	proc := mc.Arg(0)
	stringsVal := mc.Arg(1)

	mcls, err := helpers.RequireType[*machine.MachineClosure](proc, values.ErrNotAProcedure, "string-for-each")
	if err != nil {
		return err
	}

	if values.IsEmptyList(stringsVal) {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "string-for-each: expected at least one string")
	}

	// Collect all strings into a slice
	var strs []*values.String
	current := stringsVal
	for !values.IsEmptyList(current) {
		tuple, ok := current.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "string-for-each: improper argument list")
		}
		s, err := helpers.RequireType[*values.String](tuple.Car(), values.ErrNotAString, "string-for-each")
		if err != nil {
			return err
		}
		strs = append(strs, s)
		current = tuple.Cdr()
	}

	if len(strs) == 0 {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "string-for-each: expected at least one string")
	}

	// Convert all strings to rune slices and find minimum length
	runeSlices := make([][]rune, len(strs))
	minLen := -1
	for i, s := range strs {
		runeSlices[i] = s.Runes()
		if minLen < 0 || len(runeSlices[i]) < minLen {
			minLen = len(runeSlices[i])
		}
	}

	// Apply proc to each position
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)

	for i := 0; i < minLen; i++ {
		// Collect one character from each string
		args := make(values.Vector, len(strs))
		for j := range strs {
			args[j] = values.NewCharacter(runeSlices[j][i])
		}

		// Apply proc to collected arguments
		_, err := sub.Apply(mcls, args...)
		if err != nil {
			return err
		}
		err = sub.Run()
		if err != nil {
			return err
		}
	}

	mc.SetValue(values.Void)
	return nil
}

// PrimStringCiEqVariadic implements the variadic string-ci=? primitive.
func PrimStringCiEqVariadic(_ context.Context, mc *machine.MachineContext) error {
	return helpers.StringCompareVariadic(mc, "string-ci=?", func(a, b string) bool {
		return getCaseFolded(a) == getCaseFolded(b)
	})
}

// PrimStringCiLtVariadic implements the variadic string-ci<? primitive.
func PrimStringCiLtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return helpers.StringCompareVariadic(mc, "string-ci<?", func(a, b string) bool {
		return getCaseFolded(a) < getCaseFolded(b)
	})
}

// PrimStringCiGtVariadic implements the variadic string-ci>? primitive.
func PrimStringCiGtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return helpers.StringCompareVariadic(mc, "string-ci>?", func(a, b string) bool {
		return getCaseFolded(a) > getCaseFolded(b)
	})
}

// PrimStringCiLeVariadic implements the variadic string-ci<=? primitive.
func PrimStringCiLeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return helpers.StringCompareVariadic(mc, "string-ci<=?", func(a, b string) bool {
		return getCaseFolded(a) <= getCaseFolded(b)
	})
}

// PrimStringCiGeVariadic implements the variadic string-ci>=? primitive.
func PrimStringCiGeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return helpers.StringCompareVariadic(mc, "string-ci>=?", func(a, b string) bool {
		return getCaseFolded(a) >= getCaseFolded(b)
	})
}

// PrimStringUpcase implements the string-upcase primitive.
// R7RS §6.7: Returns a string whose characters are the uppercase versions of the characters in string.
// Uses Unicode full case mapping which can expand characters (e.g., ß → SS).
func PrimStringUpcase(_ context.Context, mc *machine.MachineContext) error {
	str, err := helpers.RequireArg[*values.String](mc, 0, values.ErrNotAString, "string-upcase")
	if err != nil {
		return err
	}
	// Use Unicode full case mapping (language-independent)
	caser := cases.Upper(language.Und)
	result := caser.String(str.Value)
	// R7RS §6.7: string-upcase returns a newly allocated mutable string
	mc.SetValue(values.NewMutableString(result))
	return nil
}

// PrimStringDowncase implements the string-downcase primitive.
// R7RS §6.7: Returns a string whose characters are the lowercase versions of the characters in string.
// Uses Unicode full case mapping which can expand characters.
func PrimStringDowncase(_ context.Context, mc *machine.MachineContext) error {
	str, err := helpers.RequireArg[*values.String](mc, 0, values.ErrNotAString, "string-downcase")
	if err != nil {
		return err
	}
	// Use Unicode full case mapping (language-independent)
	caser := cases.Lower(language.Und)
	result := caser.String(str.Value)
	// R7RS §6.7: string-downcase returns a newly allocated mutable string
	mc.SetValue(values.NewMutableString(result))
	return nil
}

// PrimStringFoldcase implements the string-foldcase primitive.
// R7RS §6.7: Returns a string whose characters are the case-folded versions of the characters in string.
// Uses Unicode full case folding which can expand characters (e.g., ß → ss).
func PrimStringFoldcase(_ context.Context, mc *machine.MachineContext) error {
	str, err := helpers.RequireArg[*values.String](mc, 0, values.ErrNotAString, "string-foldcase")
	if err != nil {
		return err
	}
	// Use Unicode full case folding
	caser := cases.Fold()
	result := caser.String(str.Value)
	// R7RS §6.7: string-foldcase returns a newly allocated mutable string
	mc.SetValue(values.NewMutableString(result))
	return nil
}
