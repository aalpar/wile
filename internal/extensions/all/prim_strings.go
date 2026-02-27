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
func PrimStringCopyTo(mc *machine.MachineContext) error {
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
func PrimStringFill(mc *machine.MachineContext) error {
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
func PrimStringMap(mc *machine.MachineContext) error {
	proc := mc.Arg(0)
	stringsVal := mc.Arg(1)

	mcls, err := helpers.RequireType[machine.Closure](proc, values.ErrNotAProcedure, "string-map")
	if err != nil {
		return err
	}

	if values.IsEmptyList(stringsVal) {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "string-map: expected at least one string")
	}

	strs, runeSlices, minLen, err := helpers.CollectStrings(stringsVal, "string-map")
	if err != nil {
		return err
	}
	if len(strs) == 0 {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "string-map: expected at least one string")
	}

	result := make([]rune, minLen)
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)

	for i := range minLen {
		args := make(values.Vector, len(strs))
		for j := range strs {
			args[j] = values.NewCharacter(runeSlices[j][i])
		}

		_, err := sub.ApplyCallable(mcls, args...)
		if err != nil {
			return err
		}
		err = sub.Run()
		if err != nil {
			return err
		}

		resultVal := sub.GetValue()
		char, ok := resultVal.(*values.Character)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotACharacter, "string-map: procedure must return a character but got %T", resultVal)
		}
		result[i] = char.Value
	}

	mc.SetValue(values.NewMutableString(string(result)))
	return nil
}

// PrimStringForEach implements the string-for-each primitive.
// R7RS §6.7: (string-for-each proc string1 string2 ...)
func PrimStringForEach(mc *machine.MachineContext) error {
	proc := mc.Arg(0)
	stringsVal := mc.Arg(1)

	mcls, err := helpers.RequireType[machine.Closure](proc, values.ErrNotAProcedure, "string-for-each")
	if err != nil {
		return err
	}

	if values.IsEmptyList(stringsVal) {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "string-for-each: expected at least one string")
	}

	strs, runeSlices, minLen, err := helpers.CollectStrings(stringsVal, "string-for-each")
	if err != nil {
		return err
	}
	if len(strs) == 0 {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "string-for-each: expected at least one string")
	}

	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)

	for i := range minLen {
		args := make(values.Vector, len(strs))
		for j := range strs {
			args[j] = values.NewCharacter(runeSlices[j][i])
		}

		_, err := sub.ApplyCallable(mcls, args...)
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

// stringCiCompareSpecs defines the five R7RS §6.7 case-insensitive string comparison
// predicates. Each entry pairs a primitive name with its comparison function.
// Mirrors charCiCompareSpecs in prim_characters.go.
var stringCiCompareSpecs = []struct {
	name string
	cmp  func(string, string) bool
}{
	{"string-ci=?", func(a, b string) bool { return getCaseFolded(a) == getCaseFolded(b) }},
	{"string-ci<?", func(a, b string) bool { return getCaseFolded(a) < getCaseFolded(b) }},
	{"string-ci>?", func(a, b string) bool { return getCaseFolded(a) > getCaseFolded(b) }},
	{"string-ci<=?", func(a, b string) bool { return getCaseFolded(a) <= getCaseFolded(b) }},
	{"string-ci>=?", func(a, b string) bool { return getCaseFolded(a) >= getCaseFolded(b) }},
}

// makeStringCiComparePrimitive returns a ForeignFunction that performs a variadic
// case-insensitive string comparison using the given comparator.
func makeStringCiComparePrimitive(name string, cmp func(string, string) bool) machine.ForeignFunction {
	return func(mc *machine.MachineContext) error {
		return helpers.StringCompareVariadic(mc, name, cmp)
	}
}

// makeStringCaser creates a string case-mapping primitive that extracts
// arg 0 as a String, applies a cases.Caser, and returns a new mutable string.
func makeStringCaser(name string, makeCaser func() cases.Caser) machine.ForeignFunction {
	return func(mc *machine.MachineContext) error {
		str, err := helpers.RequireArg[*values.String](mc, 0, values.ErrNotAString, name)
		if err != nil {
			return err
		}
		caser := makeCaser()
		result := caser.String(str.Value)
		mc.SetValue(values.NewMutableString(result))
		return nil
	}
}

// String case-mapping primitives — R7RS §6.7.
// Uses Unicode full case mapping (language-independent).

var PrimStringUpcase = makeStringCaser("string-upcase", func() cases.Caser {
	return cases.Upper(language.Und)
})

var PrimStringDowncase = makeStringCaser("string-downcase", func() cases.Caser {
	return cases.Lower(language.Und)
})

var PrimStringFoldcase = makeStringCaser("string-foldcase", func() cases.Caser {
	return cases.Fold()
})
