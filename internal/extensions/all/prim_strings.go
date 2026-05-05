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
// string-copy!, string-fill!

package all

import (
	"sync"

	"golang.org/x/text/cases"
	"golang.org/x/text/language"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
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
func PrimStringCopyTo(mc machine.CallContext) error {
	to, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "a string", "string-copy!")
	if err != nil {
		return err
	}
	atVal, err := helpers.RequireArg[*values.Integer](mc, 1, werr.ErrNotAnInteger, "an integer", "string-copy!")
	if err != nil {
		return err
	}
	at := int(atVal.Value)
	from, err := helpers.RequireArg[*values.String](mc, 2, werr.ErrNotAString, "a string", "string-copy!")
	if err != nil {
		return err
	}

	start, end, err := helpers.ParseSubrange(mc.Arg(3), from.Len(), "string-copy!")
	if err != nil {
		return err
	}

	toLen := to.Len()
	copyLen := end - start
	if at < 0 || at+copyLen > toLen {
		return werr.WrapForeignErrorf(werr.ErrIndexOutOfRange, "string-copy!: destination index out of bounds")
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
func PrimStringFill(mc machine.CallContext) error {
	s, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "a string", "string-fill!")
	if err != nil {
		return err
	}
	char, err := helpers.RequireArg[*values.Character](mc, 1, werr.ErrNotACharacter, "a character", "string-fill!")
	if err != nil {
		return err
	}

	start, end, err := helpers.ParseSubrange(mc.Arg(2), s.Len(), "string-fill!")
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

// stringCiCompareSpecs defines the five R7RS §6.7 case-insensitive string comparison
// predicates. Each entry has a primitive name, comparison function, and doc string.
// Mirrors stringCompareSpecs in registry/core/prim_strings.go.
var stringCiCompareSpecs = []struct {
	name string
	cmp  func(string, string) bool
	doc  string
}{
	{"string-ci=?", func(a, b string) bool { return getCaseFolded(a) == getCaseFolded(b) }, "Returns #t if all strings are equal after case folding.\n\nExamples:\n  (string-ci=? \"Hello\" \"hello\")  => #t\n  (string-ci=? \"a\" \"b\")          => #f"},
	{"string-ci<?", func(a, b string) bool { return getCaseFolded(a) < getCaseFolded(b) }, "Returns #t if strings are monotonically increasing after case folding.\n\nExamples:\n  (string-ci<? \"a\" \"B\")  => #t"},
	{"string-ci>?", func(a, b string) bool { return getCaseFolded(a) > getCaseFolded(b) }, "Returns #t if strings are monotonically decreasing after case folding.\n\nExamples:\n  (string-ci>? \"B\" \"a\")  => #t"},
	{"string-ci<=?", func(a, b string) bool { return getCaseFolded(a) <= getCaseFolded(b) }, "Returns #t if strings are monotonically non-decreasing after case folding.\n\nExamples:\n  (string-ci<=? \"a\" \"A\")  => #t"},
	{"string-ci>=?", func(a, b string) bool { return getCaseFolded(a) >= getCaseFolded(b) }, "Returns #t if strings are monotonically non-increasing after case folding.\n\nExamples:\n  (string-ci>=? \"A\" \"a\")  => #t"},
}

// makeStringCiComparePrimitive returns a ForeignFunction that performs a variadic
// case-insensitive string comparison using the given comparator.
func makeStringCiComparePrimitive(name string, cmp func(string, string) bool) machine.ForeignFunction {
	return func(mc machine.CallContext) error {
		return helpers.StringCompareVariadic(mc, name, cmp)
	}
}

// makeStringCaser creates a string case-mapping primitive that extracts
// arg 0 as a String, applies a cases.Caser, and returns a new mutable string.
func makeStringCaser(name string, makeCaser func() cases.Caser) machine.ForeignFunction {
	return func(mc machine.CallContext) error {
		str, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "a string", name)
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
