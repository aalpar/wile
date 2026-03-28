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

// Character primitives: case-insensitive comparison, Unicode case mapping,
// character classification, digit-value

package all

import (
	"unicode"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// charCiCompareSpecs defines the five R7RS §6.6 case-insensitive character comparison
// predicates. Each entry has a primitive name, comparison function, and doc string.
// Mirrors charCompareSpecs in registry/core/prim_characters.go.
var charCiCompareSpecs = []struct {
	name string
	cmp  func(rune, rune) bool
	doc  string
}{
	{"char-ci=?", func(a, b rune) bool { return simpleCaseFold(a) == simpleCaseFold(b) }, "Returns #t if all characters are equal after case folding."},
	{"char-ci<?", func(a, b rune) bool { return simpleCaseFold(a) < simpleCaseFold(b) }, "Returns #t if characters are monotonically increasing after case folding."},
	{"char-ci>?", func(a, b rune) bool { return simpleCaseFold(a) > simpleCaseFold(b) }, "Returns #t if characters are monotonically decreasing after case folding."},
	{"char-ci<=?", func(a, b rune) bool { return simpleCaseFold(a) <= simpleCaseFold(b) }, "Returns #t if characters are monotonically non-decreasing after case folding."},
	{"char-ci>=?", func(a, b rune) bool { return simpleCaseFold(a) >= simpleCaseFold(b) }, "Returns #t if characters are monotonically non-increasing after case folding."},
}

// makeCharCiComparePrimitive returns a ForeignFunction that performs a variadic
// case-insensitive character comparison using the given comparator.
func makeCharCiComparePrimitive(name string, cmp func(rune, rune) bool) machine.ForeignFunction {
	return func(mc *machine.MachineContext) error {
		return helpers.CharCompareVariadic(mc, name, cmp)
	}
}

// Character classification predicates — all use MakeCharPredicate factory.
var PrimCharAlphabeticQ = helpers.MakeCharPredicate("char-alphabetic?", unicode.IsLetter)
var PrimCharNumericQ = helpers.MakeCharPredicate("char-numeric?", unicode.IsDigit)
var PrimCharWhitespaceQ = helpers.MakeCharPredicate("char-whitespace?", unicode.IsSpace)
var PrimCharUpperCaseQ = helpers.MakeCharPredicate("char-upper-case?", unicode.IsUpper)
var PrimCharLowerCaseQ = helpers.MakeCharPredicate("char-lower-case?", unicode.IsLower)

// Character case transformation — all use MakeCharTransform factory.
var PrimCharUpcase = helpers.MakeCharTransform("char-upcase", unicode.ToUpper)
var PrimCharDowncase = helpers.MakeCharTransform("char-downcase", unicode.ToLower)
var PrimCharFoldcase = helpers.MakeCharTransform("char-foldcase", simpleCaseFold)

// simpleCaseFold performs Unicode simple case folding on a rune.
// Simple case folding maps each character to exactly one character.
// This is used for case-insensitive matching as specified by R7RS.
//
// For most characters, simple case folding is equivalent to lowercase.
// Special cases are handled based on Unicode CaseFolding.txt:
//   - Capital sharp S (ẞ U+1E9E) folds to lowercase sharp s (ß U+00DF)
//   - Most other characters just use ToLower
func simpleCaseFold(r rune) rune {
	// Handle special cases from Unicode CaseFolding.txt
	switch r {
	case 'ẞ': // U+1E9E LATIN CAPITAL LETTER SHARP S
		return 'ß' // U+00DF LATIN SMALL LETTER SHARP S
	case 'K': // U+212A KELVIN SIGN (if we get that far)
		// Actually this is regular K, ignore
	}
	// For most characters, simple case folding equals lowercase
	return unicode.ToLower(r)
}

// PrimDigitValue implements the (digit-value) primitive.
// R7RS §6.6: Returns the numeric value (0-9) of a character that is a decimal digit
// according to Unicode, or #f if it is not a decimal digit.
func PrimDigitValue(mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Character](mc, 0, werr.ErrNotACharacter, "digit-value")
	if err != nil {
		return err
	}
	// Check if it's a Unicode decimal digit (Nd category)
	// Unicode decimal digits have values 0-9 within their respective scripts
	if unicode.IsDigit(ch.Value) {
		// Get the digit value by finding the base '0' for this script
		// Unicode organizes decimal digits in blocks of 10: 0, 1, 2, ..., 9
		base := ch.Value
		for unicode.IsDigit(base - 1) {
			base--
		}
		digit := int64(ch.Value - base)
		if digit >= 0 && digit <= 9 {
			mc.SetValue(values.NewInteger(digit))
			return nil
		}
	}
	mc.SetValue(values.FalseValue)
	return nil
}
