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
	"context"
	"unicode"

	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
)

// charCompareVariadic is a helper for variadic character comparison primitives.
func charCompareVariadic(mc *machine.MachineContext, name string, cmp func(a, b rune) bool) error {
	return variadicCompare(mc, name,
		func(v values.Value) (*values.Character, bool) {
			c, ok := v.(*values.Character)
			return c, ok
		},
		func(c *values.Character) rune {
			return c.Value
		},
		cmp,
		values.ErrNotACharacter,
		"a character")
}

// PrimCharCiEqVariadic implements the variadic char-ci=? primitive.
func PrimCharCiEqVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char-ci=?", func(a, b rune) bool {
		return unicode.ToLower(a) == unicode.ToLower(b)
	})
}

// PrimCharCiLtVariadic implements the variadic char-ci<? primitive.
func PrimCharCiLtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char-ci<?", func(a, b rune) bool {
		return unicode.ToLower(a) < unicode.ToLower(b)
	})
}

// PrimCharCiGtVariadic implements the variadic char-ci>? primitive.
func PrimCharCiGtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char-ci>?", func(a, b rune) bool {
		return unicode.ToLower(a) > unicode.ToLower(b)
	})
}

// PrimCharCiLeVariadic implements the variadic char-ci<=? primitive.
func PrimCharCiLeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char-ci<=?", func(a, b rune) bool {
		return unicode.ToLower(a) <= unicode.ToLower(b)
	})
}

// PrimCharCiGeVariadic implements the variadic char-ci>=? primitive.
func PrimCharCiGeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char-ci>=?", func(a, b rune) bool {
		return unicode.ToLower(a) >= unicode.ToLower(b)
	})
}

// PrimCharAlphabeticQ tests if a character is alphabetic.
func PrimCharAlphabeticQ(_ context.Context, mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Character](mc, 0, values.ErrNotACharacter, "char-alphabetic?")
	if err != nil {
		return err
	}
	mc.SetValue(schemeutil.BoolToBoolean(unicode.IsLetter(ch.Value)))
	return nil
}

// PrimCharNumericQ tests if a character is numeric.
func PrimCharNumericQ(_ context.Context, mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Character](mc, 0, values.ErrNotACharacter, "char-numeric?")
	if err != nil {
		return err
	}
	mc.SetValue(schemeutil.BoolToBoolean(unicode.IsDigit(ch.Value)))
	return nil
}

// PrimCharWhitespaceQ tests if a character is whitespace.
func PrimCharWhitespaceQ(_ context.Context, mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Character](mc, 0, values.ErrNotACharacter, "char-whitespace?")
	if err != nil {
		return err
	}
	mc.SetValue(schemeutil.BoolToBoolean(unicode.IsSpace(ch.Value)))
	return nil
}

// PrimCharUpperCaseQ tests if a character is uppercase.
func PrimCharUpperCaseQ(_ context.Context, mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Character](mc, 0, values.ErrNotACharacter, "char-upper-case?")
	if err != nil {
		return err
	}
	mc.SetValue(schemeutil.BoolToBoolean(unicode.IsUpper(ch.Value)))
	return nil
}

// PrimCharLowerCaseQ tests if a character is lowercase.
func PrimCharLowerCaseQ(_ context.Context, mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Character](mc, 0, values.ErrNotACharacter, "char-lower-case?")
	if err != nil {
		return err
	}
	mc.SetValue(schemeutil.BoolToBoolean(unicode.IsLower(ch.Value)))
	return nil
}

// PrimCharUpcase returns the uppercase version of a character.
func PrimCharUpcase(_ context.Context, mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Character](mc, 0, values.ErrNotACharacter, "char-upcase")
	if err != nil {
		return err
	}
	mc.SetValue(values.NewCharacter(unicode.ToUpper(ch.Value)))
	return nil
}

// PrimCharDowncase returns the lowercase version of a character.
func PrimCharDowncase(_ context.Context, mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Character](mc, 0, values.ErrNotACharacter, "char-downcase")
	if err != nil {
		return err
	}
	mc.SetValue(values.NewCharacter(unicode.ToLower(ch.Value)))
	return nil
}

// PrimCharFoldcase returns the case-folded version of a character.
// R7RS §6.6: Returns the simple Unicode case-folded version of the character.
// Simple case folding maps each character to exactly one character.
func PrimCharFoldcase(_ context.Context, mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Character](mc, 0, values.ErrNotACharacter, "char-foldcase")
	if err != nil {
		return err
	}
	mc.SetValue(values.NewCharacter(simpleCaseFold(ch.Value)))
	return nil
}

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
func PrimDigitValue(_ context.Context, mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Character](mc, 0, values.ErrNotACharacter, "digit-value")
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
