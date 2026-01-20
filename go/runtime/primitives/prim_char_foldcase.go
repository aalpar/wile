// Copyright 2025 Aaron Alpar
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

package primitives

import (
	"context"
	"unicode"

	"wile/machine"
	"wile/values"
)

// PrimCharFoldcase implements the (char-foldcase) primitive.
// Returns the case-folded version of the character for case-insensitive comparison.
//
// R7RS Specification (Section 6.6 "Characters"):
//   "The char-foldcase procedure applies the Unicode simple case-folding
//   algorithm to its argument and returns the result."
//
// Current Implementation:
//   Uses Go's unicode.ToLower(), which performs Unicode lowercasing rather than
//   Unicode SimpleCaseFolding. For most characters these are equivalent, but they
//   differ in specific cases:
//
//   - U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE (İ):
//       SimpleCaseFolding: U+0069 (i)
//       ToLower:           U+0069 (i)  [same]
//
//   - U+0049 LATIN CAPITAL LETTER I (I) in Turkish locale:
//       SimpleCaseFolding: U+0069 (i)  [locale-independent]
//       ToLower:           may vary by locale
//
//   - U+212A KELVIN SIGN (K):
//       SimpleCaseFolding: U+006B (k)
//       ToLower:           U+006B (k)  [same]
//
// The practical difference is minimal for most use cases. To achieve full R7RS
// conformance, this should use golang.org/x/text/cases with cases.Fold().
//
// Reference: Unicode Standard Annex #44, CaseFolding.txt
// https://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt
func PrimCharFoldcase(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char-foldcase: expected a character but got %T", o)
	}
	mc.SetValue(values.NewCharacter(unicode.ToLower(ch.Value)))
	return nil
}
