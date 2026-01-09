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
	"wile/utils"
	"wile/values"
)

func makeCharPredicate(name string, check func(rune) bool) func(context.Context, *machine.MachineContext) error {
	return func(_ context.Context, mc *machine.MachineContext) error {
		o := mc.Arg(0)
		ch, ok := o.(*values.Character)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotACharacter, "%s: expected a character but got %T", name, o)
		}
		mc.SetValue(utils.BoolToBoolean(check(ch.Value)))
		return nil
	}
}

var PrimCharUpperCaseQ = makeCharPredicate("char-upper-case?", unicode.IsUpper)
var PrimCharLowerCaseQ = makeCharPredicate("char-lower-case?", unicode.IsLower)
var PrimCharWhitespaceQ = makeCharPredicate("char-whitespace?", unicode.IsSpace)
var PrimCharAlphabeticQ = makeCharPredicate("char-alphabetic?", unicode.IsLetter)
var PrimCharNumericQ = makeCharPredicate("char-numeric?", unicode.IsDigit)
