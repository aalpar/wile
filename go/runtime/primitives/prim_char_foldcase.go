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
func PrimCharFoldcase(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char-foldcase: expected a character but got %T", o)
	}
	mc.SetValue(values.NewCharacter(unicode.ToLower(ch.Value)))
	return nil
}
