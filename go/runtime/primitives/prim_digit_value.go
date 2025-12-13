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

	"wile/machine"
	"wile/values"
)

// PrimDigitValue implements the (digit-value) primitive.
// Returns the numeric value of a digit character, or #f if not a digit.
func PrimDigitValue(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "digit-value: expected a character but got %T", o)
	}
	if ch.Value >= '0' && ch.Value <= '9' {
		mc.SetValue(values.NewInteger(int64(ch.Value - '0')))
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}
