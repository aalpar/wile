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
	"wile/utils"
	"wile/values"
)

// PrimCharGe implements the (char>=?) primitive.
// Returns #t if the first character is greater than or equal to the second, #f otherwise.
func PrimCharGe(_ context.Context, mc *machine.MachineContext) error {
	c1 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	c2 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	ch1, ok := c1.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char>=?: expected a character but got %T", c1)
	}
	ch2, ok := c2.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char>=?: expected a character but got %T", c2)
	}
	mc.SetValue(utils.BoolToBoolean(ch1.Value >= ch2.Value))
	return nil
}
