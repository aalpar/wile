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

// PrimStringToList implements the string->list primitive.
// Converts a string to a list of characters.
func PrimStringToList(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	s, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string->list: expected a string but got %T", o)
	}
	runes := []rune(s.Value)
	var result values.Value = values.EmptyList
	for i := len(runes) - 1; i >= 0; i-- {
		result = values.NewCons(values.NewCharacter(runes[i]), result)
	}
	mc.SetValue(result)
	return nil
}
