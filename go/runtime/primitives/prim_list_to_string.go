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

// PrimListToString implements the (list->string) primitive.
// Converts a list of characters to a string.
func PrimListToString(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	if values.IsEmptyList(o) {
		mc.SetValue(values.NewString(""))
		return nil
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "list->string: expected a list but got %T", o)
	}
	var runes []rune
	v, err := pr.ForEach(nil, func(_ context.Context, i int, hasNext bool, v values.Value) error {
		ch, ok := v.(*values.Character)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotACharacter, "list->string: expected a character but got %T", v)
		}
		runes = append(runes, ch.Value)
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "list->string: expected a proper list")
	}
	mc.SetValue(values.NewString(string(runes)))
	return nil
}
