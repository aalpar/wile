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
	"errors"

	"wile/machine"
	"wile/values"
)

// PrimAssv implements the assv primitive.
// Finds pair in alist by key using eqv?
func PrimAssv(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	alist := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	pr, ok := alist.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "assv: expected a list but got %T", alist)
	}
	v, err := pr.ForEach(nil, func(i int, hasNext bool, elem values.Value) error {
		entry, ok := elem.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "assv: expected a pair in alist but got %T", elem)
		}
		if Eqv(entry.Car(), obj) {
			mc.SetValue(entry)
			return values.ErrStopIteration
		}
		return nil
	})
	if errors.Is(err, values.ErrStopIteration) {
		return nil
	}
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "assv: expected a proper list")
	}
	mc.SetValue(values.FalseValue)
	return nil
}
