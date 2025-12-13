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

// PrimListTail implements the (list-tail) primitive.
// Returns the sublist starting at the given index.
func PrimListTail(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	k := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	idx, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "list-tail: expected an integer index but got %T", k)
	}
	if idx.Value < 0 {
		return values.NewForeignError("list-tail: index must be non-negative")
	}
	if idx.Value == 0 {
		mc.SetValue(o)
		return nil
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "list-tail: expected a pair but got %T", o)
	}
	for i := int64(0); i < idx.Value; i++ {
		next := pr.Cdr()
		if values.IsEmptyList(next) {
			if i == idx.Value-1 {
				mc.SetValue(values.EmptyList)
				return nil
			}
			return values.NewForeignError("list-tail: index out of bounds")
		}
		pr, ok = next.(*values.Pair)
		if !ok {
			if i == idx.Value-1 {
				mc.SetValue(next)
				return nil
			}
			return values.WrapForeignErrorf(values.ErrNotAPair, "list-tail: expected a pair but got %T", next)
		}
	}
	mc.SetValue(pr)
	return nil
}
