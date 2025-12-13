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

// PrimListRef implements the (list-ref) primitive.
// Returns the element at the given index in a list.
func PrimListRef(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	k := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	idx, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "list-ref: expected an integer index but got %T", k)
	}
	if idx.Value < 0 {
		return values.NewForeignError("list-ref: index must be non-negative")
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "list-ref: expected a pair but got %T", o)
	}
	for i := int64(0); i < idx.Value; i++ {
		next := pr.Cdr()
		if values.IsEmptyList(next) {
			return values.NewForeignError("list-ref: index out of bounds")
		}
		pr, ok = next.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "list-ref: expected a pair but got %T", next)
		}
	}
	mc.SetValue(pr.Car())
	return nil
}
