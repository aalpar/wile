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

func PrimListSet(_ context.Context, mc *machine.MachineContext) error {
	listVal := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	idxVal := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	val := mc.EnvironmentFrame().GetLocalBindingByIndex(2).Value()

	p, ok := listVal.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "list-set!: expected a list but got %T", listVal)
	}

	idx, ok := idxVal.(*values.Integer)
	if !ok {
		return values.NewForeignError("list-set!: expected an integer index")
	}
	k := int(idx.Value)
	if k < 0 {
		return values.NewForeignError("list-set!: index must be non-negative")
	}

	current := p
	for i := 0; i < k; i++ {
		if current.IsEmptyList() {
			return values.NewForeignError("list-set!: index out of range")
		}
		cdr := current.Cdr()
		next, ok := cdr.(*values.Pair)
		if !ok {
			return values.NewForeignError("list-set!: index out of range")
		}
		current = next
	}

	if current.IsEmptyList() {
		return values.NewForeignError("list-set!: index out of range")
	}

	current.SetCar(val)
	mc.SetValue(values.Void)
	return nil
}
