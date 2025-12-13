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

// PrimVectorSet implements the vector-set! primitive.
// Sets the element of a vector at the given index to a new value.
func PrimVectorSet(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	k := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	obj := mc.EnvironmentFrame().GetLocalBindingByIndex(2).Value()
	v, ok := o.(*values.Vector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAVector, "vector-set!: expected a vector but got %T", o)
	}
	idx, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "vector-set!: expected an integer but got %T", k)
	}
	if idx.Value < 0 || idx.Value >= int64(len(*v)) {
		return values.NewForeignError("vector-set!: index out of bounds")
	}
	(*v)[idx.Value] = obj
	mc.SetValues()
	return nil
}
