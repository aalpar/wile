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

// PrimBytevector implements the bytevector primitive.
// Creates bytevector from byte arguments.
func PrimBytevector(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	if values.IsEmptyList(o) {
		bv := values.ByteVector{}
		mc.SetValue(&bv)
		return nil
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "bytevector: expected a list but got %T", o)
	}
	var bytes []values.Byte
	v, err := pr.ForEach(nil, func(_ context.Context, i int, hasNext bool, v values.Value) error {
		intVal, ok := v.(*values.Integer)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAnInteger, "bytevector: expected an integer but got %T", v)
		}
		if intVal.Value < 0 || intVal.Value > 255 {
			return values.NewForeignError("bytevector: value must be a byte (0-255)")
		}
		bytes = append(bytes, values.Byte{Value: uint8(intVal.Value)})
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "bytevector: not a proper list")
	}
	bv := values.ByteVector(bytes)
	mc.SetValue(&bv)
	return nil
}
