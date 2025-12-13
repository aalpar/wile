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

// PrimBytevectorAppend implements the bytevector-append primitive.
// Concatenates bytevectors.
func PrimBytevectorAppend(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	if values.IsEmptyList(o) {
		bv := values.ByteVector{}
		mc.SetValue(&bv)
		return nil
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "bytevector-append: expected a list but got %T", o)
	}
	var result []values.Byte
	v, err := pr.ForEach(nil, func(_ context.Context, i int, hasNext bool, v values.Value) error {
		bv, ok := v.(*values.ByteVector)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAByteVector, "bytevector-append: expected a bytevector but got %T", v)
		}
		result = append(result, (*bv)...)
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "bytevector-append: not a proper list")
	}
	bv := values.ByteVector(result)
	mc.SetValue(&bv)
	return nil
}
