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

// PrimMakeBytevector implements the (make-bytevector) primitive.
// Creates a bytevector of the given size, optionally filled with a specified byte value.
func PrimMakeBytevector(_ context.Context, mc *machine.MachineContext) error {
	k := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	size, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInteger, "make-bytevector: expected an integer but got %T", k)
	}
	if size.Value < 0 {
		return values.NewForeignError("make-bytevector: size must be non-negative")
	}
	var fill uint8 = 0
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(*values.Pair)
		if ok && !values.IsEmptyList(pr) {
			fillVal := pr.Car()
			fillInt, ok := fillVal.(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAnInteger, "make-bytevector: fill must be an integer but got %T", fillVal)
			}
			if fillInt.Value < 0 || fillInt.Value > 255 {
				return values.NewForeignError("make-bytevector: fill must be a byte (0-255)")
			}
			fill = uint8(fillInt.Value)
		}
	}
	bv := make(values.ByteVector, size.Value)
	for i := range bv {
		bv[i] = values.Byte{Value: fill}
	}
	mc.SetValue(&bv)
	return nil
}
