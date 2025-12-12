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

func PrimGetOutputBytevector(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	p, ok := o.(*values.BytevectorOutputPort)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotABytevectorOutputPort, "get-output-bytevector: expected a bytevector output port but got %T", o)
	}
	// Make a copy of the bytes and convert to ByteVector ([]Byte)
	data := p.GetBytevector()
	result := make(values.ByteVector, len(data))
	for i, b := range data {
		result[i] = values.Byte{Value: b}
	}
	mc.SetValue(&result)
	return nil
}
