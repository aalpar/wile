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
	"io"

	"wile/machine"
	"wile/values"
)

// PrimClosePort implements the (close-port) primitive.
// Closes an input or output port.
func PrimClosePort(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch p := o.(type) {
	case *values.CharacterInputPort:
		if closer, ok := p.Value.(io.Closer); ok {
			if err := closer.Close(); err != nil {
				return values.WrapForeignErrorf(err, "close-port: %v", err)
			}
		}
	case *values.CharacterOutputPort:
		if closer, ok := p.Value.(io.Closer); ok {
			if err := closer.Close(); err != nil {
				return values.WrapForeignErrorf(err, "close-port: %v", err)
			}
		}
	case *values.StringInputPort:
		// String ports don't need closing, just no-op
	case *values.StringOutputPort:
		// String ports don't need closing, just no-op
	case *values.BytevectorInputPort:
		// Bytevector ports don't need closing, just no-op
	case *values.BytevectorOutputPort:
		// Bytevector ports don't need closing, just no-op
	case *values.BinaryInputPort:
		if closer, ok := p.Value.(io.Closer); ok {
			if err := closer.Close(); err != nil {
				return values.WrapForeignErrorf(err, "close-port: %v", err)
			}
		}
	case *values.BinaryOutputPort:
		if closer, ok := p.Value.(io.Closer); ok {
			if err := closer.Close(); err != nil {
				return values.WrapForeignErrorf(err, "close-port: %v", err)
			}
		}
	default:
		return values.WrapForeignErrorf(values.ErrNotAnInputPort, "close-port: expected a port but got %T", o)
	}
	mc.SetValues()
	return nil
}
