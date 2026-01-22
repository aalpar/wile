// Copyright 2026 Aaron Alpar
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

package io

import (
	"context"
	"io"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// PrimPortQ implements the port? primitive.
// Returns #t if the argument is a port (input or output), #f otherwise.
func PrimPortQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch o.(type) {
	case *values.CharacterInputPort, *values.CharacterOutputPort,
		*values.StringInputPort, *values.StringOutputPort,
		*values.BytevectorInputPort, *values.BytevectorOutputPort:
		mc.SetValue(values.TrueValue)
	default:
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimInputPortQ implements the (input-port?) primitive.
// Returns #t if argument is input port.
func PrimInputPortQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch o.(type) {
	case *values.CharacterInputPort, *values.StringInputPort, *values.BytevectorInputPort, *values.BinaryInputPort:
		mc.SetValue(values.TrueValue)
	default:
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimOutputPortQ implements the output-port? primitive.
// Returns #t if the argument is an output port, #f otherwise.
func PrimOutputPortQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch o.(type) {
	case *values.CharacterOutputPort, *values.StringOutputPort, *values.BytevectorOutputPort, *values.BinaryOutputPort:
		mc.SetValue(values.TrueValue)
	default:
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimInputPortOpenQ implements the (input-port-open?) primitive.
// Returns #t if input port is open.
func PrimInputPortOpenQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.CharacterInputPort)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInputPort, "input-port-open?: expected an input port but got %T", o)
	}
	// For now, we assume all ports are open (proper tracking would require port state)
	mc.SetValue(values.TrueValue)
	return nil
}

// PrimOutputPortOpenQ implements the output-port-open? primitive.
// Returns #t if the output port is open, #f otherwise.
func PrimOutputPortOpenQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.CharacterOutputPort)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnOutputPort, "output-port-open?: expected an output port but got %T", o)
	}
	// For now, we assume all ports are open (proper tracking would require port state)
	mc.SetValue(values.TrueValue)
	return nil
}

// PrimClosePort implements the (close-port) primitive.
// Closes an input or output port.
func PrimClosePort(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch p := o.(type) {
	case *values.CharacterInputPort:
		if closer, ok := p.Value.(io.Closer); ok {
			err := closer.Close()
			if err != nil {
				return values.WrapForeignErrorf(err, "close-port: %v", err)
			}
		}
	case *values.CharacterOutputPort:
		if closer, ok := p.Value.(io.Closer); ok {
			err := closer.Close()
			if err != nil {
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
			err := closer.Close()
			if err != nil {
				return values.WrapForeignErrorf(err, "close-port: %v", err)
			}
		}
	case *values.BinaryOutputPort:
		if closer, ok := p.Value.(io.Closer); ok {
			err := closer.Close()
			if err != nil {
				return values.WrapForeignErrorf(err, "close-port: %v", err)
			}
		}
	default:
		return values.WrapForeignErrorf(values.ErrNotAnInputPort, "close-port: expected a port but got %T", o)
	}
	mc.SetValues()
	return nil
}

// PrimEofObject implements the (eof-object) primitive.
// Returns the EOF object.
func PrimEofObject(_ context.Context, mc *machine.MachineContext) error {
	mc.SetValue(values.EofObject)
	return nil
}

// PrimEofObjectQ implements the (eof-object?) primitive.
// Returns #t if the argument is the EOF object.
func PrimEofObjectQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	mc.SetValue(utils.BoolToBoolean(o == values.EofObject))
	return nil
}

// PrimOpenInputString implements the Scheme open-input-string primitive.
func PrimOpenInputString(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	s, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "open-input-string: expected a string but got %T", o)
	}
	mc.SetValue(values.NewStringInputPort(s.Value))
	return nil
}

// PrimOpenOutputString implements the Scheme open-output-string primitive.
func PrimOpenOutputString(_ context.Context, mc *machine.MachineContext) error {
	mc.SetValue(values.NewStringOutputPort())
	return nil
}

// PrimGetOutputString implements the Scheme get-output-string primitive.
func PrimGetOutputString(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	p, ok := o.(*values.StringOutputPort)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAStringOutputPort, "get-output-string: expected a string output port but got %T", o)
	}
	mc.SetValue(&values.String{Value: p.GetString()})
	return nil
}

// PrimOpenInputBytevector implements the Scheme open-input-bytevector primitive.
func PrimOpenInputBytevector(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	bv, ok := o.(*values.ByteVector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAByteVector, "open-input-bytevector: expected a bytevector but got %T", o)
	}
	// Convert []Byte to []byte
	data := make([]byte, len(*bv))
	for i, b := range *bv {
		data[i] = b.Value
	}
	mc.SetValue(values.NewBytevectorInputPort(data))
	return nil
}

// PrimOpenOutputBytevector implements the Scheme open-output-bytevector primitive.
func PrimOpenOutputBytevector(_ context.Context, mc *machine.MachineContext) error {
	mc.SetValue(values.NewBytevectorOutputPort())
	return nil
}

// PrimGetOutputBytevector implements the Scheme get-output-bytevector primitive.
func PrimGetOutputBytevector(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
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
