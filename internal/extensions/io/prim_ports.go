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
	"bytes"
	"context"
	"errors"

	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
)

// PrimPortQ implements the port? primitive.
// Returns #t if the argument is a port (input or output), #f otherwise.
//
// R7RS §6.13.1: Returns #t if obj is a port, otherwise returns #f.
func PrimPortQ(_ context.Context, mc *machine.MachineContext) error {
	_, ok := mc.Arg(0).(values.Port)
	mc.SetValue(schemeutil.BoolToBoolean(ok))
	return nil
}

// PrimInputPortQ implements the (input-port?) primitive.
// Returns #t if argument is input port.
//
// R7RS §6.13.1: Returns #t if obj is an input port, otherwise returns #f.
func PrimInputPortQ(_ context.Context, mc *machine.MachineContext) error {
	_, ok := mc.Arg(0).(values.InputPort)
	mc.SetValue(schemeutil.BoolToBoolean(ok))
	return nil
}

// PrimOutputPortQ implements the output-port? primitive.
// Returns #t if the argument is an output port, #f otherwise.
//
// R7RS §6.13.1: Returns #t if obj is an output port, otherwise returns #f.
func PrimOutputPortQ(_ context.Context, mc *machine.MachineContext) error {
	_, ok := mc.Arg(0).(values.OutputPort)
	mc.SetValue(schemeutil.BoolToBoolean(ok))
	return nil
}

// PrimInputPortOpenQ implements the (input-port-open?) primitive.
// Returns #t if input port is open.
//
// R7RS §6.13.1: Returns #t if port is still open and capable of performing input.
func PrimInputPortOpenQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	p, ok := o.(values.InputPort)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInputPort, "input-port-open?: expected an input port but got %T", o)
	}
	mc.SetValue(schemeutil.BoolToBoolean(!p.IsClosed()))
	return nil
}

// PrimOutputPortOpenQ implements the output-port-open? primitive.
// Returns #t if the output port is open, #f otherwise.
//
// R7RS §6.13.1: Returns #t if port is still open and capable of performing output.
func PrimOutputPortOpenQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	p, ok := o.(values.OutputPort)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnOutputPort, "output-port-open?: expected an output port but got %T", o)
	}
	mc.SetValue(schemeutil.BoolToBoolean(!p.IsClosed()))
	return nil
}

// PrimClosePort implements the (close-port) primitive.
// Closes an input or output port.
//
// R7RS §6.13.1: Closes the resource associated with port.
func PrimClosePort(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	p, ok := o.(values.Port)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInputPort, "close-port: expected a port but got %T", o)
	}
	err := p.Close()
	if err != nil {
		return values.WrapForeignErrorf(err, "close-port: %v", err)
	}
	mc.SetValues()
	return nil
}

// PrimEofObject implements the (eof-object) primitive.
// Returns the EOF object.
func PrimEofObject(_ context.Context, mc *machine.MachineContext) error {
	mc.SetValue(values.EOFObject)
	return nil
}

// PrimEofObjectQ implements the (eof-object?) primitive.
// Returns #t if the argument is the EOF object.
func PrimEofObjectQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	mc.SetValue(schemeutil.BoolToBoolean(o == values.EOFObject))
	return nil
}

// PrimOpenInputString implements the Scheme open-input-string primitive.
func PrimOpenInputString(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	s, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "open-input-string: expected a string but got %T", o)
	}
	mc.SetValue(values.NewStringInputPortWithBuffer(bytes.NewBufferString(s.Value)))
	return nil
}

// PrimOpenOutputString implements the Scheme open-output-string primitive.
func PrimOpenOutputString(_ context.Context, mc *machine.MachineContext) error {
	mc.SetValue(values.NewStringOutputPortWithBuffer(&bytes.Buffer{}))
	return nil
}

// PrimGetOutputString implements the Scheme get-output-string primitive.
func PrimGetOutputString(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	p, ok := o.(*values.StringOutputPort)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAStringOutputPort, "get-output-string: expected a string output port but got %T", o)
	}
	mc.SetValue(&values.String{Value: p.String()})
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
	mc.SetValue(values.NewByteVectorInputPortFromReader(bytes.NewReader(data)))
	return nil
}

// PrimOpenOutputBytevector implements the Scheme open-output-bytevector primitive.
func PrimOpenOutputBytevector(_ context.Context, mc *machine.MachineContext) error {
	tup, ok := mc.Arg(0).(values.Tuple)
	if ok && tup.Length() > 1 {
		return values.WrapForeignErrorf(values.ErrInvalidArgument, "open-output-bytevector: expected one or zero arguments but got %d", tup.Length())
	}
	if tup.Length() == 0 {
		mc.SetValue(values.NewByteVectorBufferdOutputPort())
		return nil
	}
	bvec, ok := tup.Car().(values.OutputPort)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnOutputPort, "open-output-bytevector: expected an output port but got %T", tup.Car())
	}
	mc.SetValue(bvec)
	return nil
}

// PrimGetOutputBytevector implements the Scheme get-output-bytevector primitive.
func PrimGetOutputBytevector(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	e, ok := o.(values.ByteVectorExtractor)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotABytevectorOutputPort, "get-output-bytevector: expected a bytevector output port but got %T", o)
	}
	q, err := e.ReadByteVector()
	if err != nil {
		return values.WrapForeignErrorf(err, "get-output-bytevector: %v", err)
	}
	mc.SetValue(q)
	return nil
}

// PrimTextualPortQ implements the textual-port? primitive.
// R7RS §6.13.1: Returns #t if the port is a textual port, #f otherwise.
func PrimTextualPortQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, isReader := o.(values.TextualReader)
	_, isWriter := o.(values.TextualWriter)
	mc.SetValue(schemeutil.BoolToBoolean(isReader || isWriter))
	return nil
}

// PrimBinaryPortQ implements the binary-port? primitive.
// R7RS §6.13.1: Returns #t if the port is a binary port, #f otherwise.
func PrimBinaryPortQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, isReader := o.(values.BinaryReader)
	_, isWriter := o.(values.BinaryWriter)
	mc.SetValue(schemeutil.BoolToBoolean(isReader || isWriter))
	return nil
}

// PrimCallWithPort implements the call-with-port primitive.
// R7RS §6.13.1: (call-with-port port proc)
// Calls proc with port as an argument. When proc returns, the port is closed.
func PrimCallWithPort(_ context.Context, mc *machine.MachineContext) error {
	portArg := mc.Arg(0)
	proc := mc.Arg(1)

	// Validate that proc is a procedure
	mcls, ok := proc.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "call-with-port: expected a procedure but got %T", proc)
	}

	// Call the procedure with the port
	sub := mc.NewSubContext()
	_, err := sub.Apply(mcls, portArg)
	if err != nil {
		return err
	}
	runErr := sub.Run()

	// Close the port after proc returns (even if there was an error)
	closePort(portArg)

	// Handle any errors from running the procedure
	if runErr != nil {
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(runErr, &escapeErr) {
			return runErr
		}
		if !errors.Is(runErr, machine.ErrMachineHalt) {
			return runErr
		}
	}

	// Return the result of the procedure
	mc.SetValues(sub.GetValues()...)
	return nil
}

// closePort closes a port regardless of its type.
func closePort(o values.Value) {
	p, ok := o.(values.Port)
	if ok {
		p.Close() //nolint:errcheck
	}
}
