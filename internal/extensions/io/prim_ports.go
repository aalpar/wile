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

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// Port type predicates — R7RS §6.13.1.
var PrimPortQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(values.Port)
	return ok
})
var PrimInputPortQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(values.InputPort)
	return ok
})
var PrimOutputPortQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(values.OutputPort)
	return ok
})

// PrimInputPortOpenQ implements the (input-port-open?) primitive.
// Returns #t if input port is open.
//
// R7RS §6.13.1: Returns #t if port is still open and capable of performing input.
func PrimInputPortOpenQ(mc *machine.MachineContext) error {
	p, err := helpers.RequireArg[values.InputPort](mc, 0, werr.ErrNotAnInputPort, "input-port-open?")
	if err != nil {
		return err
	}
	mc.SetValue(values.BoolToBoolean(!p.IsClosed()))
	return nil
}

// PrimOutputPortOpenQ implements the output-port-open? primitive.
// Returns #t if the output port is open, #f otherwise.
//
// R7RS §6.13.1: Returns #t if port is still open and capable of performing output.
func PrimOutputPortOpenQ(mc *machine.MachineContext) error {
	p, err := helpers.RequireArg[values.OutputPort](mc, 0, werr.ErrNotAnOutputPort, "output-port-open?")
	if err != nil {
		return err
	}
	mc.SetValue(values.BoolToBoolean(!p.IsClosed()))
	return nil
}

// PrimClosePort implements the (close-port) primitive.
// Closes an input or output port.
//
// R7RS §6.13.1: Closes the resource associated with port.
func PrimClosePort(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(values.Port)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAnInputPort, "close-port: expected a port but got %T", o)
	}
	err := closePort(o)
	if err != nil {
		return werr.WrapForeignErrorf(err, "close-port: %v", err)
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimCloseInputPort implements the (close-input-port) primitive.
// Requires an input port; errors if given an output port.
//
// R7RS §6.13.1: close-input-port takes an input port.
func PrimCloseInputPort(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(values.InputPort)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAnInputPort, "close-input-port: expected an input port but got %T", o)
	}
	err := closePort(o)
	if err != nil {
		return werr.WrapForeignErrorf(err, "close-input-port: %v", err)
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimCloseOutputPort implements the (close-output-port) primitive.
// Requires an output port; errors if given an input port.
// Flushes buffered data before closing.
//
// R7RS §6.13.1: close-output-port takes an output port.
func PrimCloseOutputPort(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	p, ok := o.(values.OutputPort)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAnOutputPort, "close-output-port: expected an output port but got %T", o)
	}
	flushErr := p.Flush()
	if flushErr != nil {
		return werr.WrapForeignErrorf(flushErr, "close-output-port: flush failed")
	}
	err := closePort(o)
	if err != nil {
		return werr.WrapForeignErrorf(err, "close-output-port: %v", err)
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimEofObject implements the (eof-object) primitive.
// Returns the EOF object.
func PrimEofObject(mc *machine.MachineContext) error {
	mc.SetValue(values.EOFObject)
	return nil
}

// PrimEofObjectQ implements the (eof-object?) primitive.
// Returns #t if the argument is the EOF object.
func PrimEofObjectQ(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	mc.SetValue(values.BoolToBoolean(o == values.EOFObject))
	return nil
}

// PrimOpenInputString implements the Scheme open-input-string primitive.
func PrimOpenInputString(mc *machine.MachineContext) error {
	s, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "open-input-string")
	if err != nil {
		return err
	}
	mc.SetValue(values.NewStringInputPortWithBuffer(bytes.NewBufferString(s.Value)))
	return nil
}

// PrimOpenOutputString implements the Scheme open-output-string primitive.
func PrimOpenOutputString(mc *machine.MachineContext) error {
	mc.SetValue(values.NewStringOutputPortWithBuffer(&bytes.Buffer{}))
	return nil
}

// PrimGetOutputString implements the Scheme get-output-string primitive.
func PrimGetOutputString(mc *machine.MachineContext) error {
	p, err := helpers.RequireArg[*values.StringOutputPort](mc, 0, werr.ErrNotAStringOutputPort, "get-output-string")
	if err != nil {
		return err
	}
	// R7RS §6.13.3: get-output-string returns a newly allocated mutable string.
	mc.SetValue(values.NewMutableString(p.String()))
	return nil
}

// PrimOpenInputBytevector implements the Scheme open-input-bytevector primitive.
func PrimOpenInputBytevector(mc *machine.MachineContext) error {
	bv, err := helpers.RequireArg[*values.ByteVector](mc, 0, werr.ErrNotAByteVector, "open-input-bytevector")
	if err != nil {
		return err
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
func PrimOpenOutputBytevector(mc *machine.MachineContext) error {
	tup, ok := mc.Arg(0).(values.Tuple)
	if ok && tup.Length() > 1 {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "open-output-bytevector: expected one or zero arguments but got %d", tup.Length())
	}
	if tup.Length() == 0 {
		mc.SetValue(values.NewByteVectorBufferedOutputPort())
		return nil
	}
	bvec, ok := tup.Car().(values.OutputPort)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAnOutputPort, "open-output-bytevector: expected an output port but got %T", tup.Car())
	}
	mc.SetValue(bvec)
	return nil
}

// PrimGetOutputBytevector implements the Scheme get-output-bytevector primitive.
func PrimGetOutputBytevector(mc *machine.MachineContext) error {
	e, err := helpers.RequireArg[values.ByteVectorExtractor](mc, 0, werr.ErrNotABytevectorOutputPort, "get-output-bytevector")
	if err != nil {
		return err
	}
	q, err := e.ReadByteVector()
	if err != nil {
		return werr.WrapForeignErrorf(err, "get-output-bytevector: %v", err)
	}
	mc.SetValue(q)
	return nil
}

// PrimTextualPortQ implements the textual-port? primitive.
// R7RS §6.13.1: Returns #t if the port is a textual port, #f otherwise.
func PrimTextualPortQ(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, isReader := o.(values.TextualReader)
	_, isWriter := o.(values.TextualWriter)
	mc.SetValue(values.BoolToBoolean(isReader || isWriter))
	return nil
}

// PrimBinaryPortQ implements the binary-port? primitive.
// R7RS §6.13.1: Returns #t if the port is a binary port, #f otherwise.
func PrimBinaryPortQ(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, isReader := o.(values.BinaryReader)
	_, isWriter := o.(values.BinaryWriter)
	mc.SetValue(values.BoolToBoolean(isReader || isWriter))
	return nil
}

// closePort closes a port regardless of its type and evicts
// any cached tokenizer/parser for the port.

// evictPortCache removes any cached tokenizer and parser for the given port.
// This is the single eviction choke point — all port cleanup paths
// (explicit close, EOF, call-with-port) must go through here.
func evictPortCache(port values.Value) {
	cacheMu.Lock()
	delete(Tokenizers, port)
	delete(Parsers, port)
	cacheMu.Unlock()
}

func closePort(o values.Value) error {
	var err error
	p, ok := o.(values.Port)
	if ok {
		err = p.Close()
	}
	evictPortCache(o)
	return err
}
