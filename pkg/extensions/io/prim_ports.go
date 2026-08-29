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

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// Port type predicates — R7RS §6.13.1.
//
// All ports are *values.PortObject after Phase 2; capability checks
// inspect slot presence via the AsX accessors instead of asserting
// against narrow Go interfaces.

// PrimPortQ R7RS §6.13.1 port? predicate.
var PrimPortQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(values.Port)
	return ok
})

// PrimInputPortQ R7RS §6.13.1 input-port? predicate.
var PrimInputPortQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	p, ok := o.(*values.PortObject)
	if !ok {
		return false
	}
	_, hasReader := p.AsReader()
	return hasReader
})

// PrimOutputPortQ R7RS §6.13.1 output-port? predicate.
var PrimOutputPortQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	p, ok := o.(*values.PortObject)
	if !ok {
		return false
	}
	_, hasWriter := p.AsWriter()
	return hasWriter
})

// PrimInputPortOpenQ implements the (input-port-open?) primitive.
// Returns #t if input port is open.
//
// R7RS §6.13.1: Returns #t if port is still open and capable of performing input.
func PrimInputPortOpenQ(mc machine.CallContext) error {
	p, err := helpers.RequireArg[*values.PortObject](mc, 0, werr.ErrNotAnInputPort, "input-port-open?")
	if err != nil {
		return err
	}
	_, ok := p.AsReader()
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAnInputPort,
			"input-port-open?: not an input port")
	}
	mc.SetValue(values.BoolToBoolean(!p.IsClosed()))
	return nil
}

// PrimOutputPortOpenQ implements the output-port-open? primitive.
// Returns #t if the output port is open, #f otherwise.
//
// R7RS §6.13.1: Returns #t if port is still open and capable of performing output.
func PrimOutputPortOpenQ(mc machine.CallContext) error {
	p, err := helpers.RequireArg[*values.PortObject](mc, 0, werr.ErrNotAnOutputPort, "output-port-open?")
	if err != nil {
		return err
	}
	_, ok := p.AsWriter()
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAnOutputPort,
			"output-port-open?: not an output port")
	}
	mc.SetValue(values.BoolToBoolean(!p.IsClosed()))
	return nil
}

// PrimClosePort implements the (close-port) primitive.
// Closes an input or output port.
//
// R7RS §6.13.1: Closes the resource associated with port.
func PrimClosePort(mc machine.CallContext) error {
	o := mc.Arg(0)
	_, ok := o.(values.Port)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAPort, "close-port: expected a port but got %T", o)
	}
	st, err := stateFrom(mc)
	if err != nil {
		return err
	}
	err = closePort(st, o)
	if err != nil {
		return werr.WrapForeignErrorf(err, "close-port: %v", err)
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimCloseInputPort implements the (close-input-port) primitive.
// Requires an input port; errors if given an output-only port.
//
// R7RS §6.13.1: close-input-port takes an input port.
func PrimCloseInputPort(mc machine.CallContext) error {
	o := mc.Arg(0)
	p, ok := o.(*values.PortObject)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAnInputPort, "close-input-port: expected an input port but got %T", o)
	}
	_, hasReader := p.AsReader()
	if !hasReader {
		return werr.WrapForeignErrorf(werr.ErrNotAnInputPort,
			"close-input-port: not an input port")
	}
	st, err := stateFrom(mc)
	if err != nil {
		return err
	}
	err = closePort(st, o)
	if err != nil {
		return werr.WrapForeignErrorf(err, "close-input-port: %v", err)
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimCloseOutputPort implements the (close-output-port) primitive.
// Requires an output port; errors if given an input-only port.
// Flushes buffered data before closing.
//
// R7RS §6.13.1: close-output-port takes an output port.
func PrimCloseOutputPort(mc machine.CallContext) error {
	o := mc.Arg(0)
	p, ok := o.(*values.PortObject)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAnOutputPort, "close-output-port: expected an output port but got %T", o)
	}
	_, hasWriter := p.AsWriter()
	if !hasWriter {
		return werr.WrapForeignErrorf(werr.ErrNotAnOutputPort,
			"close-output-port: not an output port")
	}
	flsh, hasFlsh := p.AsFlusher()
	if hasFlsh {
		flushErr := flsh.Flush()
		if flushErr != nil {
			return werr.WrapForeignErrorf(flushErr, "close-output-port: flush failed")
		}
	}
	st, err := stateFrom(mc)
	if err != nil {
		return err
	}
	err = closePort(st, o)
	if err != nil {
		return werr.WrapForeignErrorf(err, "close-output-port: %v", err)
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimEofObject implements the (eof-object) primitive.
// Returns the EOF object.
func PrimEofObject(mc machine.CallContext) error {
	mc.SetValue(values.EOFObject)
	return nil
}

// PrimEofObjectQ implements the (eof-object?) primitive.
// Returns #t if the argument is the EOF object.
func PrimEofObjectQ(mc machine.CallContext) error {
	o := mc.Arg(0)
	mc.SetValue(values.BoolToBoolean(o == values.EOFObject))
	return nil
}

// PrimOpenInputString implements the Scheme open-input-string primitive.
func PrimOpenInputString(mc machine.CallContext) error {
	s, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "open-input-string")
	if err != nil {
		return err
	}
	mc.SetValue(values.NewStringInputPortWithBuffer(bytes.NewBufferString(s.Value)))
	return nil
}

// PrimOpenOutputString implements the Scheme open-output-string primitive.
func PrimOpenOutputString(mc machine.CallContext) error {
	mc.SetValue(values.NewStringOutputPortWithBuffer(&bytes.Buffer{}))
	return nil
}

// PrimGetOutputString implements the Scheme get-output-string primitive.
//
// Two-step extraction: first verify the value is a *PortObject (any
// port), then ask for string-extractor capability via StringContent.
// This produces distinct errors for "not a port at all"
// (ErrNotAPort) vs "wrong port flavor" (ErrNotAStringOutputPort).
func PrimGetOutputString(mc machine.CallContext) error {
	p, err := helpers.RequireArg[*values.PortObject](mc, 0, werr.ErrNotAPort, "get-output-string")
	if err != nil {
		return err
	}
	s, ok := p.StringContent()
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAStringOutputPort,
			"get-output-string: port is not a string output port")
	}
	// R7RS §6.13.3: get-output-string returns a newly allocated mutable string.
	mc.SetValue(values.NewMutableString(s))
	return nil
}

// PrimOpenInputBytevector implements the Scheme open-input-bytevector primitive.
func PrimOpenInputBytevector(mc machine.CallContext) error {
	bv, err := helpers.RequireArg[*values.ByteVector](mc, 0, werr.ErrNotAByteVector, "open-input-bytevector")
	if err != nil {
		return err
	}
	data := bv.AsBytes()
	mc.SetValue(values.NewByteVectorInputPortFromReader(bytes.NewReader(data)))
	return nil
}

// PrimOpenOutputBytevector implements the Scheme open-output-bytevector
// primitive. Per R7RS §6.13.2 it takes no arguments and returns a fresh
// binary output port that accumulates bytes for retrieval by
// get-output-bytevector.
func PrimOpenOutputBytevector(mc machine.CallContext) error {
	mc.SetValue(values.NewByteVectorBufferedOutputPort())
	return nil
}

// PrimGetOutputBytevector implements the Scheme get-output-bytevector primitive.
//
// Two-step extraction symmetric to PrimGetOutputString: first verify
// the value is a *PortObject (any port), then ask for the
// bytevector-extractor capability via AsByteVectorExtractor. This
// produces distinct errors for "not a port at all" vs "wrong port
// flavor."
func PrimGetOutputBytevector(mc machine.CallContext) error {
	p, err := helpers.RequireArg[*values.PortObject](mc, 0, werr.ErrNotAPort, "get-output-bytevector")
	if err != nil {
		return err
	}
	e, ok := p.AsByteVectorExtractor()
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotABytevectorOutputPort,
			"get-output-bytevector: port is not a bytevector output port")
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
var PrimTextualPortQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	p, ok := o.(*values.PortObject)
	if !ok {
		return false
	}
	_, hasRR := p.AsRuneReader()
	_, hasRW := p.AsRuneWriter()
	return hasRR || hasRW
})

// PrimBinaryPortQ implements the binary-port? primitive.
// R7RS §6.13.1: Returns #t if the port is a binary port, #f otherwise.
var PrimBinaryPortQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	p, ok := o.(*values.PortObject)
	if !ok {
		return false
	}
	_, hasBR := p.AsByteReader()
	_, hasBW := p.AsByteWriter()
	return hasBR || hasBW
})

// evictPortCache removes any cached tokenizer and parser for the given port.
// This is the single eviction choke point — all port cleanup paths
// (explicit close, EOF, call-with-port) must go through here.
func evictPortCache(st *State, port values.Value) {
	st.mu.Lock()
	delete(st.tokenizers, port)
	delete(st.parsers, port)
	st.mu.Unlock()
}

// closePort closes a port regardless of its type and evicts
// any cached tokenizer/parser for the port.
func closePort(st *State, o values.Value) error {
	var err error
	p, ok := o.(values.Port)
	if ok {
		err = p.Close()
	}
	evictPortCache(st, o)
	return err
}
