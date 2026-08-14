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
	"errors"
	"io"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

const (
	// MaxReadBytevectorBytes is the maximum size of bytevector that
	// read-bytevector can allocate (100 MB).
	MaxReadBytevectorBytes = 100 * 1024 * 1024 // 100 MB
)

// PrimReadU8 implements the read-u8 primitive.
// R7RS §6.13.3: (read-u8 [port])
// Reads the next byte from the given binary input port and returns it as an exact integer.
// Returns eof-object at end of file.
func PrimReadU8(mc machine.CallContext) error {
	p, _, err := getRequiredBinaryInputPort(mc.Arg(0), "read-u8")
	if err != nil {
		return err
	}
	br, _ := p.AsByteReader()
	b, err := br.ReadByte()
	if errors.Is(err, io.EOF) {
		mc.SetValue(values.EOFObject)
		return nil
	}
	if err != nil {
		return werr.WrapForeignReadErrorf(err, "read-u8: error reading byte")
	}
	mc.SetValue(values.NewInteger(int64(b)))

	return nil
}

// PrimPeekU8 implements the peek-u8 primitive.
// R7RS §6.13.3: (peek-u8 [port])
// Like read-u8, but does not consume the byte from the port.
func PrimPeekU8(mc machine.CallContext) error {
	p, _, err := getRequiredBinaryInputPort(mc.Arg(0), "peek-u8")
	if err != nil {
		return err
	}
	br, _ := p.AsByteReader()
	b, err := br.ReadByte()
	if errors.Is(err, io.EOF) {
		mc.SetValue(values.EOFObject)
		return nil
	}
	if err != nil {
		return werr.WrapForeignReadErrorf(err, "peek-u8: error reading byte")
	}
	// Unread the byte so it can be read again. Every binary input port
	// has a ByteUnreader slot — port_constructors.go pairs rb/urb.
	urb, _ := p.AsByteUnreader()
	err = urb.UnreadByte()
	if err != nil {
		return werr.WrapForeignErrorf(err, "peek-u8: error unreading byte")
	}
	mc.SetValue(values.NewInteger(int64(b)))

	return nil
}

// PrimU8ReadyQ implements the u8-ready? primitive.
// R7RS §6.13.3: (u8-ready? [port])
// Returns #t if a byte is available for reading from the binary input port.
func PrimU8ReadyQ(mc machine.CallContext) error {
	// For now, assume a byte is always ready for bytevector input ports
	// A more accurate implementation would need non-blocking I/O
	mc.SetValue(values.TrueValue)
	return nil
}

// PrimWriteU8 implements the write-u8 primitive.
// R7RS §6.13.3: (write-u8 byte [port])
// Writes byte to the given binary output port and returns an unspecified value.
func PrimWriteU8(mc machine.CallContext) error {
	byteVal := mc.Arg(0)

	// Validate byte argument (must be exact integer 0-255)
	var b byte
	switch v := byteVal.(type) {
	case *values.Integer:
		err := values.ValidateByteValue(v, "write-u8", "byte")
		if err != nil {
			return err
		}
		b = byte(v.Value)
	default:
		return werr.WrapForeignErrorf(werr.ErrNotAnInteger, "write-u8: expected an exact integer but got %T", byteVal)
	}

	p, _, err := getRequiredBinaryOutputPort(mc.Arg(1), "write-u8")
	if err != nil {
		return err
	}
	bw, _ := p.AsByteWriter()
	err = bw.WriteByte(b)
	if err != nil {
		return werr.WrapForeignErrorf(err, "write-u8: error writing byte")
	}

	mc.SetValue(values.Void)
	return nil
}

// PrimReadBytevector implements the read-bytevector primitive.
// R7RS §6.13.3: (read-bytevector k [port])
// Reads the next k bytes from port into a newly allocated bytevector.
// Returns eof-object if no bytes are available before end of file.
func PrimReadBytevector(mc machine.CallContext) error {
	k, err := helpers.RequireArg[*values.Integer](mc, 0, werr.ErrNotAnInteger, "read-bytevector")
	if err != nil {
		return err
	}
	if k.Value < 0 {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "read-bytevector: k must be non-negative")
	}

	// Check allocation limit
	if k.Value > MaxReadBytevectorBytes {
		return werr.WrapForeignErrorf(werr.ErrAllocationLimitExceeded,
			"read-bytevector: requested allocation (%d bytes, %d MB) exceeds maximum (%d MB)",
			k.Value,
			k.Value/(1024*1024),
			MaxReadBytevectorBytes/(1024*1024),
		)
	}

	p, _, err := getRequiredBinaryInputPort(mc.Arg(1), "read-bytevector")
	if err != nil {
		return err
	}

	// Read exactly k bytes using io.ReadFull to avoid short reads from buffered ports.
	// R7RS §6.13.3: read-bytevector returns a bytevector of exactly k bytes if available.
	buf := make([]byte, k.Value)
	r, _ := p.AsReader()
	n, err := io.ReadFull(r, buf)
	switch {
	case err == nil:
		// Full read: proceed with buf as-is.
	case errors.Is(err, io.ErrUnexpectedEOF):
		// Partial read at EOF: return the bytes we got.
		buf = buf[:n]
	case errors.Is(err, io.EOF):
		// No bytes at all: return eof-object.
		mc.SetValue(values.EOFObject)
		return nil
	default:
		return werr.WrapForeignReadErrorf(err, "read-bytevector: error reading from port")
	}

	mc.SetValue(values.NewByteVectorFromBytes(buf...))
	return nil
}

// PrimReadBytevectorBang implements the read-bytevector! primitive.
// R7RS §6.13.3: (read-bytevector! bytevector [port [start [end]]])
// Reads bytes from port into an existing bytevector.
// Returns the number of bytes read, or eof-object if no bytes available.
func PrimReadBytevectorBang(mc machine.CallContext) error {
	bv, err := helpers.RequireArg[*values.ByteVector](mc, 0, werr.ErrNotAByteVector, "read-bytevector!")
	if err != nil {
		return err
	}
	if mc.ImmutableLiterals().IsImmutable(bv) {
		return werr.WrapForeignErrorf(werr.ErrImmutableBytevector,
			"read-bytevector!: cannot mutate immutable literal bytevector")
	}

	p, tuple, err := getRequiredBinaryInputPort(mc.Arg(1), "read-bytevector!")
	if err != nil {
		return err
	}

	// Extract optional start/end arguments
	start, end, err := helpers.ParseSubrange(tuple.Cdr(), bv.Length(), "read-bytevector!")
	if err != nil {
		return err
	}

	// Read exactly (end-start) bytes using io.ReadFull to avoid short reads from buffered ports.
	// R7RS §6.13.3: read-bytevector! fills the range [start,end) if bytes are available.
	buf := make([]byte, end-start)
	r, _ := p.AsReader()
	n, err := io.ReadFull(r, buf)
	if err != nil {
		if errors.Is(err, io.ErrUnexpectedEOF) {
			// Partial read at EOF: copy what we got and return the count.
			slots := bv.Elems()
			for i := range n {
				slots[start+i] = values.NewByte(buf[i])
			}
			mc.SetValue(values.NewInteger(int64(n)))
			return nil
		}
		if errors.Is(err, io.EOF) {
			mc.SetValue(values.EOFObject)
			return nil
		}
		return werr.WrapForeignReadErrorf(err, "read-bytevector!: error reading from port")
	}

	slots := bv.Elems()
	for i := range n {
		slots[start+i] = values.NewByte(buf[i])
	}
	mc.SetValue(values.NewInteger(int64(n)))
	return nil
}

// PrimWriteBytevector implements the write-bytevector primitive.
// R7RS §6.13.3: (write-bytevector bytevector [port [start [end]]])
// Writes the bytes of bytevector to port.
func PrimWriteBytevector(mc machine.CallContext) error {
	bv, err := helpers.RequireArg[*values.ByteVector](mc, 0, werr.ErrNotAByteVector, "write-bytevector")
	if err != nil {
		return err
	}

	p, tuple, err := getRequiredBinaryOutputPort(mc.Arg(1), "write-bytevector")
	if err != nil {
		return err
	}

	// Extract optional start/end arguments
	start, end, err := helpers.ParseSubrange(tuple.Cdr(), bv.Length(), "write-bytevector")
	if err != nil {
		return err
	}

	data := bv.AsBytes(start, end)
	w, _ := p.AsWriter()
	_, err = w.Write(data)
	if err != nil {
		return werr.WrapForeignErrorf(err, "write-bytevector: error writing to port")
	}

	mc.SetValue(values.Void)
	return nil
}
