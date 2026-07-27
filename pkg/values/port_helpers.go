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

package values

import (
	"errors"
	"io"
)

// Narrow interfaces for backing-type methods not in the stdlib.
// Exported because *PortObject's capability accessors expose them as
// return types (AsRuneWriter, AsByteUnreader, AsRuneUnreader, AsFlusher).

// ByteUnreader is the interface satisfied by readers that can unread the
// last byte. Mirrors io.ByteScanner's UnreadByte half.
type ByteUnreader interface {
	UnreadByte() error
}

// RuneUnreader is the interface satisfied by readers that can unread the
// last rune. Mirrors io.RuneScanner's UnreadRune half.
type RuneUnreader interface {
	UnreadRune() error
}

// RuneWriter is the interface satisfied by writers that can write a rune.
// The stdlib has no equivalent; bufio.Writer and bytes.Buffer satisfy it.
type RuneWriter interface {
	WriteRune(rune) (int, error)
}

// Flusher is the interface satisfied by buffered writers that can flush
// pending bytes to the underlying stream.
type Flusher interface {
	Flush() error
}

// Guard-and-delegate helpers. Each guards on portBase.closed,
// then delegates to the backing type via the narrowest interface.

func guardedRead(b *portBase, r io.Reader, bs []byte) (int, error) {
	err := b.guardClosed()
	if err != nil {
		return 0, err
	}
	return r.Read(bs)
}

func guardedReadByte(b *portBase, r io.ByteReader) (byte, error) {
	err := b.guardClosed()
	if err != nil {
		return 0, err
	}
	return r.ReadByte()
}

func guardedUnreadByte(b *portBase, r ByteUnreader) error {
	err := b.guardClosed()
	if err != nil {
		return err
	}
	return r.UnreadByte()
}

func guardedReadRune(b *portBase, r io.RuneReader) (rune, int, error) {
	err := b.guardClosed()
	if err != nil {
		return 0, 0, err
	}
	return r.ReadRune()
}

func guardedUnreadRune(b *portBase, r RuneUnreader) error {
	err := b.guardClosed()
	if err != nil {
		return err
	}
	return r.UnreadRune()
}

func guardedWrite(b *portBase, w io.Writer, bs []byte) (int, error) {
	err := b.guardClosed()
	if err != nil {
		return 0, err
	}
	return w.Write(bs)
}

func guardedWriteByte(b *portBase, w io.ByteWriter, c byte) error {
	err := b.guardClosed()
	if err != nil {
		return err
	}
	return w.WriteByte(c)
}

func guardedWriteString(b *portBase, w io.StringWriter, s string) (int, error) {
	err := b.guardClosed()
	if err != nil {
		return 0, err
	}
	return w.WriteString(s)
}

func guardedWriteRune(b *portBase, w RuneWriter, rn rune) (int, error) {
	err := b.guardClosed()
	if err != nil {
		return 0, err
	}
	return w.WriteRune(rn)
}

func guardedFlush(b *portBase, f Flusher) error {
	err := b.guardClosed()
	if err != nil {
		return err
	}
	return f.Flush()
}

// flushThenClose flushes buffered data then closes the port.
// If both flush and close fail, both errors are preserved via errors.Join.
//
// Called from (*PortObject).Close on the port's guardedFlusher slot (the
// flush is done while the port is still open, so the wrapper's closed-guard
// passes). Subsequent Close calls short-circuit at the *PortObject level;
// this function does not need idempotence of its own.
func flushThenClose(f Flusher, b *portBase) error {
	flushErr := f.Flush()
	closeErr := b.Close()
	if closeErr != nil {
		return errors.Join(closeErr, flushErr)
	}
	return flushErr
}

// Slot wrappers — each slot of *PortObject stores a small struct that
// guards on portBase.closed before delegating to the raw underlying
// stdlib type. Constructors allocate the wrappers once; accessors
// return the slot directly. This preserves the R7RS port-closed
// rejection semantics that the deleted concrete port types provided
// via methods like (*BinaryInputPort).ReadByte.

type guardedReader struct {
	base *portBase
	raw  io.Reader
}

func (g guardedReader) Read(b []byte) (int, error) {
	return guardedRead(g.base, g.raw, b)
}

type guardedByteReader struct {
	base *portBase
	raw  io.ByteReader
}

func (g guardedByteReader) ReadByte() (byte, error) {
	return guardedReadByte(g.base, g.raw)
}

type guardedRuneReader struct {
	base *portBase
	raw  io.RuneReader
}

func (g guardedRuneReader) ReadRune() (rune, int, error) {
	return guardedReadRune(g.base, g.raw)
}

type guardedByteUnreader struct {
	base *portBase
	raw  ByteUnreader
}

func (g guardedByteUnreader) UnreadByte() error {
	return guardedUnreadByte(g.base, g.raw)
}

type guardedRuneUnreader struct {
	base *portBase
	raw  RuneUnreader
}

func (g guardedRuneUnreader) UnreadRune() error {
	return guardedUnreadRune(g.base, g.raw)
}

type guardedWriter struct {
	base *portBase
	raw  io.Writer
}

func (g guardedWriter) Write(b []byte) (int, error) {
	return guardedWrite(g.base, g.raw, b)
}

type guardedByteWriter struct {
	base *portBase
	raw  io.ByteWriter
}

func (g guardedByteWriter) WriteByte(c byte) error {
	return guardedWriteByte(g.base, g.raw, c)
}

type guardedRuneWriter struct {
	base *portBase
	raw  RuneWriter
}

func (g guardedRuneWriter) WriteRune(r rune) (int, error) {
	return guardedWriteRune(g.base, g.raw, r)
}

type guardedStringWriter struct {
	base *portBase
	raw  io.StringWriter
}

func (g guardedStringWriter) WriteString(s string) (int, error) {
	return guardedWriteString(g.base, g.raw, s)
}

type guardedFlusher struct {
	base *portBase
	raw  Flusher
}

func (g guardedFlusher) Flush() error {
	return guardedFlush(g.base, g.raw)
}
