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
	"bufio"
	"bytes"
	"io"

	"github.com/aalpar/wile/pkg/werr"
)

// Port constructors — produce *PortObject with the slot configuration
// matching the named R7RS port flavor. Each factory writes both the
// portKind* tag and the slot set in the same struct literal; Validate
// enforces the cross-slot invariants I1–I7.
//
// Slots are populated with guarding wrappers (see port_helpers.go)
// that check portBase.closed before delegating to the raw underlying
// stdlib type. This preserves the R7RS port-closed rejection
// semantics provided previously by methods like (*BinaryInputPort).
// ReadByte.
//
// The factories panic on Validate failure: an invariant violation
// means the constructor itself is broken (programmer error), not a
// runtime condition.

// validateOrPanic invokes Validate and panics with a wrap-error on
// failure. Used by every New*Port to enforce I1–I7 at construction time.
func validateOrPanic(p *PortObject) {
	err := p.Validate()
	if err != nil {
		panic(werr.WrapForeignErrorf(err,
			"port constructor: produced invalid port"))
	}
}

// NewBinaryInputPort wraps an existing *bufio.Reader as a binary input
// port (R7RS §6.13.2 binary port).
func NewBinaryInputPort(rdr *bufio.Reader) *PortObject {
	q := &PortObject{}
	q.kind = portKindBinaryInput
	q.datum = rdr
	q.rdr = guardedReader{base: &q.portBase, raw: rdr}
	q.rb = guardedByteReader{base: &q.portBase, raw: rdr}
	q.urb = guardedByteUnreader{base: &q.portBase, raw: rdr}
	validateOrPanic(q)
	return q
}

// NewBinaryInputPortFromReader buffers reader and constructs a binary
// input port. If reader implements io.Closer, Close is propagated.
func NewBinaryInputPortFromReader(reader io.Reader) *PortObject {
	rd := bufio.NewReader(reader)
	q := &PortObject{}
	q.kind = portKindBinaryInput
	q.datum = rd
	q.setCloser(reader)
	q.rdr = guardedReader{base: &q.portBase, raw: rd}
	q.rb = guardedByteReader{base: &q.portBase, raw: rd}
	q.urb = guardedByteUnreader{base: &q.portBase, raw: rd}
	validateOrPanic(q)
	return q
}

// NewBinaryOutputPortFromWriter buffers writer and constructs a binary
// output port. ws is intentionally nil — R7RS forbids write-string on
// binary ports even though *bufio.Writer satisfies io.StringWriter.
// flsh is non-nil so Close flushes before closing.
func NewBinaryOutputPortFromWriter(writer io.Writer) *PortObject {
	wr := bufio.NewWriter(writer)
	q := &PortObject{}
	q.kind = portKindBinaryOutput
	q.datum = wr
	q.setCloser(writer)
	q.wrt = guardedWriter{base: &q.portBase, raw: wr}
	q.wb = guardedByteWriter{base: &q.portBase, raw: wr}
	q.flsh = guardedFlusher{base: &q.portBase, raw: wr}
	validateOrPanic(q)
	return q
}

// NewCharacterInputPort wraps an existing *bufio.Reader as a textual
// input port (R7RS §6.13.2 textual port).
func NewCharacterInputPort(rdr *bufio.Reader) *PortObject {
	q := &PortObject{}
	q.kind = portKindCharacterInput
	q.datum = rdr
	q.rdr = guardedReader{base: &q.portBase, raw: rdr}
	q.rr = guardedRuneReader{base: &q.portBase, raw: rdr}
	q.urr = guardedRuneUnreader{base: &q.portBase, raw: rdr}
	validateOrPanic(q)
	return q
}

// NewCharacterInputPortFromReader buffers rdr and constructs a textual
// input port.
func NewCharacterInputPortFromReader(rdr io.Reader) *PortObject {
	rd := bufio.NewReader(rdr)
	q := &PortObject{}
	q.kind = portKindCharacterInput
	q.datum = rd
	q.setCloser(rdr)
	q.rdr = guardedReader{base: &q.portBase, raw: rd}
	q.rr = guardedRuneReader{base: &q.portBase, raw: rd}
	q.urr = guardedRuneUnreader{base: &q.portBase, raw: rd}
	validateOrPanic(q)
	return q
}

// NewCharacterOutputPortFromWriter buffers wrt and constructs a textual
// output port. wb is intentionally nil — R7RS textual ports do not
// expose byte-level writes even though *bufio.Writer satisfies
// io.ByteWriter. flsh is non-nil so Close flushes before closing.
func NewCharacterOutputPortFromWriter(wrt io.Writer) *PortObject {
	wr := bufio.NewWriter(wrt)
	q := &PortObject{}
	q.kind = portKindCharacterOutput
	q.datum = wr
	q.setCloser(wrt)
	q.wrt = guardedWriter{base: &q.portBase, raw: wr}
	q.wr = guardedRuneWriter{base: &q.portBase, raw: wr}
	q.ws = guardedStringWriter{base: &q.portBase, raw: wr}
	q.flsh = guardedFlusher{base: &q.portBase, raw: wr}
	validateOrPanic(q)
	return q
}

// NewStringInputPortWithBuffer wraps a *bytes.Buffer as a string input
// port (R7RS §6.13.2 textual). bytes.Buffer satisfies io.Reader,
// io.RuneReader, and RuneUnreader directly — no buffering layer.
func NewStringInputPortWithBuffer(buffer *bytes.Buffer) *PortObject {
	q := &PortObject{}
	q.kind = portKindStringInput
	q.datum = buffer
	q.rdr = guardedReader{base: &q.portBase, raw: buffer}
	q.rr = guardedRuneReader{base: &q.portBase, raw: buffer}
	q.urr = guardedRuneUnreader{base: &q.portBase, raw: buffer}
	validateOrPanic(q)
	return q
}

// NewStringInputPortWithReaders constructs a string input port whose
// rune reader and rune unreader are supplied externally. Used by
// fault-injecting test infrastructure (e.g., internal/extensions/
// iotest) that needs to override read/unread semantics while still
// producing a *PortObject that production code's type assertions
// accept. Kind is portKindStringInput — caller is responsible for
// passing a reader whose semantics match a string-input port.
//
// rdr provides the byte-level Read; rr and urr provide rune-level.
// They are wrapped in separate guarded* wrappers, so callers must
// pass consistent semantics (typically rdr and rr both wrap the same
// underlying *bytes.Buffer-equivalent source).
//
// If rdr also implements io.Closer, Close is propagated.
// validateOrPanic runs as for any factory.
func NewStringInputPortWithReaders(rdr io.Reader, rr io.RuneReader, urr RuneUnreader) *PortObject {
	q := &PortObject{}
	q.kind = portKindStringInput
	q.datum = rdr
	q.setCloser(rdr)
	q.rdr = guardedReader{base: &q.portBase, raw: rdr}
	q.rr = guardedRuneReader{base: &q.portBase, raw: rr}
	q.urr = guardedRuneUnreader{base: &q.portBase, raw: urr}
	validateOrPanic(q)
	return q
}

// NewStringOutputPort creates a fresh string output port backed by an
// in-memory buffer. flsh is nil (no real flush needed); sext exposes
// the accumulated string via StringContent.
func NewStringOutputPort() *PortObject {
	return NewStringOutputPortWithBuffer(&bytes.Buffer{})
}

// NewStringOutputPortWithBuffer wraps the given buffer as a string
// output port. wb is intentionally nil — R7RS textual ports do not
// expose byte-level writes even though *bytes.Buffer satisfies
// io.ByteWriter. flsh is nil (no real flush needed).
func NewStringOutputPortWithBuffer(buffer *bytes.Buffer) *PortObject {
	q := &PortObject{}
	q.kind = portKindStringOutput
	q.datum = buffer
	q.wrt = guardedWriter{base: &q.portBase, raw: buffer}
	q.wr = guardedRuneWriter{base: &q.portBase, raw: buffer}
	q.ws = guardedStringWriter{base: &q.portBase, raw: buffer}
	q.sext = buffer
	validateOrPanic(q)
	return q
}

// NewByteVectorInputPortFromReader buffers reader and constructs a
// bytevector input port (R7RS §6.13.2 binary). Same slot set as
// NewBinaryInputPortFromReader; the kind tag distinguishes them.
func NewByteVectorInputPortFromReader(reader io.Reader) *PortObject {
	rd := bufio.NewReader(reader)
	q := &PortObject{}
	q.kind = portKindBytevectorInput
	q.datum = rd
	q.setCloser(reader)
	q.rdr = guardedReader{base: &q.portBase, raw: rd}
	q.rb = guardedByteReader{base: &q.portBase, raw: rd}
	q.urb = guardedByteUnreader{base: &q.portBase, raw: rd}
	validateOrPanic(q)
	return q
}

// NewByteVectorOutputPortFromWriter buffers wrt and constructs a
// bytevector output port. ws is nil (binary). flsh is non-nil so
// Close flushes before closing.
func NewByteVectorOutputPortFromWriter(wrt io.Writer) *PortObject {
	wr := bufio.NewWriter(wrt)
	q := &PortObject{}
	q.kind = portKindBytevectorOutput
	q.datum = wr
	q.setCloser(wrt)
	q.wrt = guardedWriter{base: &q.portBase, raw: wr}
	q.wb = guardedByteWriter{base: &q.portBase, raw: wr}
	q.flsh = guardedFlusher{base: &q.portBase, raw: wr}
	validateOrPanic(q)
	return q
}

// NewByteVectorBufferedOutputPort creates a fresh in-memory bytevector
// output port. flsh is nil. ext extracts the accumulated bytevector
// (returns the bytes verbatim — no EOF on empty, matching the prior
// ByteVectorBufferedOutputPort semantics).
func NewByteVectorBufferedOutputPort() *PortObject {
	return NewByteVectorBufferedOutputPortFromBuffer(bytes.NewBuffer([]byte{}))
}

// NewByteVectorBufferedOutputPortFromBuffer wraps the given buffer as a
// bytevector buffered output port.
func NewByteVectorBufferedOutputPortFromBuffer(buf *bytes.Buffer) *PortObject {
	q := &PortObject{}
	q.kind = portKindBytevectorOutput
	q.datum = buf
	q.wrt = guardedWriter{base: &q.portBase, raw: buf}
	q.wb = guardedByteWriter{base: &q.portBase, raw: buf}
	q.ext = bufferedBVExtractor{buf: buf}
	validateOrPanic(q)
	return q
}

// NewByteVectorInputOutputPort creates a fresh bidirectional bytevector
// port. Both sides share a *bytes.Buffer.
func NewByteVectorInputOutputPort() *PortObject {
	return NewByteVectorInputOutputPortFromBuffer(&bytes.Buffer{})
}

// NewByteVectorInputOutputPortFromBuffer wraps the given buffer as a
// bidirectional bytevector port. ext returns io.EOF when the buffer
// is empty (preserving the prior ByteVectorInputOutputPort semantics
// where empty is signalled differently from BufferedOutputPort).
func NewByteVectorInputOutputPortFromBuffer(buf *bytes.Buffer) *PortObject {
	q := &PortObject{}
	q.kind = portKindBytevectorInputOutput
	q.datum = buf
	q.rdr = guardedReader{base: &q.portBase, raw: buf}
	q.rb = guardedByteReader{base: &q.portBase, raw: buf}
	q.urb = guardedByteUnreader{base: &q.portBase, raw: buf}
	q.wrt = guardedWriter{base: &q.portBase, raw: buf}
	q.wb = guardedByteWriter{base: &q.portBase, raw: buf}
	q.ext = inputOutputBVExtractor{buf: buf}
	validateOrPanic(q)
	return q
}

// bufferedBVExtractor is the ByteVectorExtractor for buffered output
// ports. Returns the accumulated bytes verbatim; never signals EOF.
type bufferedBVExtractor struct {
	buf *bytes.Buffer
}

func (b bufferedBVExtractor) ReadByteVector() (*ByteVector, error) {
	return NewByteVectorFromBytes(b.buf.Bytes()...), nil
}

// inputOutputBVExtractor is the ByteVectorExtractor for input-output
// ports. Returns io.EOF when the buffer is empty.
type inputOutputBVExtractor struct {
	buf *bytes.Buffer
}

func (b inputOutputBVExtractor) ReadByteVector() (*ByteVector, error) {
	bs := b.buf.Bytes()
	if len(bs) == 0 {
		return nil, io.EOF
	}
	return NewByteVectorFromBytes(bs...), nil
}
