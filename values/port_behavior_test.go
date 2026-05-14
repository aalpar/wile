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

// Tests in this file exercise the runtime behavior of *PortObject
// produced by the New*Port factories. They consume the public
// accessor API (AsByteReader, AsWriter, etc.) — operations are reached
// through the typed accessor, not through narrow interface assertion.
//
// Test count delta vs. the deleted concrete-port test files
// (binary_port_test.go, character_port_test.go, string_port_test.go,
// byte_vector_port_test.go, port_coverage_test.go) is recorded in the
// PR body.

package values_test

import (
	"bufio"
	"bytes"
	"errors"
	"io"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// --- I8 factory contract: kind + Validate pass for each factory.
//
// Phase 2 §2.13 acceptance: "Constructor I8 contract: one positive
// test per New* factory asserts that the produced *PortObject has
// PortKind() matching the expected portKind* constant and that
// Validate() returns nil. 15 such assertions total."
//
// Validate is called by each constructor (panic on failure); we
// re-assert it here for documentation.

func TestNewBinaryInputPort_Kind(t *testing.T) {
	c := qt.New(t)
	p := values.NewBinaryInputPort(bufioReaderFromBytes(t, []byte{1}))
	c.Assert(p.PortKind(), qt.Equals, "binary-input-port")
	c.Assert(p.Validate(), qt.IsNil)
}

func TestNewBinaryInputPortFromReader_Kind(t *testing.T) {
	c := qt.New(t)
	p := values.NewBinaryInputPortFromReader(bytes.NewReader([]byte{1}))
	c.Assert(p.PortKind(), qt.Equals, "binary-input-port")
	c.Assert(p.Validate(), qt.IsNil)
}

func TestNewBinaryOutputPortFromWriter_Kind(t *testing.T) {
	c := qt.New(t)
	p := values.NewBinaryOutputPortFromWriter(&bytes.Buffer{})
	c.Assert(p.PortKind(), qt.Equals, "binary-output-port")
	c.Assert(p.Validate(), qt.IsNil)
	// R7RS forbids write-string on binary ports.
	_, hasStringWriter := p.AsStringWriter()
	c.Assert(hasStringWriter, qt.IsFalse)
}

func TestNewCharacterInputPort_Kind(t *testing.T) {
	c := qt.New(t)
	p := values.NewCharacterInputPort(bufioReaderFromBytes(t, []byte("a")))
	c.Assert(p.PortKind(), qt.Equals, "character-input-port")
	c.Assert(p.Validate(), qt.IsNil)
}

func TestNewCharacterInputPortFromReader_Kind(t *testing.T) {
	c := qt.New(t)
	p := values.NewCharacterInputPortFromReader(strings.NewReader("a"))
	c.Assert(p.PortKind(), qt.Equals, "character-input-port")
	c.Assert(p.Validate(), qt.IsNil)
}

func TestNewCharacterOutputPortFromWriter_Kind(t *testing.T) {
	c := qt.New(t)
	p := values.NewCharacterOutputPortFromWriter(&bytes.Buffer{})
	c.Assert(p.PortKind(), qt.Equals, "character-output-port")
	c.Assert(p.Validate(), qt.IsNil)
}

func TestNewStringInputPortWithBuffer_Kind(t *testing.T) {
	c := qt.New(t)
	p := values.NewStringInputPortWithBuffer(bytes.NewBufferString("a"))
	c.Assert(p.PortKind(), qt.Equals, "string-input-port")
	c.Assert(p.Validate(), qt.IsNil)
}

func TestNewStringOutputPort_Kind(t *testing.T) {
	c := qt.New(t)
	p := values.NewStringOutputPort()
	c.Assert(p.PortKind(), qt.Equals, "string-output-port")
	c.Assert(p.Validate(), qt.IsNil)
}

func TestNewStringOutputPortWithBuffer_Kind(t *testing.T) {
	c := qt.New(t)
	p := values.NewStringOutputPortWithBuffer(&bytes.Buffer{})
	c.Assert(p.PortKind(), qt.Equals, "string-output-port")
	c.Assert(p.Validate(), qt.IsNil)
}

func TestNewByteVectorInputPortFromReader_Kind(t *testing.T) {
	c := qt.New(t)
	p := values.NewByteVectorInputPortFromReader(bytes.NewReader([]byte{1}))
	c.Assert(p.PortKind(), qt.Equals, "bytevector-input-port")
	c.Assert(p.Validate(), qt.IsNil)
}

func TestNewByteVectorOutputPortFromWriter_Kind(t *testing.T) {
	c := qt.New(t)
	p := values.NewByteVectorOutputPortFromWriter(&bytes.Buffer{})
	c.Assert(p.PortKind(), qt.Equals, "bytevector-output-port")
	c.Assert(p.Validate(), qt.IsNil)
}

func TestNewByteVectorBufferedOutputPort_Kind(t *testing.T) {
	c := qt.New(t)
	p := values.NewByteVectorBufferedOutputPort()
	c.Assert(p.PortKind(), qt.Equals, "bytevector-output-port")
	c.Assert(p.Validate(), qt.IsNil)
}

func TestNewByteVectorBufferedOutputPortFromBuffer_Kind(t *testing.T) {
	c := qt.New(t)
	p := values.NewByteVectorBufferedOutputPortFromBuffer(&bytes.Buffer{})
	c.Assert(p.PortKind(), qt.Equals, "bytevector-output-port")
	c.Assert(p.Validate(), qt.IsNil)
}

func TestNewByteVectorInputOutputPort_Kind(t *testing.T) {
	c := qt.New(t)
	p := values.NewByteVectorInputOutputPort()
	c.Assert(p.PortKind(), qt.Equals, "bytevector-input-output-port")
	c.Assert(p.Validate(), qt.IsNil)
}

func TestNewByteVectorInputOutputPortFromBuffer_Kind(t *testing.T) {
	c := qt.New(t)
	p := values.NewByteVectorInputOutputPortFromBuffer(&bytes.Buffer{})
	c.Assert(p.PortKind(), qt.Equals, "bytevector-input-output-port")
	c.Assert(p.Validate(), qt.IsNil)
}

// --- Behavior: read + write through accessors.

func TestBinaryInputPort_ReadByteThroughAccessor(t *testing.T) {
	c := qt.New(t)
	p := values.NewBinaryInputPortFromReader(bytes.NewReader([]byte{42, 99}))
	br, ok := p.AsByteReader()
	c.Assert(ok, qt.IsTrue)
	b, err := br.ReadByte()
	c.Assert(err, qt.IsNil)
	c.Assert(b, qt.Equals, byte(42))
	b, err = br.ReadByte()
	c.Assert(err, qt.IsNil)
	c.Assert(b, qt.Equals, byte(99))
	_, err = br.ReadByte()
	c.Assert(errors.Is(err, io.EOF), qt.IsTrue)
}

func TestBinaryInputPort_UnreadByte(t *testing.T) {
	c := qt.New(t)
	p := values.NewBinaryInputPortFromReader(bytes.NewReader([]byte{1, 2}))
	br, _ := p.AsByteReader()
	urb, ok := p.AsByteUnreader()
	c.Assert(ok, qt.IsTrue)

	b, _ := br.ReadByte()
	c.Assert(b, qt.Equals, byte(1))

	c.Assert(urb.UnreadByte(), qt.IsNil)
	b, _ = br.ReadByte()
	c.Assert(b, qt.Equals, byte(1))
}

func TestBinaryOutputPort_Write(t *testing.T) {
	c := qt.New(t)
	buf := &bytes.Buffer{}
	p := values.NewBinaryOutputPortFromWriter(buf)
	w, ok := p.AsWriter()
	c.Assert(ok, qt.IsTrue)
	n, err := w.Write([]byte{1, 2, 3})
	c.Assert(err, qt.IsNil)
	c.Assert(n, qt.Equals, 3)

	bw, _ := p.AsByteWriter()
	c.Assert(bw.WriteByte(4), qt.IsNil)

	flsh, _ := p.AsFlusher()
	c.Assert(flsh.Flush(), qt.IsNil)
	c.Assert(buf.Bytes(), qt.DeepEquals, []byte{1, 2, 3, 4})
}

func TestCharacterInputPort_ReadRune(t *testing.T) {
	c := qt.New(t)
	p := values.NewCharacterInputPortFromReader(strings.NewReader("abc"))
	rr, ok := p.AsRuneReader()
	c.Assert(ok, qt.IsTrue)
	r, _, err := rr.ReadRune()
	c.Assert(err, qt.IsNil)
	c.Assert(r, qt.Equals, 'a')

	urr, ok := p.AsRuneUnreader()
	c.Assert(ok, qt.IsTrue)
	c.Assert(urr.UnreadRune(), qt.IsNil)

	r, _, _ = rr.ReadRune()
	c.Assert(r, qt.Equals, 'a')
}

func TestCharacterOutputPort_WriteString(t *testing.T) {
	c := qt.New(t)
	buf := &bytes.Buffer{}
	p := values.NewCharacterOutputPortFromWriter(buf)
	sw, ok := p.AsStringWriter()
	c.Assert(ok, qt.IsTrue)
	_, err := sw.WriteString("hi ")
	c.Assert(err, qt.IsNil)
	rw, _ := p.AsRuneWriter()
	_, err = rw.WriteRune('!')
	c.Assert(err, qt.IsNil)
	flsh, _ := p.AsFlusher()
	c.Assert(flsh.Flush(), qt.IsNil)
	c.Assert(buf.String(), qt.Equals, "hi !")
}

func TestStringOutputPort_StringContent(t *testing.T) {
	c := qt.New(t)
	p := values.NewStringOutputPort()
	sw, ok := p.AsStringWriter()
	c.Assert(ok, qt.IsTrue)
	_, _ = sw.WriteString("hello")
	s, ok := p.StringContent()
	c.Assert(ok, qt.IsTrue)
	c.Assert(s, qt.Equals, "hello")
}

func TestStringInputPort_Read(t *testing.T) {
	c := qt.New(t)
	p := values.NewStringInputPortWithBuffer(bytes.NewBufferString("hello"))
	r, ok := p.AsReader()
	c.Assert(ok, qt.IsTrue)
	buf := make([]byte, 5)
	n, err := r.Read(buf)
	c.Assert(err, qt.IsNil)
	c.Assert(n, qt.Equals, 5)
	c.Assert(string(buf), qt.Equals, "hello")
}

func TestByteVectorInputOutputPort_WriteThenRead(t *testing.T) {
	c := qt.New(t)
	p := values.NewByteVectorInputOutputPort()
	bw, _ := p.AsByteWriter()
	c.Assert(bw.WriteByte(7), qt.IsNil)
	br, _ := p.AsByteReader()
	b, err := br.ReadByte()
	c.Assert(err, qt.IsNil)
	c.Assert(b, qt.Equals, byte(7))
}

func TestByteVectorBufferedOutputPort_ReadByteVector(t *testing.T) {
	c := qt.New(t)
	p := values.NewByteVectorBufferedOutputPort()
	w, _ := p.AsWriter()
	_, err := w.Write([]byte{1, 2, 3})
	c.Assert(err, qt.IsNil)
	ext, ok := p.AsByteVectorExtractor()
	c.Assert(ok, qt.IsTrue)
	bv, err := ext.ReadByteVector()
	c.Assert(err, qt.IsNil)
	c.Assert(len(*bv), qt.Equals, 3)
}

func TestByteVectorInputOutputPort_ReadByteVector_EmptyEOF(t *testing.T) {
	c := qt.New(t)
	p := values.NewByteVectorInputOutputPort()
	ext, _ := p.AsByteVectorExtractor()
	_, err := ext.ReadByteVector()
	c.Assert(errors.Is(err, io.EOF), qt.IsTrue,
		qt.Commentf("InputOutputPort.ReadByteVector returns EOF on empty"))
}

func TestByteVectorBufferedOutputPort_ReadByteVector_EmptyNoEOF(t *testing.T) {
	c := qt.New(t)
	p := values.NewByteVectorBufferedOutputPort()
	ext, _ := p.AsByteVectorExtractor()
	bv, err := ext.ReadByteVector()
	c.Assert(err, qt.IsNil)
	c.Assert(len(*bv), qt.Equals, 0,
		qt.Commentf("BufferedOutputPort.ReadByteVector returns empty, not EOF"))
}

// --- Behavior: operations on closed ports return ErrPortClosed.

func TestPortObject_OperationsAfterClose_ReturnPortClosed(t *testing.T) {
	c := qt.New(t)

	// Binary input
	p := values.NewBinaryInputPortFromReader(bytes.NewReader([]byte{1}))
	br, _ := p.AsByteReader()
	urb, _ := p.AsByteUnreader()
	r, _ := p.AsReader()
	c.Assert(p.Close(), qt.IsNil)
	_, err := br.ReadByte()
	c.Assert(errors.Is(err, werr.ErrPortClosed), qt.IsTrue)
	c.Assert(errors.Is(urb.UnreadByte(), werr.ErrPortClosed), qt.IsTrue)
	_, err = r.Read(make([]byte, 1))
	c.Assert(errors.Is(err, werr.ErrPortClosed), qt.IsTrue)

	// Binary output
	p = values.NewBinaryOutputPortFromWriter(&bytes.Buffer{})
	w, _ := p.AsWriter()
	bw, _ := p.AsByteWriter()
	flsh, _ := p.AsFlusher()
	c.Assert(p.Close(), qt.IsNil)
	c.Assert(errors.Is(bw.WriteByte(1), werr.ErrPortClosed), qt.IsTrue)
	_, err = w.Write([]byte{1})
	c.Assert(errors.Is(err, werr.ErrPortClosed), qt.IsTrue)
	c.Assert(errors.Is(flsh.Flush(), werr.ErrPortClosed), qt.IsTrue)
}

// TestPortObject_OperationsAfterClose_TextualReadSide covers character
// input + string input — the rune-read side of closed-port semantics.
func TestPortObject_OperationsAfterClose_TextualReadSide(t *testing.T) {
	c := qt.New(t)
	type tc struct {
		name string
		port *values.PortObject
	}
	cases := []tc{
		{"character-input", values.NewCharacterInputPortFromReader(strings.NewReader("abc"))},
		{"string-input", values.NewStringInputPortWithBuffer(bytes.NewBufferString("abc"))},
	}
	for _, t := range cases {
		rr, _ := t.port.AsRuneReader()
		urr, _ := t.port.AsRuneUnreader()
		c.Assert(t.port.Close(), qt.IsNil, qt.Commentf("kind=%s", t.name))
		_, _, err := rr.ReadRune()
		c.Assert(errors.Is(err, werr.ErrPortClosed), qt.IsTrue,
			qt.Commentf("kind=%s ReadRune after close", t.name))
		c.Assert(errors.Is(urr.UnreadRune(), werr.ErrPortClosed), qt.IsTrue,
			qt.Commentf("kind=%s UnreadRune after close", t.name))
	}
}

// TestPortObject_OperationsAfterClose_TextualWriteSide covers character
// output + string output — the rune/string-write side of closed-port
// semantics.
func TestPortObject_OperationsAfterClose_TextualWriteSide(t *testing.T) {
	c := qt.New(t)
	type tc struct {
		name string
		port *values.PortObject
	}
	cases := []tc{
		{"character-output", values.NewCharacterOutputPortFromWriter(&bytes.Buffer{})},
		{"string-output", values.NewStringOutputPort()},
	}
	for _, t := range cases {
		w, _ := t.port.AsWriter()
		rw, _ := t.port.AsRuneWriter()
		sw, _ := t.port.AsStringWriter()
		c.Assert(t.port.Close(), qt.IsNil, qt.Commentf("kind=%s", t.name))
		_, err := w.Write([]byte("x"))
		c.Assert(errors.Is(err, werr.ErrPortClosed), qt.IsTrue,
			qt.Commentf("kind=%s Write after close", t.name))
		_, err = rw.WriteRune('x')
		c.Assert(errors.Is(err, werr.ErrPortClosed), qt.IsTrue,
			qt.Commentf("kind=%s WriteRune after close", t.name))
		_, err = sw.WriteString("x")
		c.Assert(errors.Is(err, werr.ErrPortClosed), qt.IsTrue,
			qt.Commentf("kind=%s WriteString after close", t.name))
	}
}

// TestPortObject_OperationsAfterClose_BytevectorPorts covers the
// bytevector input/output families — they share guard wrappers with
// the binary families but exercise distinct factories.
func TestPortObject_OperationsAfterClose_BytevectorPorts(t *testing.T) {
	c := qt.New(t)

	pin := values.NewByteVectorInputPortFromReader(bytes.NewReader([]byte{1}))
	br, _ := pin.AsByteReader()
	c.Assert(pin.Close(), qt.IsNil)
	_, err := br.ReadByte()
	c.Assert(errors.Is(err, werr.ErrPortClosed), qt.IsTrue)

	pout := values.NewByteVectorOutputPortFromWriter(&bytes.Buffer{})
	bw, _ := pout.AsByteWriter()
	c.Assert(pout.Close(), qt.IsNil)
	c.Assert(errors.Is(bw.WriteByte(1), werr.ErrPortClosed), qt.IsTrue)

	pio := values.NewByteVectorInputOutputPort()
	bwio, _ := pio.AsByteWriter()
	brio, _ := pio.AsByteReader()
	c.Assert(pio.Close(), qt.IsNil)
	c.Assert(errors.Is(bwio.WriteByte(1), werr.ErrPortClosed), qt.IsTrue)
	_, err = brio.ReadByte()
	c.Assert(errors.Is(err, werr.ErrPortClosed), qt.IsTrue)
}

// recordingCloser tracks whether Close was invoked. Used to verify
// that the FromReader/FromWriter factories propagate Close calls to
// the underlying io.Closer (setCloser path in portBase).
type recordingCloser struct {
	io.Reader
	closed bool
}

func (r *recordingCloser) Close() error {
	r.closed = true
	return nil
}

// TestPortObject_CloseWithCloser verifies that closing a port whose
// underlying source implements io.Closer propagates the Close call.
// Subsumes the deleted TestBinaryInputPort_CloseWithCloser test.
func TestPortObject_CloseWithCloser(t *testing.T) {
	c := qt.New(t)
	rc := &recordingCloser{Reader: bytes.NewReader([]byte{1})}
	p := values.NewBinaryInputPortFromReader(rc)
	c.Assert(p.Close(), qt.IsNil)
	c.Assert(p.IsClosed(), qt.IsTrue)
	c.Assert(rc.closed, qt.IsTrue,
		qt.Commentf("NewBinaryInputPortFromReader should propagate Close to underlying io.Closer"))
}

// --- Helpers.

// bufioReaderFromBytes wraps a byte slice in a *bufio.Reader for
// constructors that require a *bufio.Reader directly.
func bufioReaderFromBytes(t *testing.T, bs []byte) *bufio.Reader {
	t.Helper()
	return bufio.NewReader(bytes.NewReader(bs))
}
