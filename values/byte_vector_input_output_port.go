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
	"bytes"
	"fmt"
	"io"
)

var _ Value = (*ByteVectorInputOutputPort)(nil)
var _ Port = (*ByteVectorInputOutputPort)(nil)
var _ InputOutputPort = (*ByteVectorInputOutputPort)(nil)
var _ BinaryReader = (*ByteVectorInputOutputPort)(nil)
var _ BinaryWriter = (*ByteVectorInputOutputPort)(nil)
var _ ByteVectorExtractor = (*ByteVectorInputOutputPort)(nil)
var _ io.WriteCloser = (*ByteVectorInputOutputPort)(nil)
var _ io.ByteWriter = (*ByteVectorInputOutputPort)(nil)
var _ io.ReadCloser = (*ByteVectorInputOutputPort)(nil)
var _ io.ByteReader = (*ByteVectorInputOutputPort)(nil)

// ByteVectorInputOutputPort represents a Scheme output port writing to memory.
type ByteVectorInputOutputPort struct {
	buf    *bytes.Buffer
	closed bool
}

// NewByteVectorInputOutputPortFromBuffer creates a new in-memory bytevector output port.
func NewByteVectorInputOutputPortFromBuffer(buf *bytes.Buffer) *ByteVectorInputOutputPort {
	return &ByteVectorInputOutputPort{buf: buf}
}

// NewByteVectorInputOutputPort creates a new in-memory bytevector output port.
func NewByteVectorInputOutputPort() *ByteVectorInputOutputPort {
	return &ByteVectorInputOutputPort{buf: &bytes.Buffer{}}
}

func (p *ByteVectorInputOutputPort) Flush() error {
	if p.closed {
		return ErrPortClosed
	}
	return nil
}

func (p *ByteVectorInputOutputPort) Close() error {
	defer func() { p.closed = true }()
	return nil
}

func (p *ByteVectorInputOutputPort) IsClosed() bool {
	return p.closed
}

func (p *ByteVectorInputOutputPort) Write(bs []byte) (n int, err error) {
	if p.closed {
		return 0, ErrPortClosed
	}
	return p.buf.Write(bs)
}

func (p *ByteVectorInputOutputPort) Read(bs []byte) (int, error) {
	if p.closed {
		return 0, ErrPortClosed
	}
	return p.buf.Read(bs)
}

func (p *ByteVectorInputOutputPort) WriteByte(b byte) error {
	if p.closed {
		return ErrPortClosed
	}
	return p.buf.WriteByte(b)
}

func (p *ByteVectorInputOutputPort) ReadByte() (byte, error) {
	if p.closed {
		return 0, ErrPortClosed
	}
	return p.buf.ReadByte()
}

// UnreadByte unreads the last byte read, allowing it to be read again.
func (p *ByteVectorInputOutputPort) UnreadByte() error {
	if p.closed {
		return ErrPortClosed
	}
	return p.buf.UnreadByte()
}

func (p *ByteVectorInputOutputPort) ReadByteVector() (*ByteVector, error) {
	b := p.buf.Bytes()
	if len(b) == 0 {
		return nil, io.EOF
	}
	return NewByteVectorFromBytes(b...), nil
}

// Datum returns the underlying bytes.Buffer.
func (p *ByteVectorInputOutputPort) Datum() *bytes.Buffer {
	return p.buf
}

// IsVoid returns true if this port is nil.
func (p *ByteVectorInputOutputPort) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both ports share the same buffer.
func (p *ByteVectorInputOutputPort) EqualTo(v Value) bool {
	other, ok := v.(*ByteVectorInputOutputPort)
	if ok {
		return p.buf == other.buf
	}
	return false
}

// SchemeString returns the Scheme representation of this port.
func (p *ByteVectorInputOutputPort) SchemeString() string {
	return fmt.Sprintf("<bytevector-input-output-port %p>", p.buf)
}
