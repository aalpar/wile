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
	portBase
	buf *bytes.Buffer
}

// NewByteVectorInputOutputPortFromBuffer creates a new in-memory bytevector output port.
func NewByteVectorInputOutputPortFromBuffer(buf *bytes.Buffer) *ByteVectorInputOutputPort {
	q := &ByteVectorInputOutputPort{buf: buf}
	q.kind = portKindBytevectorInputOutput
	q.datum = q.buf
	return q
}

// NewByteVectorInputOutputPort creates a new in-memory bytevector output port.
func NewByteVectorInputOutputPort() *ByteVectorInputOutputPort {
	q := &ByteVectorInputOutputPort{buf: &bytes.Buffer{}}
	q.kind = portKindBytevectorInputOutput
	q.datum = q.buf
	return q
}

func (p *ByteVectorInputOutputPort) Flush() error {
	return p.guardClosed()
}

func (p *ByteVectorInputOutputPort) Write(bs []byte) (int, error) {
	return guardedWrite(&p.portBase, p.buf, bs)
}

func (p *ByteVectorInputOutputPort) Read(bs []byte) (int, error) {
	return guardedRead(&p.portBase, p.buf, bs)
}

func (p *ByteVectorInputOutputPort) WriteByte(b byte) error {
	return guardedWriteByte(&p.portBase, p.buf, b)
}

func (p *ByteVectorInputOutputPort) ReadByte() (byte, error) {
	return guardedReadByte(&p.portBase, p.buf)
}

// UnreadByte unreads the last byte read, allowing it to be read again.
func (p *ByteVectorInputOutputPort) UnreadByte() error {
	return guardedUnreadByte(&p.portBase, p.buf)
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
