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

var _ Value = (*ByteVectorBufferedOutputPort)(nil)
var _ Port = (*ByteVectorBufferedOutputPort)(nil)
var _ OutputPort = (*ByteVectorBufferedOutputPort)(nil)
var _ BinaryWriter = (*ByteVectorBufferedOutputPort)(nil)
var _ ByteVectorExtractor = (*ByteVectorBufferedOutputPort)(nil)
var _ io.WriteCloser = (*ByteVectorBufferedOutputPort)(nil)
var _ io.ByteWriter = (*ByteVectorBufferedOutputPort)(nil)

// ByteVectorBufferedOutputPort represents a Scheme output port writing to memory.
type ByteVectorBufferedOutputPort struct {
	portBase
	buf *bytes.Buffer
}

// NewByteVectorBufferedOutputPort creates a new in-memory bytevector output port.
func NewByteVectorBufferedOutputPort() *ByteVectorBufferedOutputPort {
	return &ByteVectorBufferedOutputPort{
		buf: bytes.NewBuffer([]byte{}),
	}
}

// NewByteVectorBufferedOutputPortFromBuffer creates a new in-memory bytevector output port.
func NewByteVectorBufferedOutputPortFromBuffer(buf *bytes.Buffer) *ByteVectorBufferedOutputPort {
	q := &ByteVectorBufferedOutputPort{
		buf: buf,
	}
	return q
}

func (p *ByteVectorBufferedOutputPort) Flush() error {
	return p.guardClosed()
}

func (p *ByteVectorBufferedOutputPort) Write(bs []byte) (int, error) {
	return guardedWrite(&p.portBase, p.buf, bs)
}

func (p *ByteVectorBufferedOutputPort) WriteByte(b byte) error {
	return guardedWriteByte(&p.portBase, p.buf, b)
}

// Datum returns the underlying bytes.Buffer.
func (p *ByteVectorBufferedOutputPort) Datum() *bytes.Buffer {
	return p.buf
}

func (p *ByteVectorBufferedOutputPort) ReadByteVector() (*ByteVector, error) {
	b := p.buf.Bytes()
	return NewByteVectorFromBytes(b...), nil
}

// IsVoid returns true if this port is nil.
func (p *ByteVectorBufferedOutputPort) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both ports share the same buffer.
func (p *ByteVectorBufferedOutputPort) EqualTo(v Value) bool {
	other, ok := v.(*ByteVectorBufferedOutputPort)
	if ok {
		return p.buf == other.buf
	}
	return false
}

// SchemeString returns the Scheme representation of this port.
func (p *ByteVectorBufferedOutputPort) SchemeString() string {
	return fmt.Sprintf("<bytevector-output-port %p>", p.buf)
}
