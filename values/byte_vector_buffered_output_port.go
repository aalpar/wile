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

var _ Value = (*ByteVectorBufferdOutputPort)(nil)
var _ Port = (*ByteVectorBufferdOutputPort)(nil)
var _ OutputPort = (*ByteVectorBufferdOutputPort)(nil)
var _ BinaryWriter = (*ByteVectorBufferdOutputPort)(nil)
var _ ByteVectorExtractor = (*ByteVectorBufferdOutputPort)(nil)
var _ io.WriteCloser = (*ByteVectorBufferdOutputPort)(nil)
var _ io.ByteWriter = (*ByteVectorBufferdOutputPort)(nil)

// ByteVectorBufferdOutputPort represents a Scheme output port writing to memory.
type ByteVectorBufferdOutputPort struct {
	portBase
	buf *bytes.Buffer
}

// NewByteVectorBufferdOutputPort creates a new in-memory bytevector output port.
func NewByteVectorBufferdOutputPort() *ByteVectorBufferdOutputPort {
	return &ByteVectorBufferdOutputPort{
		buf: bytes.NewBuffer([]byte{}),
	}
}

// NewByteVectorBufferdOutputPortFromBuffer creates a new in-memory bytevector output port.
func NewByteVectorBufferdOutputPortFromBuffer(buf *bytes.Buffer) *ByteVectorBufferdOutputPort {
	q := &ByteVectorBufferdOutputPort{
		buf: buf,
	}
	return q
}

func (p *ByteVectorBufferdOutputPort) Flush() error {
	return p.guardClosed()
}

func (p *ByteVectorBufferdOutputPort) Write(bs []byte) (int, error) {
	return guardedWrite(&p.portBase, p.buf, bs)
}

func (p *ByteVectorBufferdOutputPort) WriteByte(b byte) error {
	return guardedWriteByte(&p.portBase, p.buf, b)
}

// Datum returns the underlying bytes.Buffer.
func (p *ByteVectorBufferdOutputPort) Datum() *bytes.Buffer {
	return p.buf
}

func (p *ByteVectorBufferdOutputPort) ReadByteVector() (*ByteVector, error) {
	b := p.buf.Bytes()
	return NewByteVectorFromBytes(b...), nil
}

// IsVoid returns true if this port is nil.
func (p *ByteVectorBufferdOutputPort) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both ports share the same buffer.
func (p *ByteVectorBufferdOutputPort) EqualTo(v Value) bool {
	other, ok := v.(*ByteVectorBufferdOutputPort)
	if ok {
		return p.buf == other.buf
	}
	return false
}

// SchemeString returns the Scheme representation of this port.
func (p *ByteVectorBufferdOutputPort) SchemeString() string {
	return fmt.Sprintf("<bytevector-output-port %p>", p.buf)
}
