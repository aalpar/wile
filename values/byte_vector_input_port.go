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
	"fmt"
	"io"
)

var _ Value = (*ByteVectorInputPort)(nil)
var _ Port = (*ByteVectorInputPort)(nil)
var _ InputPort = (*ByteVectorInputPort)(nil)
var _ BinaryReader = (*ByteVectorInputPort)(nil)
var _ io.ReadCloser = (*ByteVectorInputPort)(nil)
var _ io.ByteScanner = (*ByteVectorInputPort)(nil)

// ByteVectorInputPort represents a Scheme input port reading from a byte slice.
type ByteVectorInputPort struct {
	portBase
	rdr *bufio.Reader
}

// NewByteVectorInputPort creates a new input port reading from the given byte slice.
func NewByteVectorInputPort(reader *bufio.Reader) *ByteVectorInputPort {
	return &ByteVectorInputPort{rdr: reader}
}

// NewByteVectorInputPortFromReader creates a new input port reading from the given byte slice.
func NewByteVectorInputPortFromReader(reader io.Reader) *ByteVectorInputPort {
	q := &ByteVectorInputPort{rdr: bufio.NewReader(reader)}
	q.setCloser(reader)
	return q
}

func (p *ByteVectorInputPort) Read(bs []byte) (int, error) {
	return guardedRead(&p.portBase, p.rdr, bs)
}

// ReadByte reads and returns the next byte from the port.
func (p *ByteVectorInputPort) ReadByte() (byte, error) {
	return guardedReadByte(&p.portBase, p.rdr)
}

// UnreadByte unreads the last byte read, allowing it to be read again.
func (p *ByteVectorInputPort) UnreadByte() error {
	return guardedUnreadByte(&p.portBase, p.rdr)
}

// Datum returns the underlying bytes.Reader.
func (p *ByteVectorInputPort) Datum() *bufio.Reader {
	return p.rdr
}

// IsVoid returns true if this port is nil.
func (p *ByteVectorInputPort) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both ports share the same rdr.
func (p *ByteVectorInputPort) EqualTo(v Value) bool {
	other, ok := v.(*ByteVectorInputPort)
	if ok {
		return p.rdr == other.rdr
	}
	return false
}

// SchemeString returns the Scheme representation of this port.
func (p *ByteVectorInputPort) SchemeString() string {
	return fmt.Sprintf("<bytevector-input-port %p>", p.rdr)
}
