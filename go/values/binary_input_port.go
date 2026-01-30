// Copyright 2025 Aaron Alpar
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

var _ Value = (*BinaryInputPort)(nil)
var _ io.ByteScanner = (*BinaryInputPort)(nil)
var _ io.Closer = (*BinaryInputPort)(nil)

// BinaryInputPort represents a Scheme binary input port.
type BinaryInputPort struct {
	rdr    *bufio.Reader
	clsr   io.Closer
	closed bool
}

// NewBinaryInputPort creates a new binary input port wrapping the given rdr.
func NewBinaryInputPort(rdr *bufio.Reader) *BinaryInputPort {
	return &BinaryInputPort{rdr: rdr}
}

// NewBinaryInputPortFromReader creates a new input port reading from the given byte slice.
func NewBinaryInputPortFromReader(reader io.Reader) *BinaryInputPort {
	q := &BinaryInputPort{rdr: bufio.NewReader(reader)}
	closer, ok := reader.(io.Closer)
	if ok {
		q.clsr = closer
	}
	return q
}

func (p *BinaryInputPort) ReadByte() (byte, error) {
	if p.closed {
		return 0, ErrPortClosed
	}
	return p.rdr.ReadByte()
}

func (p *BinaryInputPort) UnreadByte() error {
	if p.closed {
		return ErrPortClosed
	}
	return p.rdr.UnreadByte()
}

func (p *BinaryInputPort) Read(bs []byte) (n int, err error) {
	if p.closed {
		return 0, ErrPortClosed
	}
	return p.rdr.Read(bs)
}

func (p *BinaryInputPort) Close() error {
	defer func() { p.closed = true }()
	if p.clsr != nil {
		return p.clsr.Close()
	}
	return nil
}

func (p *BinaryInputPort) IsClosed() bool {
	return p.closed
}

// Datum returns the underlying io.Reader.
func (p *BinaryInputPort) Datum() io.Reader {
	return p.rdr
}

// IsVoid returns true if the port is nil.
func (p *BinaryInputPort) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both ports wrap the same rdr.
func (p *BinaryInputPort) EqualTo(v Value) bool {
	if other, ok := v.(*BinaryInputPort); ok {
		return p.rdr == other.rdr
	}
	return false
}

// SchemeString returns the Scheme representation of the port.
func (p *BinaryInputPort) SchemeString() string {
	return fmt.Sprintf("<binary-input-port %p>", p.rdr)
}
