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

var _ Value = (*BinaryOutputPort)(nil)
var _ Port = (*BinaryOutputPort)(nil)
var _ OutputPort = (*BinaryOutputPort)(nil)
var _ BinaryWriter = (*BinaryOutputPort)(nil)
var _ io.Writer = (*BinaryOutputPort)(nil)
var _ io.ByteWriter = (*BinaryOutputPort)(nil)
var _ io.Closer = (*BinaryOutputPort)(nil)

// BinaryOutputPort represents a Scheme binary output port.
type BinaryOutputPort struct {
	wrt    *bufio.Writer
	clsr   io.Closer
	closed bool
}

// NewBinaryOutputPort creates a new binary output port wrapping the given buf.
func NewBinaryOutputPort(wrt *bufio.Writer) *BinaryOutputPort {
	return &BinaryOutputPort{wrt: wrt}
}

// NewBinaryOutputPortFromWriter creates a new input port reading from the given byte slice.
func NewBinaryOutputPortFromWriter(writer io.Writer) *BinaryOutputPort {
	q := &BinaryOutputPort{wrt: bufio.NewWriter(writer)}
	closer, ok := writer.(io.Closer)
	if ok {
		q.clsr = closer
	}
	return q
}

// Write writes bytes to the port.
func (p *BinaryOutputPort) Write(bs []byte) (int, error) {
	if p.closed {
		return 0, ErrPortClosed
	}
	return p.wrt.Write(bs)
}

// WriteByte writes a single byte to the port.
func (p *BinaryOutputPort) WriteByte(b byte) error {
	if p.closed {
		return ErrPortClosed
	}
	return p.wrt.WriteByte(b)
}

// Flush flushes the port's buffer.
func (p *BinaryOutputPort) Flush() error {
	if p.closed {
		return ErrPortClosed
	}
	return p.wrt.Flush()
}

func (p *BinaryOutputPort) Close() error {
	defer func() { p.closed = true }()
	if p.clsr != nil {
		return p.clsr.Close()
	}
	return nil
}

func (p *BinaryOutputPort) IsClosed() bool {
	return p.closed
}

// Datum returns the underlying io.Writer.
func (p *BinaryOutputPort) Datum() io.Writer {
	return p.wrt
}

// IsVoid returns true if the port is nil.
func (p *BinaryOutputPort) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both ports wrap the same buf.
func (p *BinaryOutputPort) EqualTo(v Value) bool {
	if other, ok := v.(*BinaryOutputPort); ok { //nolint:gocritic
		return p.wrt == other.wrt
	}
	return false
}

// SchemeString returns the Scheme representation of the port.
func (p *BinaryOutputPort) SchemeString() string {
	return fmt.Sprintf("<binary-output-port %p>", p.wrt)
}
