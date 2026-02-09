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

var _ Value = (*ByteVectorOutputPort)(nil)
var _ Port = (*ByteVectorOutputPort)(nil)
var _ OutputPort = (*ByteVectorOutputPort)(nil)
var _ BinaryWriter = (*ByteVectorOutputPort)(nil)
var _ io.WriteCloser = (*ByteVectorOutputPort)(nil)
var _ io.ByteWriter = (*ByteVectorOutputPort)(nil)

// ByteVectorOutputPort represents a Scheme output port writing to memory.
type ByteVectorOutputPort struct {
	wrt    *bufio.Writer
	clsr   io.Closer
	closed bool
}

// NewByteVectorOutputPort creates a new in-memory bytevector output port.
func NewByteVectorOutputPort(wrt *bufio.Writer) *ByteVectorOutputPort {
	return &ByteVectorOutputPort{wrt: wrt}
}

// NewByteVectorOutputPortFromWriter creates a new in-memory bytevector output port.
func NewByteVectorOutputPortFromWriter(wrt io.Writer) *ByteVectorOutputPort {
	q := &ByteVectorOutputPort{
		wrt: bufio.NewWriter(wrt),
	}
	closer, ok := wrt.(io.Closer)
	if ok {
		q.clsr = closer
	}
	return q
}

func (p *ByteVectorOutputPort) Flush() error {
	if p.closed {
		return ErrPortClosed
	}
	return p.wrt.Flush()
}

func (p *ByteVectorOutputPort) Close() error {
	defer func() { p.closed = true }()
	flushErr := p.Flush()
	if p.clsr != nil {
		closeErr := p.clsr.Close()
		if closeErr != nil {
			return closeErr
		}
	}
	return flushErr
}

func (p *ByteVectorOutputPort) IsClosed() bool {
	return p.closed
}

func (p *ByteVectorOutputPort) Write(bs []byte) (n int, err error) {
	if p.closed {
		return 0, ErrPortClosed
	}
	return p.wrt.Write(bs)
}

func (p *ByteVectorOutputPort) WriteByte(b byte) error {
	if p.closed {
		return ErrPortClosed
	}
	return p.wrt.WriteByte(b)
}

// Datum returns the underlying bytes.Buffer.
func (p *ByteVectorOutputPort) Datum() *bufio.Writer {
	return p.wrt
}

// IsVoid returns true if this port is nil.
func (p *ByteVectorOutputPort) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both ports share the same buffer.
func (p *ByteVectorOutputPort) EqualTo(v Value) bool {
	other, ok := v.(*ByteVectorOutputPort)
	if ok {
		return p.wrt == other.wrt
	}
	return false
}

// SchemeString returns the Scheme representation of this port.
func (p *ByteVectorOutputPort) SchemeString() string {
	return fmt.Sprintf("<bytevector-output-port %p>", p.wrt)
}
