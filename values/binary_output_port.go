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
	portBase
	wrt *bufio.Writer
}

// NewBinaryOutputPortFromWriter creates a new input port reading from the given byte slice.
func NewBinaryOutputPortFromWriter(writer io.Writer) *BinaryOutputPort {
	q := &BinaryOutputPort{wrt: bufio.NewWriter(writer)}
	q.kind = portKindBinaryOutput
	q.datum = q.wrt
	q.setCloser(writer)
	return q
}

// Write writes bytes to the port.
func (p *BinaryOutputPort) Write(bs []byte) (int, error) {
	return guardedWrite(&p.portBase, p.wrt, bs)
}

// WriteByte writes a single byte to the port.
func (p *BinaryOutputPort) WriteByte(b byte) error {
	return guardedWriteByte(&p.portBase, p.wrt, b)
}

// Flush flushes the port's buffer.
func (p *BinaryOutputPort) Flush() error {
	return guardedFlush(&p.portBase, p.wrt)
}

// Close flushes buffered data and closes the underlying stream.
func (p *BinaryOutputPort) Close() error {
	return flushThenClose(p.wrt, &p.portBase)
}

// Datum returns the underlying io.Writer.
func (p *BinaryOutputPort) Datum() io.Writer {
	return p.wrt
}

// IsVoid returns true if the port is nil.
func (p *BinaryOutputPort) IsVoid() bool {
	return p == nil
}
