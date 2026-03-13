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

var _ Value = (*ByteVectorOutputPort)(nil)
var _ Port = (*ByteVectorOutputPort)(nil)
var _ OutputPort = (*ByteVectorOutputPort)(nil)
var _ BinaryWriter = (*ByteVectorOutputPort)(nil)
var _ io.WriteCloser = (*ByteVectorOutputPort)(nil)
var _ io.ByteWriter = (*ByteVectorOutputPort)(nil)

// ByteVectorOutputPort represents a Scheme output port writing to memory.
type ByteVectorOutputPort struct {
	portBase
	wrt *bufio.Writer
}

// NewByteVectorOutputPortFromWriter creates a new in-memory bytevector output port.
func NewByteVectorOutputPortFromWriter(wrt io.Writer) *ByteVectorOutputPort {
	q := &ByteVectorOutputPort{
		wrt: bufio.NewWriter(wrt),
	}
	q.kind = portKindBytevectorOutput
	q.datum = q.wrt
	q.setCloser(wrt)
	return q
}

func (p *ByteVectorOutputPort) Flush() error {
	return guardedFlush(&p.portBase, p.wrt)
}

// Close flushes buffered data and closes the underlying stream.
func (p *ByteVectorOutputPort) Close() error {
	return flushThenClose(p.wrt, &p.portBase)
}

func (p *ByteVectorOutputPort) Write(bs []byte) (int, error) {
	return guardedWrite(&p.portBase, p.wrt, bs)
}

func (p *ByteVectorOutputPort) WriteByte(b byte) error {
	return guardedWriteByte(&p.portBase, p.wrt, b)
}

// Datum returns the underlying bytes.Buffer.
func (p *ByteVectorOutputPort) Datum() *bufio.Writer {
	return p.wrt
}

// IsVoid returns true if the port is nil.
func (p *ByteVectorOutputPort) IsVoid() bool {
	return p == nil
}
