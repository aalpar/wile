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

var _ Value = (*BinaryInputPort)(nil)
var _ Port = (*BinaryInputPort)(nil)
var _ InputPort = (*BinaryInputPort)(nil)
var _ BinaryReader = (*BinaryInputPort)(nil)
var _ io.ByteScanner = (*BinaryInputPort)(nil)
var _ io.Closer = (*BinaryInputPort)(nil)

// BinaryInputPort represents a Scheme binary input port.
type BinaryInputPort struct {
	portBase
	rdr *bufio.Reader
}

// NewBinaryInputPort creates a new binary input port wrapping the given rdr.
func NewBinaryInputPort(rdr *bufio.Reader) *BinaryInputPort {
	q := &BinaryInputPort{rdr: rdr}
	q.kind = portKindBinaryInput
	q.datum = q.rdr
	return q
}

// NewBinaryInputPortFromReader creates a new input port reading from the given byte slice.
func NewBinaryInputPortFromReader(reader io.Reader) *BinaryInputPort {
	q := &BinaryInputPort{rdr: bufio.NewReader(reader)}
	q.kind = portKindBinaryInput
	q.datum = q.rdr
	q.setCloser(reader)
	return q
}

func (p *BinaryInputPort) ReadByte() (byte, error) {
	return guardedReadByte(&p.portBase, p.rdr)
}

func (p *BinaryInputPort) UnreadByte() error {
	return guardedUnreadByte(&p.portBase, p.rdr)
}

func (p *BinaryInputPort) Read(bs []byte) (int, error) {
	return guardedRead(&p.portBase, p.rdr, bs)
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
	other, ok := v.(*BinaryInputPort)
	if ok {
		return p.rdr == other.rdr
	}
	return false
}
