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
	"bytes"
	"fmt"
)

var _ Value = (*BytevectorInputPort)(nil)

// BytevectorInputPort represents a Scheme input port reading from a byte slice.
type BytevectorInputPort struct {
	reader *bytes.Reader
}

// NewBytevectorInputPort creates a new input port reading from the given byte slice.
func NewBytevectorInputPort(data []byte) *BytevectorInputPort {
	return &BytevectorInputPort{reader: bytes.NewReader(data)}
}

func (p *BytevectorInputPort) Read(data []byte) (int, error) {
	return p.reader.Read(data)
}

// ReadByte reads and returns the next byte from the port.
func (p *BytevectorInputPort) ReadByte() (byte, error) {
	return p.reader.ReadByte()
}

// UnreadByte unreads the last byte read, allowing it to be read again.
func (p *BytevectorInputPort) UnreadByte() error {
	return p.reader.UnreadByte()
}

// Datum returns the underlying bytes.Reader.
func (p *BytevectorInputPort) Datum() *bytes.Reader {
	return p.reader
}

// IsVoid returns true if this port is nil.
func (p *BytevectorInputPort) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both ports share the same reader.
func (p *BytevectorInputPort) EqualTo(v Value) bool {
	if other, ok := v.(*BytevectorInputPort); ok {
		return p.reader == other.reader
	}
	return false
}

// SchemeString returns the Scheme representation of this port.
func (p *BytevectorInputPort) SchemeString() string {
	return fmt.Sprintf("<bytevector-input-port %p>", p.reader)
}
