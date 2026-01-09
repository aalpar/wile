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

var _ Value = (*BytevectorOutputPort)(nil)

// BytevectorOutputPort represents a Scheme output port writing to memory.
type BytevectorOutputPort struct {
	buffer *bytes.Buffer
}

// NewBytevectorOutputPort creates a new in-memory bytevector output port.
func NewBytevectorOutputPort() *BytevectorOutputPort {
	return &BytevectorOutputPort{buffer: &bytes.Buffer{}}
}

// Write writes data to the port's buffer.
func (p *BytevectorOutputPort) Write(data []byte) (int, error) {
	return p.buffer.Write(data)
}

// WriteByte writes a single byte to the port's buffer.
func (p *BytevectorOutputPort) WriteByte(b byte) error {
	return p.buffer.WriteByte(b)
}

// GetBytevector returns the accumulated bytes written to this port.
func (p *BytevectorOutputPort) GetBytevector() []byte {
	return p.buffer.Bytes()
}

// Datum returns the underlying bytes.Buffer.
func (p *BytevectorOutputPort) Datum() *bytes.Buffer {
	return p.buffer
}

// IsVoid returns true if this port is nil.
func (p *BytevectorOutputPort) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both ports share the same buffer.
func (p *BytevectorOutputPort) EqualTo(v Value) bool {
	if other, ok := v.(*BytevectorOutputPort); ok {
		return p.buffer == other.buffer
	}
	return false
}

// SchemeString returns the Scheme representation of this port.
func (p *BytevectorOutputPort) SchemeString() string {
	return fmt.Sprintf("<bytevector-output-port %p>", p.buffer)
}
