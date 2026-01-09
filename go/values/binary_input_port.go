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
	"fmt"
	"io"
)

var _ Value = (*BinaryInputPort)(nil)

// BinaryInputPort represents a Scheme binary input port.
type BinaryInputPort struct {
	Value io.Reader
}

// NewBinaryInputPort creates a new binary input port wrapping the given reader.
func NewBinaryInputPort(rdr io.Reader) *BinaryInputPort {
	return &BinaryInputPort{Value: rdr}
}

func (p *BinaryInputPort) Read(buf []byte) (int, error) {
	return p.Value.Read(buf)
}

// Datum returns the underlying io.Reader.
func (p *BinaryInputPort) Datum() io.Reader {
	return p.Value
}

// IsVoid returns true if the port is nil.
func (p *BinaryInputPort) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both ports wrap the same reader.
func (p *BinaryInputPort) EqualTo(v Value) bool {
	if other, ok := v.(*BinaryInputPort); ok {
		return p.Value == other.Value
	}
	return false
}

// SchemeString returns the Scheme representation of the port.
func (p *BinaryInputPort) SchemeString() string {
	return fmt.Sprintf("<binary-input-port %p>", p.Value)
}
