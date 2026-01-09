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

var _ Value = (*StringOutputPort)(nil)

// StringOutputPort represents a Scheme string output port backed by a buffer.
type StringOutputPort struct {
	buffer *bytes.Buffer
}

// NewStringOutputPort creates a new string output port.
func NewStringOutputPort() *StringOutputPort {
	return &StringOutputPort{buffer: &bytes.Buffer{}}
}

func (p *StringOutputPort) Write(data []byte) (int, error) {
	return p.buffer.Write(data)
}

// GetString returns the accumulated output as a string.
func (p *StringOutputPort) GetString() string {
	return p.buffer.String()
}

// Datum returns the underlying buffer.
func (p *StringOutputPort) Datum() *bytes.Buffer {
	return p.buffer
}

// IsVoid returns true if the port is nil.
func (p *StringOutputPort) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both ports use the same buffer.
func (p *StringOutputPort) EqualTo(v Value) bool {
	if other, ok := v.(*StringOutputPort); ok {
		return p.buffer == other.buffer
	}
	return false
}

// SchemeString returns the Scheme representation of the port.
func (p *StringOutputPort) SchemeString() string {
	return fmt.Sprintf("<string-output-port %p>", p.buffer)
}
