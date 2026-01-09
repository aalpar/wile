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
	"strings"
)

var _ Value = (*StringInputPort)(nil)

// StringInputPort represents a Scheme input port reading from a string.
type StringInputPort struct {
	reader *strings.Reader
}

// NewStringInputPort creates a new input port reading from the given string.
func NewStringInputPort(s string) *StringInputPort {
	return &StringInputPort{reader: strings.NewReader(s)}
}

// ReadRune reads and returns the next rune from the port.
func (p *StringInputPort) ReadRune() (rune, int, error) {
	return p.reader.ReadRune()
}

// UnreadRune unreads the last rune read, allowing it to be read again.
func (p *StringInputPort) UnreadRune() error {
	return p.reader.UnreadRune()
}

// Datum returns the underlying strings.Reader.
func (p *StringInputPort) Datum() *strings.Reader {
	return p.reader
}

// IsVoid returns true if this port is nil.
func (p *StringInputPort) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both ports share the same reader.
func (p *StringInputPort) EqualTo(v Value) bool {
	if other, ok := v.(*StringInputPort); ok {
		return p.reader == other.reader
	}
	return false
}

// SchemeString returns the Scheme representation of this port.
func (p *StringInputPort) SchemeString() string {
	return fmt.Sprintf("<string-input-port %p>", p.reader)
}
