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

var _ Value = (*CharacterInputPort)(nil)

// TODO: type should implement Port interface.  Port interface not yet defined.  Exposed methods may include IsOpen, Close, etc.
// need to be implemented here so that we have a custom interface that can be used without calling Datum() and doing type assertions elsewhere.
// _ Port  = (Port)(*CharacterInputPort)(nil)

// CharacterInputPort represents a Scheme textual input port.
type CharacterInputPort struct {
	Value io.RuneReader
}

// NewCharacterInputPortFromReader creates a new character input port from an io.Reader.
func NewCharacterInputPortFromReader(rdr io.Reader) *CharacterInputPort {
	q := &CharacterInputPort{Value: bufio.NewReader(rdr)}
	return q
}

// NewCharacterInputPort creates a new character input port from an io.RuneReader.
func NewCharacterInputPort(rdr io.RuneReader) *CharacterInputPort {
	q := &CharacterInputPort{Value: rdr}
	return q
}

// Datum returns the underlying io.RuneReader.
func (p *CharacterInputPort) Datum() io.RuneReader {
	return p.Value
}

// IsVoid returns true if the port is nil.
func (p *CharacterInputPort) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both ports wrap the same reader.
func (p *CharacterInputPort) EqualTo(v Value) bool {
	if other, ok := v.(*CharacterInputPort); ok {
		return p.Value == other.Value
	}
	return false
}

// SchemeString returns the Scheme representation of the port.
func (p *CharacterInputPort) SchemeString() string {
	return fmt.Sprintf("<character-input-port %p>", p.Value)
}
