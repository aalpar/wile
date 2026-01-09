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

var _ Value = (*CharacterOutputPort)(nil)

// need to be implemented here so that we have a custom interface that can be used without calling Datum() and doing type assertions elsewhere.
// _ Port  = (Port)(*CharacterInputPort)(nil)

// CharacterOutputPort represents a Scheme textual output port.
type CharacterOutputPort struct {
	Value io.Writer
}

// NewCharacterOutputPortFromWriter creates a new character output port wrapping the given writer.
func NewCharacterOutputPortFromWriter(wrt io.Writer) *CharacterOutputPort {
	q := &CharacterOutputPort{Value: wrt}
	return q
}

// Datum returns the underlying data of the CharacterOutputPort as an io.Writer.
// there is no RunWriter interface in the standard library, so we just use io.Writer here.
func (p *CharacterOutputPort) Datum() io.Writer {
	return p.Value
}

// IsVoid returns true if the port is nil.
func (p *CharacterOutputPort) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both ports wrap the same writer.
func (p *CharacterOutputPort) EqualTo(v Value) bool {
	if other, ok := v.(*CharacterOutputPort); ok {
		return p.Value == other.Value
	}
	return false
}

// SchemeString returns the Scheme representation of the port.
func (p *CharacterOutputPort) SchemeString() string {
	return fmt.Sprintf("<character-output-port %p>", p.Value)
}
