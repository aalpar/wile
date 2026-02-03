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
	"unicode/utf8"
)

var (
	_ Value    = (*Character)(nil)
	_ Hashable = (*Character)(nil)
)

// Character represents a Scheme character value.
type Character struct {
	Value rune
}

// NewCharacter creates a new character from a rune.
func NewCharacter(v rune) *Character {
	q := &Character{Value: v}
	return q
}

// HashCode returns a hash of the character value.
func (p *Character) HashCode() uint64 {
	return hashUint64(0x4, uint64(p.Value))
}

// Datum returns the underlying rune value.
func (p *Character) Datum() rune {
	return p.Value
}

// IsVoid returns true if the character is nil.
func (p *Character) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both characters have the same rune value.
func (p *Character) EqualTo(v Value) bool {
	if other, ok := v.(*Character); ok {
		return p.Value == other.Value
	}
	return false
}

// SchemeString returns the Scheme representation of the character.
func (p *Character) SchemeString() string {
	return fmt.Sprintf(`#\%c`, p.Value)
}

func (p *Character) String() string {
	buf := make([]byte, 0, utf8.UTFMax)
	q := utf8.AppendRune(buf, p.Value)
	return string(q)
}
