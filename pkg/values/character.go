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
	"fmt"
	"unicode"
	"unicode/utf8"
)

var (
	_ Value    = (*Character)(nil)
	_ Hashable = (*Character)(nil)
)

// Flyweight pattern (GoF, 1994): pre-allocates a pool of frequently-used
// immutable objects and returns shared references instead of new allocations.
// Python caches [-5, 256], Java caches [-128, 127]; Wile caches ASCII [0, 127].
// See BIBLIOGRAPHY.md "Flyweight Pattern / Value Caching".
//
// Character cache for ASCII characters (0 to 127).
const charCacheMax = 127

var charCache [charCacheMax + 1]*Character

func init() {
	for i := rune(0); i <= charCacheMax; i++ {
		charCache[i] = &Character{Value: i}
	}
}

// Character represents a Scheme character value.
type Character struct {
	Value rune
}

// NewCharacter creates a new character from a rune.
func NewCharacter(v rune) *Character {
	if v >= 0 && v <= charCacheMax {
		return charCache[v]
	}
	q := &Character{Value: v}
	return q
}

// HashCode returns a hash of the character value.
func (p *Character) HashCode() uint64 {
	return hashUint64(0x4, uint64(p.Value))
}

// IsVoid returns true if the character is nil.
func (p *Character) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both characters have the same rune value.
func (p *Character) EqualTo(v Value) bool {
	other, ok := v.(*Character)
	if ok {
		return p.Value == other.Value
	}
	return false
}

// charNames maps the 9 R7RS named characters (§6.6) to their mnemonic names.
// Characters not in this map use #\<char> for graphic chars or #\xHEX for non-graphic.
var charNames = map[rune]string{
	'\a': "alarm",
	'\b': "backspace",
	0x7F: "delete",
	0x1B: "escape",
	'\n': "newline",
	0x00: "null",
	'\r': "return",
	' ':  "space",
	'\t': "tab",
}

// SchemeString returns the Scheme representation of the character.
// Named characters use the R7RS mnemonic form (#\newline). Graphic characters
// use #\<char>. Non-graphic non-named characters use #\xHEX for round-trip safety.
func (p *Character) SchemeString() string {
	name, ok := charNames[p.Value]
	if ok {
		return `#\` + name
	}
	if unicode.IsGraphic(p.Value) {
		return fmt.Sprintf(`#\%c`, p.Value)
	}
	return fmt.Sprintf(`#\x%X`, p.Value)
}

func (p *Character) String() string {
	buf := make([]byte, 0, utf8.UTFMax)
	q := utf8.AppendRune(buf, p.Value)
	return string(q)
}
