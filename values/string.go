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

	"github.com/aalpar/wile/werr"
)

var (
	_ Value        = (*String)(nil)
	_ Hashable     = (*String)(nil)
	_ fmt.Stringer = (*String)(nil)
)

// String represents a Scheme string value.
// R7RS §6.7: Literal strings and strings from symbol->string are immutable.
type String struct {
	Value     string
	immutable bool
}

// NewString returns an immutable String value.
// R7RS §6.7: Literal strings and strings from symbol->string are immutable.
// Use NewMutableString for runtime-allocated strings that may be mutated.
func NewString(str string) *String {
	return &String{Value: str, immutable: true}
}

// NewMutableString returns a mutable String value.
// Use this for strings that may be mutated (e.g., via string-set! or string-fill!).
// R7RS §6.7: Procedures like string-copy return mutable strings.
func NewMutableString(str string) *String {
	return &String{Value: str, immutable: false}
}

// HashCode returns a hash of the string value.
func (p *String) HashCode() uint64 {
	return hashString(0x3, p.Value)
}

// IsVoid returns true if the string is nil.
func (p *String) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the strings have equal values.
func (p *String) EqualTo(v Value) bool {
	other, ok := v.(*String)
	if ok {
		return p.Value == other.Value
	}
	return false
}

// SchemeString returns the Scheme representation of the string.
func (p *String) SchemeString() string {
	return fmt.Sprintf("%q", p.Value)
}

func (p *String) String() string {
	return p.Value
}

// IsImmutable returns true if the string cannot be mutated.
// Literal strings and strings returned by symbol->string are immutable.
// R7RS §6.7: It is an error to apply mutation procedures to literal strings
// or strings returned by symbol->string.
func (p *String) IsImmutable() bool {
	return p.immutable
}

// SetChar sets the character at index k to the given rune.
// Returns an error if the string is immutable.
//
// R7RS §6.7: (string-set! string k char)
// R7RS §6.7: "It is an error" to mutate literal strings or strings
// returned by symbol->string. This implementation signals an error
// when mutation is attempted on immutable strings.
func (p *String) SetChar(k int, char rune) error {
	if p.immutable {
		return werr.WrapForeignErrorf(werr.ErrImmutableString, "cannot modify immutable string")
	}
	runes := []rune(p.Value)
	runes[k] = char
	p.Value = string(runes)
	return nil
}

// Fill fills the string (or a portion of it) with the given character.
// Returns an error if the string is immutable.
// R7RS §6.7: (string-fill! string fill [start [end]])
func (p *String) Fill(char rune, start, end int) error {
	if p.immutable {
		return werr.WrapForeignErrorf(werr.ErrImmutableString, "cannot modify immutable string")
	}
	runes := []rune(p.Value)
	for i := start; i < end; i++ {
		runes[i] = char
	}
	p.Value = string(runes)
	return nil
}

// Len returns the length of the string in characters (runes).
func (p *String) Len() int {
	return len([]rune(p.Value))
}

// Length returns the length of the string in characters (runes).
//
// R7RS §6.7: (string-length string) returns the number of characters.
func (p *String) Length() int {
	return len([]rune(p.Value))
}

// Get returns the character at the given rune index as a Character value.
//
// R7RS §6.7: (string-ref string k) returns character k of string.
func (p *String) Get(i int) Value {
	return NewCharacter([]rune(p.Value)[i])
}

// Set sets the character at the given rune index from a Character value.
// Returns an error if the string is immutable.
//
// R7RS §6.7: (string-set! string k char) stores char in element k.
func (p *String) Set(i int, v Value) error {
	if p.immutable {
		return werr.WrapForeignErrorf(werr.ErrImmutableString, "cannot modify immutable string")
	}
	return p.SetChar(i, v.(*Character).Value)
}

// StringOrFalse returns a Scheme string if s is non-empty, or #f if empty.
// Follows the BoolToBoolean precedent for eliminating repeated if/else patterns.
func StringOrFalse(s string) Value {
	if s == "" {
		return FalseValue
	}
	return NewString(s)
}

// Runes returns the string as a slice of runes.
func (p *String) Runes() []rune {
	return []rune(p.Value)
}

// SetValue sets the entire string value.
// Returns an error if the string is immutable.
func (p *String) SetValue(s string) error {
	if p.immutable {
		return werr.WrapForeignErrorf(werr.ErrImmutableString, "cannot modify immutable string")
	}
	p.Value = s
	return nil
}
