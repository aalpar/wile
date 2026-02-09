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
	"sync"
)

var (
	_ Value        = (*String)(nil)
	_ Hashable     = (*String)(nil)
	_ Indexable    = (*String)(nil)
	_ fmt.Stringer = (*String)(nil)
)

// String interning for commonly used strings.
// Short strings (up to 64 bytes) are automatically interned.
const stringInternMaxLen = 64

var stringInterns sync.Map // map[string]*String

// String represents a Scheme string value.
type String struct {
	Value string
}

// NewString returns a String value. Short strings (up to 64 bytes)
// are automatically interned and return the same pointer for the same value.
func NewString(str string) *String {
	if len(str) <= stringInternMaxLen {
		return InternString(str)
	}
	return &String{Value: str}
}

// NewMutableString returns a newly allocated String that is not interned.
// Use this for strings that may be mutated (e.g., via string-set! or string-fill!).
// R7RS §6.7: Procedures like string-copy return mutable strings.
func NewMutableString(str string) *String {
	return &String{Value: str}
}

// InternString returns an interned String for the given value.
// Multiple calls with the same string value return the same pointer.
func InternString(str string) *String {
	existing, ok := stringInterns.Load(str)
	if ok {
		return existing.(*String)
	}
	newStr := &String{Value: str}
	actual, _ := stringInterns.LoadOrStore(str, newStr)
	return actual.(*String)
}

// HashCode returns a hash of the string value.
func (p *String) HashCode() uint64 {
	return hashString(0x3, p.Value)
}

// Datum returns the underlying string value.
func (p *String) Datum() string {
	return p.Value
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

// SetChar sets the character at index k to the given rune.
// This modifies the string in place.
// R7RS §6.7: (string-set! string k char)
func (p *String) SetChar(k int, char rune) {
	runes := []rune(p.Value)
	runes[k] = char
	p.Value = string(runes)
}

// Fill fills the string (or a portion of it) with the given character.
// R7RS §6.7: (string-fill! string fill [start [end]])
func (p *String) Fill(char rune, start, end int) {
	runes := []rune(p.Value)
	for i := start; i < end; i++ {
		runes[i] = char
	}
	p.Value = string(runes)
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
//
// R7RS §6.7: (string-set! string k char) stores char in element k.
func (p *String) Set(i int, v Value) {
	p.SetChar(i, v.(*Character).Value)
}

// Runes returns the string as a slice of runes.
func (p *String) Runes() []rune {
	return []rune(p.Value)
}

// SetValue sets the entire string value.
func (p *String) SetValue(s string) {
	p.Value = s
}
