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
	"sync"
)

var (
	_ Value        = (*String)(nil)
	_ fmt.Stringer = (*String)(nil)
)

// String interning for commonly used strings.
// Short strings (up to 64 characters) are automatically interned.
const stringInternMaxLen = 64

var stringInterns sync.Map // map[string]*String

// String represents a Scheme string value.
type String struct {
	Value string
}

// NewString returns a String value. Short strings (up to 64 characters)
// are automatically interned and return the same pointer for the same value.
func NewString(str string) *String {
	if len(str) <= stringInternMaxLen {
		return InternString(str)
	}
	return &String{Value: str}
}

// InternString returns an interned String for the given value.
// Multiple calls with the same string value return the same pointer.
func InternString(str string) *String {
	if existing, ok := stringInterns.Load(str); ok {
		return existing.(*String)
	}
	newStr := &String{Value: str}
	actual, _ := stringInterns.LoadOrStore(str, newStr)
	return actual.(*String)
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
	if other, ok := v.(*String); ok {
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
