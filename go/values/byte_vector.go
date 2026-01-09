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
	"strings"
)

var _ Value = (*ByteVector)(nil)

// ByteVector represents a Scheme bytevector.
type ByteVector []Byte

// NewByteVector creates a new bytevector from integer values.
func NewByteVector(vs ...*Integer) *ByteVector {
	if len(vs) == 0 {
		return &ByteVector{}
	}
	q := ByteVector(make([]Byte, len(vs)))
	for i := range vs {
		// FIXME: handle overflow, ugly but necessary
		b := NewByte(uint8((*vs[i]).Value))
		q[i] = *b
	}
	return &q
}

// Datum returns the underlying byte slice.
func (p *ByteVector) Datum() []Byte {
	return *p
}

// IsVoid returns true if the bytevector is nil.
func (p *ByteVector) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the bytevectors have equal contents.
func (p *ByteVector) EqualTo(v Value) bool {
	other, ok := v.(*ByteVector)
	if !ok {
		return false
	}
	if len(*p) != len(*other) {
		return false
	}
	for i := range *p {
		if (*p)[i].Value != (*other)[i].Value {
			return false
		}
	}
	return true
}

// SchemeString returns the Scheme representation of the bytevector.
func (p *ByteVector) SchemeString() string {
	q := &strings.Builder{}
	q.WriteString("#u8(")
	if len(*p) > 0 {
		q.WriteString(" ")
		q.WriteString((*p)[0].SchemeString())
		for _, v := range (*p)[1:] {
			q.WriteString(" ")
			q.WriteString(v.SchemeString())
		}
		q.WriteString(" ")
	}
	q.WriteString(")")
	return q.String()
}
