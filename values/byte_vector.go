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
	"strings"
)

var (
	_ Value     = (*ByteVector)(nil)
	_ Indexable = (*ByteVector)(nil)
)

// ByteVector represents a Scheme bytevector.
type ByteVector []*Byte

// NewByteVector creates a new bytevector from byte values.
func NewByteVector(vs ...*Byte) *ByteVector {
	if len(vs) == 0 {
		return &ByteVector{}
	}
	bs := make([]*Byte, len(vs))
	q := ByteVector(bs)
	for i := range vs {
		b := NewByte(vs[i].Value)
		q[i] = b
	}
	return &q
}

func NewByteVectorFromBytes(vs ...byte) *ByteVector {
	if len(vs) == 0 {
		return NewByteVector()
	}
	bs := make([]*Byte, len(vs))
	q := ByteVector(bs)
	for i := range vs {
		b := NewByte(vs[i])
		q[i] = b
	}
	return &q
}

// NewByteVectorFromIntegers creates a new bytevector from integer values.
// Each integer must be in the range [0, 255] per R7RS §6.4.
func NewByteVectorFromIntegers(vs ...*Integer) (*ByteVector, error) {
	if len(vs) == 0 {
		return &ByteVector{}, nil
	}
	bs := make([]*Byte, len(vs))
	q := ByteVector(bs)
	for i := range vs {
		v := vs[i].Value
		if v < 0 || v > 255 {
			return nil, WrapForeignErrorf(ErrNotAByte, "NewByteVectorFromIntegers: integer %d is not a byte (0-255)", v)
		}
		b := NewByte(uint8(v))
		q[i] = b
	}
	return &q, nil
}

func (p *ByteVector) Get(i int) Value {
	return (*p)[i]
}

func (p *ByteVector) Set(i int, value Value) {
	x, ok := value.(*Byte)
	if !ok {
		panic(NewForeignErrorf("bytevector element must be a byte: %v", ErrNotAByte))
	}
	(*p)[i] = x
}

// AsList converts the vector to a proper list (linked list of pairs).
// Returns void (nil Pair) if the vector is void.
// Returns EmptyList if the vector is empty.
// Otherwise returns a newly constructed list containing the vector's elements.
func (p *ByteVector) AsList() Tuple {
	if p.IsVoid() {
		return (*Pair)(nil)
	}
	vs := make([]Value, len(*p))
	for i, b := range *p {
		vs[i] = b
	}
	return List(vs...)
}

func (p *ByteVector) Length() int {
	return len(*p)
}

// AsBytes converts the bytevector to a Go byte slice.
// The starti and endi parameters specify the range of bytes to include.
// If starti is negative, it is treated as 0.
// If endi is greater than the length of the bytevector or non-positive, it is treated as the length of the bytevector.
// If starti is greater than endi, it is treated as equal to endi.
func (p *ByteVector) AsBytes(is ...int) []byte {
	starti := 0
	endi := p.Length()
	if len(is) >= 1 {
		starti = is[0]
	}
	if len(is) >= 2 {
		endi = is[1]
	}
	if endi > p.Length() || endi < 0 {
		endi = p.Length()
	}
	if starti < 0 {
		starti = 0
	}
	if starti > endi {
		starti = endi
	}
	out := make([]byte, endi-starti)
	for i, v := range (*p)[starti:endi] {
		out[i] = v.Value
	}
	return out
}

// Datum returns the underlying byte slice.
func (p *ByteVector) Datum() []*Byte {
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
