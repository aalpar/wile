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

import "github.com/aalpar/wile/pkg/werr"

var (
	_ Value     = (*ByteVector)(nil)
	_ Immutable = (*ByteVector)(nil)
)

// ByteVector represents a Scheme bytevector.
//
// Like Vector, it carries its own immutability rather than being tracked in an
// engine-scoped side set; see that type's comment for the write discipline.
type ByteVector struct {
	elems     []*Byte
	immutable bool
}

// NewByteVector creates a new bytevector from byte values.
func NewByteVector(vs ...*Byte) *ByteVector {
	if len(vs) == 0 {
		return &ByteVector{}
	}
	q := &ByteVector{elems: make([]*Byte, len(vs))}
	for i := range vs {
		b := NewByte(vs[i].Value)
		q.elems[i] = b
	}
	return q
}

func NewByteVectorFromBytes(vs ...byte) *ByteVector {
	if len(vs) == 0 {
		return NewByteVector()
	}
	q := &ByteVector{elems: make([]*Byte, len(vs))}
	for i := range vs {
		b := NewByte(vs[i])
		q.elems[i] = b
	}
	return q
}

// NewByteVectorFromIntegers creates a new bytevector from integer values.
// Each integer must be in the range [0, 255] per R7RS §6.9.
func NewByteVectorFromIntegers(vs ...*Integer) (*ByteVector, error) {
	if len(vs) == 0 {
		return &ByteVector{}, nil
	}
	q := &ByteVector{elems: make([]*Byte, len(vs))}
	for i := range vs {
		err := ValidateByteValue(vs[i], "NewByteVectorFromIntegers", "element")
		if err != nil {
			return nil, err
		}
		b := NewByte(uint8(vs[i].Value))
		q.elems[i] = b
	}
	return q, nil
}

// Elems returns the bytevector's backing element slice. It is the storage, not a
// copy; see Vector.Elems for what that costs a caller.
func (p *ByteVector) Elems() []*Byte {
	if p.IsVoid() {
		return nil
	}
	return p.elems
}

func (p *ByteVector) Get(i int) Value {
	return p.elems[i]
}

// Set sets the element at the specified index to the given value.
// Returns an error if the bytevector is immutable or the value is not a Byte.
func (p *ByteVector) Set(i int, value Value) error {
	if p.immutable {
		return werr.WrapForeignErrorf(werr.ErrImmutableBytevector, "ByteVector.Set: cannot mutate immutable bytevector")
	}
	x, ok := value.(*Byte)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAByte, "bytevector element must be a byte")
	}
	p.elems[i] = x
	return nil
}

// IsImmutable reports whether in-place mutation of this bytevector is forbidden.
// R7RS §4.1.2: a quoted-literal constant is immutable.
func (p *ByteVector) IsImmutable() bool {
	return p != nil && p.immutable
}

// setImmutable marks the bytevector immutable. Unexported for the same reason
// Vector.setImmutable is; values.MarkImmutable is the entry point.
func (p *ByteVector) setImmutable() {
	if p == nil {
		return
	}
	p.immutable = true
}

// AsList converts the vector to a proper list (linked list of pairs).
// Returns void (nil Pair) if the vector is void.
// Returns EmptyList if the vector is empty.
// Otherwise returns a newly constructed list containing the vector's elements.
func (p *ByteVector) AsList() Tuple {
	if p.IsVoid() {
		return (*Pair)(nil)
	}
	vs := make([]Value, len(p.elems))
	for i, b := range p.elems {
		vs[i] = b
	}
	return List(vs...)
}

func (p *ByteVector) Length() int {
	if p.IsVoid() {
		return 0
	}
	return len(p.elems)
}

// AsBytes converts the bytevector to a Go byte slice.
// The starti and endi parameters specify the range of bytes to include.
// If starti is negative, it is treated as 0.
// If endi is greater than the length of the bytevector or negative, it is treated as the length of the bytevector.
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
	for i, v := range p.elems[starti:endi] {
		out[i] = v.Value
	}
	return out
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
	if len(p.elems) != len(other.elems) {
		return false
	}
	for i := range p.elems {
		if p.elems[i].Value != other.elems[i].Value {
			return false
		}
	}
	return true
}

// SchemeString returns the Scheme representation of the bytevector.
// Bytevector elements are bytes, never compound, so no cycle-detection set
// is needed (nil visited) and the depth bound is never reached (bytes cannot
// recurse). Elements sit one level below the bytevector root (depth 2).
func (p *ByteVector) SchemeString() string {
	return formatIndexable("#u8(", len(p.elems), func(i int) Value {
		return p.elems[i]
	}, nil, 2)
}
