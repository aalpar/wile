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
	"context"
	"slices"
	"strings"
)

var (
	_ Value = (*ArrayList)(nil)
	_ Tuple = (*ArrayList)(nil)

	// FIXME: consider using types for EmptyList and Void
	ArrayListEmptyList = NewArrayList(nil, nil)
)

// ArrayList represents a mutable list backed by a slice.
type ArrayList []Value

// Datum returns the underlying slice.
func (p *ArrayList) Datum() []Value {
	return *p
}

// AsVector converts the ArrayList to a Vector.
func (p *ArrayList) AsVector() *Vector {
	if p.IsVoid() {
		return nil
	}
	q := NewVector()
	*q = make([]Value, len(*p))
	copy(*q, *p)
	return q
}

// NewArrayList creates a new ArrayList from the given values.
func NewArrayList(vs ...Value) *ArrayList {
	q := (ArrayList)(slices.Clone(vs))
	return &q
}

// Append adds a value to the list and returns a new ArrayList.
func (p *ArrayList) Append(vs Value) Value {
	if IsVoid(p) {
		panic(ErrNotAList)
	}
	if IsEmptyList(p) {
		return NewArrayList(vs)
	}
	q := p.Copy()
	if IsVoid(vs) {
		*q = append(*q, Void)
	} else if IsEmptyList(vs) {
		*q = append(*q, EmptyList)
	} else {
		*q = append(*q, vs)
	}
	return q
}

// AppendList appends another list to this ArrayList.
func (p *ArrayList) AppendList(o Value) *ArrayList {
	if IsVoid(p) {
		panic(ErrNotAList)
	}
	q := p.Copy()
	switch vs := o.(type) {
	case *ArrayList:
		if IsEmptyList(p) {
			return vs
		}
		if IsVoid(vs) {
			*q = append(*q, Void)
		} else {
			// FIXME: this is a bit ugly
			if len(*vs) == 0 {
				return q
			}
			if len(*vs) == 1 && (*vs)[0] == EmptyList {
				return q
			}
			if len(*vs) == 2 && IsVoid((*vs)[0]) && IsVoid((*vs)[1]) {
				return q
			}
			*q = (*q)[:len(*q)-1]
			*q = append(*q, (*vs)[:len(*vs)]...)
		}
	case *Pair:
		for v0 := vs; !v0.IsEmptyList(); {
			if v0.IsVoid() {
				panic(ErrNotAList)
			}
			if v0.IsEmptyList() {
				break
			}
			*q = append(*q, v0.Car())
			v0 = v0.Cdr().(*Pair)
		}
	default:
		if !IsEmptyList(p) {
			panic(ErrNotAList)
		}
		return NewArrayList(vs)
	}
	return q
}

// Car returns the first element of this ArrayList.
func (p *ArrayList) Car() Value {
	return (*p)[0]
}

// Cdr returns all but the first element of this ArrayList.
func (p *ArrayList) Cdr() Value {
	q := (*p)[1:]
	return &q
}

// IsList returns true if this ArrayList represents a proper list.
func (p *ArrayList) IsList() bool {
	// FIXME: ugly
	if p == nil {
		return false
	}
	if len(*p) == 0 {
		return false
	}
	if len(*p) == 1 && (*p)[0] == EmptyList {
		return true
	}
	if len(*p) == 2 && IsVoid((*p)[0]) && IsVoid((*p)[1]) {
		return true
	}
	return (*p)[len(*p)-1] == EmptyList
}

// Len returns the number of elements in this ArrayList.
func (p *ArrayList) Len() int {
	if !p.IsList() {
		panic(ErrNotAList)
	}
	if IsEmptyList(p) {
		return 0
	}
	return len(*p) - 1
}

// IsEmptyList returns true if this ArrayList represents an empty list.
func (p *ArrayList) IsEmptyList() bool {
	if p == nil {
		return false
	}
	if len(*p) == 1 && (*p)[0] == EmptyList {
		return true
	}
	if len(*p) == 2 && IsVoid((*p)[0]) && IsVoid((*p)[1]) {
		return true
	}
	return false
}

// IsVoid returns true if this ArrayList is void.
func (p *ArrayList) IsVoid() bool {
	if p == nil {
		return true
	}
	if len(*p) == 0 {
		return true
	}
	if len(*p) == 1 && IsVoid((*p)[0]) {
		return true
	}
	return false
}

// ForEach iterates over each element in this ArrayList.
func (p *ArrayList) ForEach(ctx context.Context, fn ForEachFunc) (Value, error) {
	if p == nil {
		return EmptyList, nil
	}
	l := len(*p)
	for i, v := range *p {
		err := fn(ctx, i, i < l-1, v)
		if err != nil {
			return EmptyList, err
		}
	}
	return EmptyList, nil
}

// AsList converts this ArrayList to a Pair-based list.
func (p *ArrayList) AsList() Value {
	if IsVoid(p) {
		return nil
	}
	q := NewCons(nil, EmptyList)
	current := q
	l := len(*p)
	for i := 0; i < l-2; i++ {
		current[0] = (*p)[i]
		current[1] = NewCons(nil, EmptyList)
		current = current[1].(*Pair)
	}
	current[0] = (*p)[l-2]
	end := (*p)[l-1]
	if IsEmptyList(end) {
		return q
	} else {
		current[1] = end
	}
	return q
}

// EqualTo returns true if this ArrayList equals another value.
func (p *ArrayList) EqualTo(o Value) bool {
	if p == nil || o == nil {
		return p == o
	}
	v, ok := o.(*ArrayList)
	if !ok {
		return false
	}
	if len(*p) != len(*v) {
		return false
	}
	for i := range *p {
		if IsVoid((*p)[i]) || IsVoid((*v)[i]) {
			if IsVoid((*p)[i]) && IsVoid((*v)[i]) {
				continue
			}
			return false
		}
		if !(*p)[i].EqualTo((*v)[i]) {
			return false
		}
	}
	return true
}

// Copy returns a shallow copy of this ArrayList.
func (p *ArrayList) Copy() *ArrayList {
	if p == nil {
		return nil
	}
	q := NewArrayList()
	*q = make([]Value, len(*p))
	copy(*q, *p)
	return q
}

// SchemeString returns the Scheme representation of this ArrayList.
func (p *ArrayList) SchemeString() string {
	if p == nil || len(*p) == 0 {
		return "#<void>"
	}
	if len(*p) == 1 {
		if IsEmptyList((*p)[0]) {
			return "()"
		}
		if IsVoid((*p)[0]) {
			return "#<void>"
		}
		return (*p)[0].SchemeString()
	}
	l := len(*p)
	q := &strings.Builder{}
	q.WriteString("(")
	for i, v := range (*p)[:l-1] {
		if i > 0 {
			q.WriteString(" ")
		}
		if IsVoid(v) {
			q.WriteString("#<void>")
			continue
		}
		q.WriteString(v.SchemeString())
	}
	if !IsEmptyList((*p)[l-1]) {
		q.WriteString(" . ")
		if IsVoid((*p)[l-1]) {
			q.WriteString("#<void>")
		} else {
			q.WriteString((*p)[l-1].SchemeString())
		}
	}
	q.WriteString(")")
	return q.String()
}
