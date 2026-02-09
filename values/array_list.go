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

	// ArrayListEmptyList is the canonical empty ArrayList. It uses the
	// two-nil encoding [nil, nil], which IsEmptyList() recognizes as ().
	ArrayListEmptyList = NewArrayList(nil, nil)
)

// ArrayList is an array-backed representation of Scheme lists. It stores list
// elements and a terminator in a contiguous []Value slice, providing O(1)
// element access and better cache locality than the equivalent Pair chain.
//
// Internal layout — the last element is the terminator (CDR of the final
// cons cell in the equivalent Pair chain):
//
//	Proper list (1 2 3):      [1, 2, 3, EmptyList]
//	                           ^^^elements^^^  ^^^terminator
//	Improper list (1 2 . 3):  [1, 2, 3]
//	                           ^^elems^^  ^^last=cdr
//	Empty list ():             [EmptyList]  or  [nil, nil]
//	Void (no value):           nil  |  []  |  [Void]
//
// Length() returns len(slice)-1 for proper lists (element count, excluding
// the terminator).
//
// ArrayList implements Value and Tuple. It is interconvertible with Pair
// chains via AsList() and with Vectors via AsVector().
//
// R7RS §6.4: Pairs and lists.
type ArrayList []Value

// Datum returns the raw underlying slice, including the terminator element.
func (p *ArrayList) Datum() []Value {
	return *p
}

// AsVector converts the ArrayList to a Vector containing all elements
// including the terminator. Returns nil if the receiver is void.
func (p *ArrayList) AsVector() *Vector {
	if p.IsVoid() {
		return nil
	}
	q := NewVector()
	*q = make([]Value, len(*p))
	copy(*q, *p)
	return q
}

// NewArrayList creates a new ArrayList from the given values. The caller is
// responsible for including the terminator: pass EmptyList as the final
// element for a proper list, or omit it for an improper list.
//
//	NewArrayList(Int(1), Int(2), EmptyList)  →  (1 2)
//	NewArrayList(Int(1), Int(2), Int(3))     →  (1 2 . 3)
//	NewArrayList(EmptyList)                  →  ()
func NewArrayList(vs ...Value) *ArrayList {
	q := (ArrayList)(slices.Clone(vs))
	return &q
}

// Append inserts a value before the terminator, extending the list by one
// element. Returns a new ArrayList; does not mutate the receiver. Panics if
// the receiver is void (not a list).
func (p *ArrayList) Append(vs Value) Value {
	if IsVoid(p) {
		panic(ErrNotAList)
	}
	if IsEmptyList(p) {
		return NewArrayList(vs)
	}
	q := p.Copy()
	switch {
	case IsVoid(vs):
		*q = append(*q, Void)
	case IsEmptyList(vs):
		*q = append(*q, EmptyList)
	default:
		*q = append(*q, vs)
	}
	return q
}

// AppendList concatenates another list (ArrayList or Pair chain) onto this
// list. Returns a new ArrayList; does not mutate either operand.
// Panics with ErrNotAList if the receiver is void, or if the operand is not
// a recognized list type and the receiver is not the empty list. When the
// receiver is the empty list and the operand is not a list, returns a new
// (improper) ArrayList containing the operand.
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
		v0 := vs
		for {
			if v0.IsVoid() {
				panic(ErrNotAList)
			}
			*q = append(*q, v0.Car())
			next, ok := v0.Cdr().(*Pair)
			if !ok {
				break
			}
			v0 = next
		}
	default:
		if !IsEmptyList(p) {
			panic(ErrNotAList)
		}
		return NewArrayList(vs)
	}
	return q
}

// Car returns the first element. Panics if the receiver is nil or the slice is empty.
func (p *ArrayList) Car() Value {
	return (*p)[0]
}

// Cdr returns a new ArrayList sharing the underlying storage from index 1
// onward (sub-slice, no copy). The terminator is preserved.
func (p *ArrayList) Cdr() Value {
	q := (*p)[1:]
	return &q
}

// IsList returns true if the last element is EmptyList (proper list) or if
// the list matches an empty-list encoding. Returns false for nil, empty
// slices, and improper lists.
func (p *ArrayList) IsList() bool {
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

// Length returns the number of list elements, excluding the terminator.
// Panics with ErrNotAList if the receiver is not a proper list.
func (p *ArrayList) Length() int {
	if !p.IsList() {
		panic(ErrNotAList)
	}
	if IsEmptyList(p) {
		return 0
	}
	return len(*p) - 1
}

// IsEmptyList returns true if this ArrayList encodes the empty list ().
// Two representations are recognized: [EmptyList] and any two-element slice
// of void values (for example [nil, nil] or [Void, Void]).
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

// IsVoid returns true if this ArrayList represents the absence of a value.
// Three representations are recognized: nil receiver, empty slice, and
// single-element [Void].
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

// ForEach iterates over every element in the slice, including the terminator.
// The callback receives the index, whether the element is non-terminal
// (i < len-1), and the element value. Returns EmptyList on success.
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

// AsList converts this ArrayList to a Pair (cons cell) chain. Returns nil if
// void. The terminator element becomes the CDR of the final Pair — EmptyList
// for proper lists, any other value for improper lists.
func (p *ArrayList) AsList() Value {
	if IsVoid(p) {
		return nil
	}
	q := NewCons(nil, EmptyList)
	current := q
	l := len(*p)
	for i := 0; i < l-2; i++ {
		current.SetCar((*p)[i])
		current.SetCdr(NewCons(nil, EmptyList))
		current = current.Cdr().(*Pair)
	}
	current.SetCar((*p)[l-2])
	end := (*p)[l-1]
	if IsEmptyList(end) {
		return q
	}
	current.SetCdr(end)
	return q
}

// EqualTo returns true if the other value is an *ArrayList with the same
// length and element-wise structural equality (via Value.EqualTo).
// Void elements are compared by void-ness, not identity.
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

// SchemeString returns the Scheme external representation. Proper lists print
// as (a b c), improper lists as (a b . c), empty lists as (), and void
// values as #<void>. Single-element lists without a terminator print as the
// element's SchemeString directly (unwrapped).
func (p *ArrayList) SchemeString() string {
	if p == nil || len(*p) == 0 {
		return "#<void>"
	}
	if p.IsEmptyList() {
		return "()"
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
