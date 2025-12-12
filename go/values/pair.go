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
	"fmt"
	"strings"
)

var (
	_ Value = (*Pair)(nil)
	_ Tuple = (*Pair)(nil)

	// FIXME: consider using types for EmptyList and Void
	EmptyList = NewCons(nil, nil)
)

type Pair [2]Value

// NewCons creates a new Pair with the given car and cdr Values.
func NewCons(car, cdr Value) *Pair {
	q := &Pair{car, cdr}
	return q
}

// Datum returns the underlying data of the Pair as a [2]Value array.
func (p *Pair) Datum() [2]Value {
	return [2]Value{p[0], p[1]}
}

// Car returns the car of the Pair.
func (p *Pair) Car() Value {
	return p[0]
}

// Cdr returns the cdr of the Pair.
func (p *Pair) Cdr() Value {
	return p[1]
}

// SetCar sets the car of the Pair to the given Value v.
func (p *Pair) SetCar(v Value) {
	p[0] = v
}

// SetCdr sets the cdr of the Pair to the given Value v.
func (p *Pair) SetCdr(v Value) {
	p[1] = v
}

// IsList checks if the Pair represents a proper list.
func (p *Pair) IsList() bool {
	pr := p
	if IsVoid(pr) {
		return false
	}
	v, _ := p.ForEach(nil, func(_ context.Context, _ int, hasNext bool, _ Value) error {
		return nil
	})
	return IsEmptyList(v)
}

// Append appends the given Value vs to the end of the list represented by the Pair.
// It panics if the Pair does not represent a proper list.
func (p *Pair) Append(vs Value) Value {
	if !p.IsList() {
		panic(ErrNotAList)
	}
	if IsEmptyList(vs) {
		if p.IsVoid() || p.IsEmptyList() {
			return p
		}
		return p
	}
	if IsEmptyList(p) || IsVoid(p) {
		return vs
	}
	q := p
	for !IsVoid(q) && !IsEmptyList(q.Cdr()) {
		ok := false
		q, ok = q.Cdr().(*Pair)
		if !ok {
			break
		}
	}
	if q.IsVoid() {
		panic(ErrNotAList)
	}
	q[1] = vs
	return p
}

// Length returns the length of the list represented by the Pair.
// It panics if the Pair does not represent a proper list.
func (p *Pair) Length() int {
	q := 0
	r, _ := p.ForEach(nil, func(_ context.Context, i int, _ bool, _ Value) error {
		q = i + 1
		return nil
	})
	if r != EmptyList {
		panic(ErrNotAList)
	}
	return q
}

// IsEmptyList checks if the Pair represents an empty list.
func (p *Pair) IsEmptyList() bool {
	if p == nil {
		return false
	}
	if p == EmptyList {
		return true
	}
	return p[0] == nil && p[1] == nil
}

// ForEach iterates over each element in the list represented by the Pair.
// The provided function fn is called for each element with the index i,
// a boolean hasNext indicating if there are more elements, and the value v.
// If fn returns an error, the iteration stops and the error is returned.
// If the list ends with a non-empty cdr, that cdr is returned as the second return value.
func (p *Pair) ForEach(ctx context.Context, fn ForEachFunc) (Value, error) {
	if p == nil {
		return Void, nil
	}
	ok := false
	pr := p
	pr0 := p
	i := 0
	for pr != nil && !pr.IsEmptyList() {
		hasNext := !IsEmptyList(pr[1])
		err := fn(ctx, i, hasNext, pr[0])
		if err != nil {
			return nil, err
		}
		pr0, ok = pr[1].(*Pair)
		if !ok {
			return pr[1], nil
		}
		pr = pr0
		i++
	}
	return pr, nil
}

// EqualTo checks if the Pair is equal to another Value o.
func (p *Pair) EqualTo(o Value) bool {
	v, ok := o.(*Pair)
	if !ok {
		return false
	}
	if p == v {
		return true
	}
	p0 := p
	v0 := v
	for !p0.IsEmptyList() && !v0.IsEmptyList() {
		if !EqualTo(p0[0], v0[0]) {
			return false
		}
		// FIXME: consider using types for EmptyList and Void.  ugly void logic everywhere
		if IsVoid(p0[1]) || IsVoid(v0[1]) {
			if IsVoid(p0[1]) && IsVoid(v0[1]) {
				return true
			}
			return p0[1] == v0[1]
		}
		if p0[1] == v0[1] {
			break
		}
		pv0, _ := p0[1].(*Pair)
		vv0, _ := v0[1].(*Pair)
		if pv0 == nil || vv0 == nil {
			return p0[1].EqualTo(v0[1])
		}
		p0 = pv0
		v0 = vv0
	}
	if p0.IsEmptyList() != v0.IsEmptyList() {
		return false
	}
	return true
}

// IsVoid checks if the Pair is void (nil).
func (p *Pair) IsVoid() bool {
	return p == nil
}

// SchemeString returns the Scheme representation of the Pair.
func (p *Pair) SchemeString() string {
	if p.IsVoid() {
		return "#<void>"
	}
	if p.IsEmptyList() {
		return "()"
	}
	q := &strings.Builder{}
	q.WriteString("(")
	cdr, _ := p.ForEach(nil, func(_ context.Context, i int, _ bool, v Value) error {
		if i > 0 {
			q.WriteString(" ")
		}
		q.WriteString(v.SchemeString())
		return nil
	})
	if !IsEmptyList(cdr) {
		q.WriteString(" . ")
		q.WriteString(cdr.SchemeString())
	}
	q.WriteString(")")
	return q.String()
}

func stringValue(o Value) string {
	q := ""
	strnr, ok := o.(fmt.Stringer)
	if ok {
		q = strnr.String()
	} else if o != nil {
		q = o.SchemeString()
	} else {
		q = "#<void>"
	}
	return q
}

// String returns the string representation of the Pair.
func (p *Pair) String() string {
	if p.IsVoid() {
		return ""
	}
	if p.IsEmptyList() {
		return "()"
	}
	q := &strings.Builder{}
	q.WriteString("(")
	cdr, _ := p.ForEach(nil, func(ctx context.Context, i int, hasNext bool, v Value) error {
		if i > 0 {
			q.WriteString(" ")
		}
		q.WriteString(stringValue(v))
		return nil
	})
	if !IsEmptyList(cdr) {
		q.WriteString(" . ")
		q.WriteString(stringValue(cdr))
	}
	q.WriteString(")")
	return q.String()
}

// AsVector converts the Pair representing a proper list into a Vector.
// It panics if the Pair does not represent a proper list.
func (p *Pair) AsVector() *Vector {
	if p.IsVoid() {
		return nil
	}
	if p.IsEmptyList() {
		return NewVector()
	}
	vs := []Value{}
	cdr, _ := p.ForEach(nil, func(_ context.Context, _ int, _ bool, v Value) error {
		vs = append(vs, v)
		return nil
	})
	if !IsEmptyList(cdr) {
		panic(ErrNotAList)
	}
	return NewVector(vs...)
}
