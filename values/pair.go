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
	"context"
	"fmt"
	"strings"
)

var (
	_ Value = (*Pair)(nil)

	// EmptyList is the singleton empty list ().
	// It implements Tuple but is not *Pair, enforcing (pair? '()) -> #f
	// at the type level per R7RS 6.4.
	EmptyList Tuple = emptyListType{}
)

// Pair represents a Scheme cons cell.
type Pair [2]Value

// NewCons creates a new Pair with the given car and cdr Values.
func NewCons(car, cdr Value) *Pair {
	q := &Pair{car, cdr}
	return q
}

// Datum returns the underlying data of the Pair as a [2]buf array.
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
// Uses Floyd's cycle detection (tortoise-and-hare) to handle circular lists.
// Returns false for circular lists per R7RS §6.4.
func (p *Pair) IsList() bool {
	if IsVoid(p) {
		return false
	}
	slow := p
	fast := p
	for {
		// Fast pointer advances two steps
		next, ok := fast.Cdr().(*Pair)
		if !ok {
			return IsEmptyList(fast.Cdr())
		}
		fast = next
		if fast.IsEmptyList() {
			return true
		}
		next, ok = fast.Cdr().(*Pair)
		if !ok {
			return IsEmptyList(fast.Cdr())
		}
		fast = next
		if fast.IsEmptyList() {
			return true
		}
		// Slow pointer advances one step
		slow = slow.Cdr().(*Pair)
		// Cycle detected
		if slow == fast {
			return false
		}
	}
}

// Append appends the given Value vs to the end of the list represented by the Pair.
// It panics if the Pair does not represent a proper list.
func (p *Pair) Append(vs Value) Value {
	if !p.IsList() {
		panic(ErrNotAList)
	}
	if IsEmptyList(vs) {
		return p
	}
	if IsVoid(p) {
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

// Len returns the length of the list represented by the Pair.
// It panics if the Pair does not represent a proper list.
func (p *Pair) Length() int {
	q := 0
	Must(p.ForEach(context.TODO(), func(_ context.Context, i int, _ bool, _ Value) error {
		q = i + 1
		return nil
	}))
	return q
}

// IsEmptyList returns false. A *Pair is never the empty list;
// EmptyList is a separate emptyListType value.
func (p *Pair) IsEmptyList() bool {
	return false
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
	pr := p
	i := 0
	for pr != nil {
		hasNext := !IsEmptyList(pr[1])
		err := fn(ctx, i, hasNext, pr[0])
		if err != nil {
			return nil, err
		}
		pr0, ok := pr[1].(*Pair)
		if !ok {
			return pr[1], nil
		}
		pr = pr0
		i++
	}
	return pr, nil
}

// Must panics if err is non-nil or v is not EmptyList.
// Designed for use with ForEach on lists guaranteed to be proper:
//
//	Must(p.ForEach(ctx, func(...) error { ... }))
func Must(v Value, err error) {
	if err != nil {
		panic(err)
	}
	if !IsEmptyList(v) {
		panic(ErrNotAList)
	}
}

// EqualTo checks if the Pair is equal to another Value o.
// Delegates to the cycle-aware pairEqualToDeep to handle circular lists.
func (p *Pair) EqualTo(o Value) bool {
	v, ok := o.(*Pair)
	if !ok {
		return false
	}
	if p == v {
		return true
	}
	return pairEqualToDeep(p, v, make(map[equalPairKey]bool))
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
	q := &strings.Builder{}
	q.WriteString("(")
	cdr, _ := p.ForEach(context.TODO(), func(_ context.Context, i int, _ bool, v Value) error {
		if i > 0 {
			q.WriteString(" ")
		}
		if IsVoid(v) {
			q.WriteString("#<void>")
		} else {
			q.WriteString(v.SchemeString())
		}
		return nil
	})
	if !IsEmptyList(cdr) {
		q.WriteString(" . ")
		if IsVoid(cdr) {
			q.WriteString("#<void>")
		} else {
			q.WriteString(cdr.SchemeString())
		}
	}
	q.WriteString(")")
	return q.String()
}

func stringValue(o Value) string {
	q := ""
	strnr, ok := o.(fmt.Stringer)
	switch {
	case ok:
		q = strnr.String()
	case o != nil:
		q = o.SchemeString()
	default:
		q = "#<void>"
	}
	return q
}

// String returns the string representation of the Pair.
func (p *Pair) String() string {
	if p.IsVoid() {
		return ""
	}
	q := &strings.Builder{}
	q.WriteString("(")
	cdr, _ := p.ForEach(context.TODO(), func(_ context.Context, i int, _ bool, v Value) error {
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
	vs := []Value{}
	Must(p.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, v Value) error {
		vs = append(vs, v)
		return nil
	}))
	return NewVector(vs...)
}
