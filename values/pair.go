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

	"github.com/aalpar/wile/werr"
)

var (
	_ Value = (*Pair)(nil)

	// EmptyList is the singleton empty list ().
	// It implements Tuple but is not *Pair, enforcing (pair? '()) -> #f
	// at the type level per R7RS 6.4.
	EmptyList Tuple = emptyListType{}
)

// Pair represents a Scheme cons cell.
//
// Initial algebra (Bird & de Moor 1997, Meijer et al. 1991). Proper
// lists are the initial algebra of a polynomial functor.
//
//	List = μX. 1 + Value × X
//
//	Constructors:
//	  nil  : 1 → List          = EmptyList (emptyListType, not *Pair)
//	  cons : Value × List → List = NewCons(car, cdr)
//
//	Eliminator (catamorphism / fold):
//	  ForEach(f) applies f to each car, returns tail
//
//	Invariant: EmptyList is a separate Go type from *Pair. This encodes
//	  the two constructors as distinct injections: (pair? '()) → #f.
//	Constrains: IsList (must terminate — uses Floyd cycle detection),
//	  PairBlock (batch allocation optimization, does not change the algebra),
//	  all list-processing primitives (must handle both constructors).
//	Constrained by: Tuple interface (read-only view over both constructors).
//
// See BIBLIOGRAPHY.md "Lists as Initial Algebras".
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
// Uses Floyd's cycle detection (tortoise-and-hare) to handle circular lists.
// Returns false for circular lists per R7RS §6.4.
// See BIBLIOGRAPHY.md "Floyd's Cycle Detection".
//
// Implementation note: This method must use *Pair (not Tuple) for cycle
// detection because it requires pointer identity comparison (slow == fast).
// Interfaces cannot be compared by pointer identity.
func (p *Pair) IsList() bool {
	if IsVoid(p) {
		return false
	}
	slow := p
	fast := p
	for {
		// Fast pointer advances two steps
		// Type assertion to *Pair required for pointer identity comparison
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
//
// R7RS §6.4: The resulting list is always newly allocated, except that it shares
// structure with the last argument. This implementation copies the spine of p
// and sets the last cdr to vs.
func (p *Pair) Append(vs Value) Value {
	if !p.IsList() {
		panic(werr.WrapForeignErrorf(werr.ErrNotAList, "Pair.Append: receiver is not a proper list"))
	}
	if IsEmptyList(vs) {
		return p
	}
	if IsVoid(p) {
		return vs
	}

	// Copy the spine of p and append vs
	// R7RS §6.4: all arguments except the last must be newly allocated
	var head, tail *Pair
	q := p
	for !IsEmptyList(q) {
		newPair := NewCons(q.Car(), EmptyList)
		if head == nil {
			head = newPair
			tail = newPair
		} else {
			tail[1] = newPair
			tail = newPair
		}

		cdr := q.Cdr()
		if IsEmptyList(cdr) {
			break
		}
		var ok bool
		q, ok = cdr.(*Pair)
		if !ok {
			panic(werr.WrapForeignErrorf(werr.ErrNotAList, "Pair.Append: improper list during spine copy"))
		}
	}

	// Attach vs to the last copied pair
	if tail != nil {
		tail[1] = vs
	}

	return head
}

// Length returns the length of the list represented by the Pair.
// It panics if the Pair does not represent a proper list.
//
// Note: Uses context.Background() because Length has no cancellation path.
// Circular lists are rejected by Must (improper tail), but a truly pathological
// structure could hang. See TODO.md "context.TODO() in production code".
func (p *Pair) Length() int {
	q := 0
	Must(p.ForEach(context.Background(), func(_ context.Context, i int, _ bool, _ Value) error {
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
		return EmptyList, nil
	}
	pr := p
	i := 0
	for pr != nil {
		hasNext := !IsEmptyList(pr[1])
		err := fn(ctx, i, hasNext, pr[0])
		if err != nil {
			return nil, err
		}
		// Type assertion to *Pair required for iterating through linked structure.
		// We need access to the next *Pair pointer, not just Tuple methods.
		pr0, ok := pr[1].(*Pair)
		if !ok {
			return pr[1], nil
		}
		pr = pr0
		i++
	}
	return EmptyList, nil
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
		panic(werr.WrapForeignErrorf(werr.ErrNotAList, "Must: tail is not empty list"))
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
// Handles circular structures (from datum labels or set-cdr!/set-car!)
// by emitting "..." when a cycle is detected.
func (p *Pair) SchemeString() string {
	if p.IsVoid() {
		return "#<void>"
	}
	visited := make(map[*Pair]bool)
	return p.schemeStringWithVisited(visited)
}

func (p *Pair) schemeStringWithVisited(visited map[*Pair]bool) string {
	if visited[p] {
		return "..."
	}
	visited[p] = true

	q := &strings.Builder{}
	q.WriteString("(")
	pr := p
	i := 0
	for pr != nil {
		if i > 0 {
			q.WriteString(" ")
		}
		car := pr[0]
		if IsVoid(car) {
			q.WriteString("#<void>")
		} else if carPair, ok := car.(*Pair); ok && carPair != nil {
			q.WriteString(carPair.schemeStringWithVisited(visited))
		} else {
			q.WriteString(car.SchemeString())
		}
		cdrPair, ok := pr[1].(*Pair)
		if !ok || cdrPair == nil {
			// Non-pair cdr, nil *Pair (void), or empty list
			if !IsEmptyList(pr[1]) {
				q.WriteString(" . ")
				if IsVoid(pr[1]) {
					q.WriteString("#<void>")
				} else {
					q.WriteString(pr[1].SchemeString())
				}
			}
			break
		}
		if visited[cdrPair] {
			q.WriteString(" . ...")
			break
		}
		visited[cdrPair] = true
		pr = cdrPair
		i++
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
// Handles circular structures (from datum labels or set-cdr!/set-car!)
// by emitting "..." when a cycle is detected.
func (p *Pair) String() string {
	if p.IsVoid() {
		return ""
	}
	visited := make(map[*Pair]bool)
	return p.stringWithVisited(visited)
}

func (p *Pair) stringWithVisited(visited map[*Pair]bool) string {
	if visited[p] {
		return "..."
	}
	visited[p] = true

	q := &strings.Builder{}
	q.WriteString("(")
	pr := p
	i := 0
	for pr != nil {
		if i > 0 {
			q.WriteString(" ")
		}
		car := pr[0]
		if carPair, ok := car.(*Pair); ok && carPair != nil {
			q.WriteString(carPair.stringWithVisited(visited))
		} else {
			q.WriteString(stringValue(car))
		}
		cdrPair, ok := pr[1].(*Pair)
		if !ok || cdrPair == nil {
			if !IsEmptyList(pr[1]) {
				q.WriteString(" . ")
				q.WriteString(stringValue(pr[1]))
			}
			break
		}
		if visited[cdrPair] {
			q.WriteString(" . ...")
			break
		}
		visited[cdrPair] = true
		pr = cdrPair
		i++
	}
	q.WriteString(")")
	return q.String()
}

// AsVector converts the Pair representing a proper list into a Vector.
// It panics if the Pair does not represent a proper list.
//
// Note: Uses context.Background() — see Length comment for rationale.
func (p *Pair) AsVector() *Vector {
	if p.IsVoid() {
		return nil
	}
	vs := []Value{}
	Must(p.ForEach(context.Background(), func(_ context.Context, _ int, _ bool, v Value) error {
		vs = append(vs, v)
		return nil
	}))
	return NewVector(vs...)
}
