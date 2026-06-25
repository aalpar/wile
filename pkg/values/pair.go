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
	"iter"
	"strings"

	"github.com/aalpar/wile/pkg/werr"
)

var (
	_ Value = (*Pair)(nil)

	// EmptyList is the singleton empty list ().
	// It implements Tuple but is not *Pair, enforcing (pair? '()) -> #f
	// at the type level per R7RS 6.4.
	//
	// It also satisfies SyntaxValue and SyntaxTuple — the empty list has
	// no symbols, scopes, or source-attachable hygiene content, so the
	// value-level singleton serves as the syntax-level singleton too
	// (matching Chez's `(equal? (syntax ()) '()) → #t`). For callers that
	// need the SyntaxTuple-typed view (e.g. so a SyntaxValue-returning
	// function can return the empty list directly), use SyntaxEmptyList
	// below — it refers to the same singleton.
	//
	// EmptyList is statically typed as Tuple (not SyntaxTuple) because the
	// common pattern `list := EmptyList; list = NewCons(...)` builds a
	// value-level list via type inference, and *Pair only implements
	// Tuple. Promoting EmptyList to SyntaxTuple would break that pattern.
	EmptyList Tuple = emptyListType{}

	// SyntaxEmptyList is the empty-list singleton typed as SyntaxTuple,
	// for use in contexts that build syntax-level lists or return
	// SyntaxValue. It is the same struct value as EmptyList; the package
	// `internal/syntax` re-exports this name for backward compatibility.
	SyntaxEmptyList SyntaxTuple = emptyListType{}
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

// Spine yields each *Pair along p's cdr chain. If the list terminates in
// EmptyList, *improperTail is set to EmptyList. If it terminates in a
// non-list cdr (improper list), *improperTail is set to that value.
// improperTail may be nil if the caller does not care about the tail.
//
// Spine is the catamorphism for the initial list algebra
//
//	List = μX. 1 + Value × X
//
// and is the irreducible spine-walk consumed by Pair.IsList, Length,
// AsVector, EqualTo, and SchemeString. It does NOT detect cycles —
// for cyclic input, use SpineWithCycleCheck.
//
// The yielded value pair (*Pair, struct{}) uses struct{} so consumers
// can write either `for cell := range Spine(p, &tail)` (preferred) or
// `for cell, _ := range Spine(p, &tail)`.
func Spine(p *Pair, improperTail *Value) iter.Seq2[*Pair, struct{}] {
	return func(yield func(*Pair, struct{}) bool) {
		pr := p
		for pr != nil {
			if !yield(pr, struct{}{}) {
				return
			}
			cdr := pr[1]
			if IsEmptyList(cdr) {
				if improperTail != nil {
					*improperTail = EmptyList
				}
				return
			}
			next, ok := cdr.(*Pair)
			if !ok {
				if improperTail != nil {
					*improperTail = cdr
				}
				return
			}
			pr = next
		}
		if improperTail != nil {
			*improperTail = EmptyList
		}
	}
}

// SpineWithCycleCheck is Spine with Floyd's tortoise-and-hare cycle
// detection. *cycled is set to true if a cycle is detected, false
// otherwise. cycled may be nil if the caller does not care.
//
// The iterator yields every cell up to (but not necessarily including)
// the point of cycle detection, and yields every cell of a proper or
// improper list before terminating. It does NOT report the improper
// tail — Floyd's algorithm cannot distinguish improper-tail termination
// from cycle detection in a single pass without an extra O(n) visited
// set. Callers that need both should use Spine with an external visited
// map.
func SpineWithCycleCheck(p *Pair, cycled *bool) iter.Seq2[*Pair, struct{}] {
	return func(yield func(*Pair, struct{}) bool) {
		if cycled != nil {
			*cycled = false
		}
		if p == nil {
			return
		}
		slow, fast := p, p
		for slow != nil {
			if !yield(slow, struct{}{}) {
				return
			}
			// Try to advance fast two cdr-steps; if either step
			// terminates, fastAdvanced is false and we will not test
			// for a cycle this round — but slow still keeps walking
			// so the iteration finishes with the remaining proper or
			// improper tail.
			fastAdvanced := false
			fastNext1, ok := fast[1].(*Pair)
			if ok {
				fast = fastNext1
				fastNext2, ok2 := fast[1].(*Pair)
				if ok2 {
					fast = fastNext2
					fastAdvanced = true
				}
			}
			// Advance slow one cdr-step. If slow's cdr is not a *Pair,
			// the list (proper or improper) is exhausted from slow's
			// side and we are done.
			slowNext, ok := slow[1].(*Pair)
			if !ok {
				return
			}
			slow = slowNext
			if fastAdvanced && slow == fast {
				if cycled != nil {
					*cycled = true
				}
				return
			}
		}
	}
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
// Consumes SpineWithCycleCheck: a cycle short-circuits the spine, and
// the final cell's cdr is checked for EmptyList to distinguish proper
// from improper termination.
func (p *Pair) IsList() bool {
	if IsVoid(p) {
		return false
	}
	var cycled bool
	var last *Pair
	for cell := range SpineWithCycleCheck(p, &cycled) {
		last = cell
	}
	if cycled {
		return false
	}
	return IsEmptyList(last.Cdr())
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
// Consumes Spine. Callers must ensure the receiver is a proper list
// (e.g., via IsList) — a circular list will hang indefinitely because
// Spine does not detect cycles.
func (p *Pair) Length() int {
	var tail Value
	count := 0
	for range Spine(p, &tail) {
		count++
	}
	if !IsEmptyList(tail) {
		panic(werr.WrapForeignErrorf(werr.ErrNotAList,
			"Pair.Length: improper list"))
	}
	return count
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
//
// Stays open-coded rather than consuming Spine: a Spine-consuming
// variant was measured ~40–56% slower across 10/100/1000-element
// lists (BenchmarkPairForEach in pair_bench_test.go) because each
// iter.Seq2 yield goes through two function pointers, and ForEach is
// hot enough that the per-step overhead dominates. The C.3/C.4 spine
// consumers (IsList, Length, AsVector) are called far less often per
// list, so their regression is invisible.
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
	visited := make(map[Value]bool)
	return p.schemeStringWithVisited(visited)
}

func (p *Pair) schemeStringWithVisited(visited map[Value]bool) string {
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
		// schemeStringChild dispatches nested *Pair/*Vector through the shared
		// visited set, catching pair↔vector cross-cycles, and renders void/nil
		// as "#<void>".
		q.WriteString(schemeStringChild(pr[0], visited))
		cdrPair, ok := pr[1].(*Pair)
		if !ok || cdrPair == nil {
			// Non-pair cdr, nil *Pair (void), or empty list
			if !IsEmptyList(pr[1]) {
				q.WriteString(" . ")
				q.WriteString(schemeStringChild(pr[1], visited))
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
// Consumes Spine. See Length for the circular-list caveat.
func (p *Pair) AsVector() *Vector {
	if p.IsVoid() {
		return nil
	}
	var tail Value
	vs := []Value{}
	for cell := range Spine(p, &tail) {
		vs = append(vs, cell.Car())
	}
	if !IsEmptyList(tail) {
		panic(werr.WrapForeignErrorf(werr.ErrNotAList,
			"Pair.AsVector: improper list"))
	}
	return NewVector(vs...)
}
