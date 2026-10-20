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
	"errors"
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
	// SyntaxValue. It is the same struct value as EmptyList; the public
	// package pkg/syntax re-exports this name.
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

// SpineEnd reports why a spine walk stopped. It travels with the LAST cell the
// walk yields; every earlier cell carries the zero value.
//
// The zero value therefore means "this is not the last cell" — and it is also
// exactly what a consumer that breaks out mid-walk observes, which is the
// truthful answer: no terminator was reached, so none is reported. That is the
// whole reason the termination facts ride on the yield rather than on an
// out-parameter. A `*Value` written only at natural termination cannot answer
// an abandoned walk at all, and its zero value (nil, i.e. void) is
// indistinguishable from a real answer; consumers papered over it with an ad-hoc
// "did I break?" flag, or silently rendered #<void> as the tail.
//
// There is no "cyclic" case here because Spine does not detect cycles and
// nothing else walks a spine generically: IsList, the only caller that needs
// Floyd, open-codes it (see there for why).
type SpineEnd struct {
	// Tail is the terminating cdr: EmptyList for a proper list, the trailing
	// atom for an improper one. nil while the walk is still in progress.
	Tail Value
}

// Proper reports whether the walk ran to a proper-list terminator.
func (e SpineEnd) Proper() bool {
	return e.Tail != nil && IsEmptyList(e.Tail)
}

// Improper reports whether the walk ran to a terminator that is NOT the empty
// list — i.e. whether Tail names a value the caller must still account for.
// False for a proper list and an abandoned walk alike, which is what lets a
// consumer write `if end.Improper()` without a separate "did I finish?" guard.
func (e SpineEnd) Improper() bool {
	return e.Tail != nil && !IsEmptyList(e.Tail)
}

// Spine yields each *Pair along p's cdr chain, paired with a SpineEnd that is
// zero on every cell but the last and carries the terminating cdr on that one.
//
// Spine is the catamorphism for the initial list algebra
//
//	List = μX. 1 + Value × X
//
// and is the irreducible spine-walk consumed by Pair.IsList, Length,
// AsVector, EqualTo, and SchemeString. It does NOT detect cycles — it will spin
// forever on a circular list. For cyclic input, use SpineChecked.
func Spine(p *Pair) iter.Seq2[*Pair, SpineEnd] {
	return func(yield func(*Pair, SpineEnd) bool) {
		pr := p
		for pr != nil {
			// The terminator is decided BEFORE the yield, which is what lets it
			// travel with the cell that owns it. A consumer that breaks here has
			// simply not seen it.
			next, end := spineStep(pr[1])
			if next == nil {
				yield(pr, end)
				return
			}
			if !yield(pr, SpineEnd{}) {
				return
			}
			pr = next
		}
	}
}

// spineStep classifies one cell's cdr for both spine walkers: it returns the
// next cell to walk, or nil plus the terminating SpineEnd when there is none.
//
// A cdr that is not a live *Pair ends the spine and IS the tail. Both spellings
// of void — an untyped nil and a (*Pair)(nil) — collapse through ValueOrVoid to
// the Void singleton or to themselves, so Tail is never nil once a terminator is
// reached, which is what keeps "nil Tail" unambiguously meaning "walk not
// finished".
//
// Reporting void as a tail rather than as EmptyList is what makes the walkers
// agree with the printers: the out-parameter implementation this replaced let a
// (*Pair)(nil) cdr fall out of its `for pr != nil` loop and reported EmptyList,
// while SchemeString's hand-rolled walk called the same cdr an improper tail and
// printed " . #<void>" — the behaviour TestPair_SchemeString pins.
func spineStep(cdr Value) (*Pair, SpineEnd) {
	next, ok := cdr.(*Pair)
	if ok && next != nil {
		return next, SpineEnd{}
	}
	return nil, SpineEnd{Tail: ValueOrVoid(cdr)}
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
// Floyd is open-coded here rather than offered as a second spine walker, and
// that is a decision with two independent reasons. Cheapness: routing it through
// an iter.Seq2 whose second element carries a terminator cost a measured
// +11.5% at 10 and 100 elements against the struct{}-yielding predecessor,
// because the wider yield is paid per cell while the answer is wanted once.
// Safety: a shared cycle-detecting walker has to report "cyclic" through
// something, and the out-parameter form it used to have (`cycled *bool`)
// carried exactly the staleness SpineEnd was introduced to remove — an
// abandoned walk read back false, a silent false negative. With one caller,
// inlining removes the hazard instead of re-encoding it.
func (p *Pair) IsList() bool {
	if IsVoid(p) {
		return false
	}
	slow, fast := p, p
	for {
		// Advance fast two cdr-steps where it can; if either step leaves the
		// spine, no cycle test happens this round but slow keeps walking to its
		// own terminator.
		fastAdvanced := false
		fastNext, ok := fast[1].(*Pair)
		if ok && fastNext != nil {
			fast = fastNext
			fastNext, ok = fast[1].(*Pair)
			if ok && fastNext != nil {
				fast = fastNext
				fastAdvanced = true
			}
		}
		// The cdr is classified with a bare assertion rather than through
		// spineStep: this loop wants one bit, and spineStep's (*Pair, SpineEnd)
		// return is three words paid per cell. Measured at +18-22% on 10- and
		// 100-element lists when it was routed through the shared classifier.
		slowNext, ok := slow[1].(*Pair)
		if !ok || slowNext == nil {
			return IsEmptyList(slow[1])
		}
		slow = slowNext
		if fastAdvanced && slow == fast {
			return false
		}
	}
}

// Length returns the length of the list represented by the Pair.
// It panics if the Pair does not represent a proper list.
//
// Consumes Spine. Callers must ensure the receiver is a proper list
// (e.g., via IsList) — a circular list will hang indefinitely because
// Spine does not detect cycles.
func (p *Pair) Length() int {
	// A void pair yields no cells, so there is no yield for a SpineEnd to ride
	// out on and end would stay zero — the one case an out-parameter could
	// answer that a yield-carried terminator cannot. Length's answer for void is
	// a convenience of this method (IsList calls void not-a-list, AsVector
	// returns nil), so it belongs here rather than synthesized by the walker.
	if p.IsVoid() {
		return 0
	}
	count := 0
	var end SpineEnd
	for _, e := range Spine(p) {
		count++
		end = e
	}
	if !end.Proper() {
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

// contextCheckMask gates how often Pair.ForEach polls ctx.Done() while walking a
// list. A non-blocking check is cheap but not free; polling every 1024 elements
// eliminates ~99.9% of them while keeping cancellation latency bounded. Power of
// two so the test is a single AND. Mirrors machine.contextCheckMask, which gates
// the VM dispatch loop for the same reason.
const contextCheckMask = 1023

// ForEach iterates over each element in the list represented by the Pair.
// The provided function fn is called for each element with the index i,
// a boolean hasNext indicating if there are more elements, and the value v.
// If fn returns an error, the iteration stops and the error is returned.
// If the list ends with a non-empty cdr, that cdr is returned as the first
// return value; a proper list returns EmptyList.
//
// Two further error returns: ctx.Err() when the embedder's context is cancelled,
// and a wrapped werr.ErrCircularList when Brent's cycle detection fires. Unlike
// Length and AsVector, ForEach terminates on circular input.
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
	// Brent's cycle detection: one pointer comparison per step, plus a
	// power-of-two teleport of the checkpoint. No allocation, and fewer
	// comparisons than Floyd's two-pointer walk. Without it, a circular cdr
	// chain spins here forever — and because this is *the* list walker
	// (ForEachProperList, length, list-copy, append, reverse, and apply's
	// argument spread all funnel through it), that unbounded walk is what let
	// (apply + circular-list) grow the eval stack past every configured limit.
	checkpoint := pr
	power := 1
	steps := 0
	for pr != nil {
		// The context poll is INDEPENDENT of cycle detection: a proper list of
		// 10^9 elements is legal, finite, and must still respect the embedder's
		// deadline. ForEach accepted a ctx and never read it, which is the whole
		// of why apply ignored ctx cancellation, maxStackSize, and maxCallDepth.
		// Amortized every 1024 elements, matching the VM loop's contextCheckMask.
		if i&contextCheckMask == 0 && i != 0 {
			err := ctx.Err()
			if err != nil {
				return nil, err
			}
		}

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

		steps++
		if pr == checkpoint {
			return nil, werr.WrapForeignErrorf(werr.ErrCircularList,
				"Pair.ForEach: circular list detected at element %d", i)
		}
		if steps == power {
			checkpoint = pr
			power *= 2
			steps = 0
		}
	}
	return EmptyList, nil
}

// EqualTo checks if the Pair is equal to another Value o.
// Delegates to Equal, which owns the iterative traversal and terminates on
// circular lists.
func (p *Pair) EqualTo(o Value) bool {
	return Equal(p, o)
}

// EqualComponents pushes the two pairs' cars and cdrs for Equal to compare.
//
// The cdr is pushed BEFORE the car so that the worklist, which pops last-in
// first, drains the car's subtree before walking on down the spine. Pushing car
// first would queue one pending entry per spine element, making a flat list cost
// O(n) auxiliary space instead of O(1).
func (p *Pair) EqualComponents(o Value, push func(a, b Value)) bool {
	v, ok := o.(*Pair)
	if !ok {
		return false
	}
	if p == nil || v == nil {
		return p == v
	}
	push(p.Cdr(), v.Cdr())
	push(p.Car(), v.Car())
	return true
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
	visited := NewMapSet[Value](0)
	return p.schemeStringWithVisited(visited, 1)
}

// schemeStringWithVisited renders the pair using PATH-SCOPED cycle detection:
// every node this call marks (the entry pair plus each spine cdrPair) is
// recorded and unmarked on exit, so a node reachable by two SIBLING paths (an
// acyclic DAG / structural sharing) is rendered in full at each occurrence.
// Only a node reachable from ITSELF — still on the current path — collapses to
// "...". Children recurse via schemeStringChild, which applies the same
// mark/unmark discipline to nested *Pair/*Vector, catching pair↔vector cycles.
//
// depth is this pair's nesting level (root = 1). The cdr-spine is walked
// iteratively, so a flat list of any length stays at one level and does not
// consume depth or the Go stack; only descent into a car or the improper tail
// — genuine nesting — passes depth+1 to schemeStringChild, where the
// host-safety bound is enforced.
func (p *Pair) schemeStringWithVisited(visited MapSet[Value], depth int) string {
	seen := visited.ContainsOne(p)
	if seen {
		return "..."
	}
	// marked accumulates every node this call adds to the path so we can remove
	// all of them on exit — not just the entry pair, but every spine cdrPair.
	marked := []Value{p}
	visited.Set(p)
	defer func() {
		for _, m := range marked {
			visited.Unset(m)
		}
	}()

	q := &strings.Builder{}
	q.WriteString("(")
	// Open-coded rather than consuming Spine, for the reason Pair.ForEach and
	// syntax.walkSyntaxSpine already give: each yield goes through two function
	// pointers. Routing this walk through Spine was measured at +20-23% across
	// 10/100/1000-element lists, interleaved, for no behavioural gain — the
	// terminator here is consumed one line after it is found, so the staleness
	// SpineEnd exists to prevent cannot arise.
	pr := p
	i := 0
	for pr != nil {
		if i > 0 {
			q.WriteString(" ")
		}
		// schemeStringChild dispatches nested *Pair/*Vector through the shared
		// visited set, catching pair↔vector cross-cycles, and renders void/nil
		// as "#<void>".
		q.WriteString(schemeStringChild(pr[0], visited, depth+1))
		cdrPair, ok := pr[1].(*Pair)
		if !ok || cdrPair == nil {
			// Non-pair cdr, nil *Pair (void), or empty list. The improper tail
			// sits at the same nesting level as the cars (depth+1).
			if !IsEmptyList(pr[1]) {
				q.WriteString(" . ")
				q.WriteString(schemeStringChild(pr[1], visited, depth+1))
			}
			break
		}
		seen = visited.ContainsOne(cdrPair)
		if seen {
			q.WriteString(" . ...")
			break
		}
		visited.Set(cdrPair)
		marked = append(marked, cdrPair)
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
	visited := NewMapSet[*Pair](0)
	return p.stringWithVisited(visited, 1)
}

// stringWithVisited is the Stringer twin of schemeStringWithVisited. It keeps a
// pair-only visited set (MapSet[*Pair]): vector children are rendered via
// stringValue → Vector.SchemeString, which carries its own shared visited set,
// so a pair↔vector cross-cycle still terminates (the vector's set catches the
// re-entry) even though this set tracks pairs only. The two paths deliberately
// diverge on map type for now; Phase 3 unifies both onto path-scoped marking.
//
// depth is this pair's nesting level (root = 1), mirroring schemeStringWithVisited's
// host-safety bound: the cdr-spine is iterative (a flat list of any length stays
// at one level), and only descent into a car pair — genuine nesting — recurses,
// degrading to deepMarker at DefaultMaxWriteDepth rather than overflowing the Go
// stack. Non-pair cars route through stringValue → SchemeString, itself bounded.
func (p *Pair) stringWithVisited(visited MapSet[*Pair], depth int) string {
	seen := visited.ContainsOne(p)
	if seen {
		return "..."
	}
	visited.Set(p)

	q := &strings.Builder{}
	q.WriteString("(")
	// Open-coded for the same measured reason as schemeStringWithVisited above.
	pr := p
	i := 0
	for pr != nil {
		if i > 0 {
			q.WriteString(" ")
		}
		car := pr[0]
		carPair, ok := car.(*Pair)
		switch {
		case ok && carPair != nil && depth+1 > DefaultMaxWriteDepth:
			q.WriteString(deepMarker)
		case ok && carPair != nil:
			q.WriteString(carPair.stringWithVisited(visited, depth+1))
		default:
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
		seen = visited.ContainsOne(cdrPair)
		if seen {
			q.WriteString(" . ...")
			break
		}
		visited.Set(cdrPair)
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
	vs := []Value{}
	var end SpineEnd
	for cell, e := range Spine(p) {
		vs = append(vs, cell.Car())
		end = e
	}
	if !end.Proper() {
		panic(werr.WrapForeignErrorf(werr.ErrNotAList,
			"Pair.AsVector: improper list"))
	}
	return NewVector(vs...)
}

// List constructs a proper list from the given values.
// Returns EmptyList if no arguments are provided.
// The resulting list has the values in the same order as the arguments.
//
// Implementation note: Block-allocates all Pair cells in a single slice and links
// them via cdr pointers. Callers receive the Tuple interface.
func List(os ...Value) Tuple {
	if len(os) == 0 {
		return EmptyList
	}
	return PairBlock(make([]Pair, len(os))).LinkWith(os)
}

// ForEach iterates over a Tuple value, calling fn for each element.
// If the value is not a Tuple, returns the value unchanged with no error.
// The callback receives the element index, whether more elements follow, and the element value.
// Returns the tail of the tuple (EmptyList for proper lists) and any error from the callback.
func ForEach(ctx context.Context, o Value, fn ForEachFunc) (Value, error) {
	pr, ok := o.(Tuple)
	if ok {
		return pr.ForEach(ctx, fn)
	}
	return o, nil
}

// ForEachProperList calls fn on each element of t and returns ErrNotAList
// if the tail is not the empty list (i.e., t is an improper list). If fn
// returns an error, that error is returned unchanged, except that any error
// matching werr.ErrCircularList is rewrapped as ErrNotAList (with the cycle
// sentinel chained as its cause), since a circular list is an improper list.
//
// This is the canonical proper-list eliminator — every site that walks a
// list and rejects improper tails should funnel through this function so
// the rejection logic is defined exactly once. registry/helpers.ForEachList
// delegates here; new code in any layer should call ForEachProperList
// directly when it cannot import the helpers package (e.g., machine/).
func ForEachProperList(ctx context.Context, t Tuple, name string, fn ForEachFunc) error {
	v, err := t.ForEach(ctx, fn)
	if err != nil {
		// A circular list IS an improper list, and rejecting improper lists is
		// already this function's job (R7RS §6.10 requires apply's last argument to
		// be a proper list). Report the error callers already handle, chaining the
		// cycle sentinel as the cause so errors.Is(err, werr.ErrCircularList) still
		// resolves for anyone who wants to distinguish it.
		if errors.Is(err, werr.ErrCircularList) {
			return werr.WrapForeignErrorWithCause(werr.ErrNotAList, err,
				"%s: expected a proper list", name)
		}
		return err
	}
	if !IsEmptyList(v) {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "%s: expected a proper list", name)
	}
	return nil
}

// CollectList walks a proper list and returns its elements as a Go slice.
// The empty list yields a nil slice and no error; a non-Tuple or an improper
// tail yields a wrapped ErrNotAList naming the caller.
//
// This is the reification twin of List: List folds a slice into a spine,
// CollectList unfolds a spine into a slice. Every site that drains a variadic
// rest-argument into []Value before handing it to SetValues (or to a Go API)
// should funnel through here — the pre-existing hand-rolled copies each
// re-derived the improper-tail rejection and none of them detected cycles,
// which ForEachProperList does for free.
func CollectList(ctx context.Context, v Value, name string) ([]Value, error) {
	t, ok := v.(Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAList,
			"%s: expected a proper list but got %T", name, v)
	}
	if t.IsEmptyList() {
		return nil, nil
	}
	var q []Value
	err := ForEachProperList(ctx, t, name, func(_ context.Context, _ int, _ bool, elem Value) error {
		q = append(q, elem)
		return nil
	})
	if err != nil {
		return nil, err
	}
	return q, nil
}

// Uncons asserts v is a non-empty Tuple and projects (car, cdr).
// On empty list or non-Tuple input, returns a wrapped ErrNotAList with
// the canonical "<name>: <role>" message format. The cdr may be any
// Value — improper lists are accepted here; callers that need a
// proper-list tail should follow up with ForEachProperList.
//
// Uncons is the eliminator for the Tuple algebra: every site that needs
// to peel one element off the front of a list and continue with the
// remainder should funnel through here so the empty-list / non-Tuple
// rejection is defined exactly once. registry/helpers.Uncons delegates here.
func Uncons(v Value, name, role string) (Value, Value, error) {
	if IsEmptyList(v) {
		return nil, nil, werr.WrapForeignErrorf(werr.ErrNotAList,
			"%s: %s: expected a non-empty list", name, role)
	}
	t, ok := v.(Tuple)
	if !ok {
		return nil, nil, werr.WrapForeignErrorf(werr.ErrNotAList,
			"%s: %s: expected a list but got %T", name, role, v)
	}
	return t.Car(), t.Cdr(), nil
}

// UnconsTyped is Uncons followed by a type assertion on the head.
// On head-type mismatch, returns a wrapped headSentinel with the
// expected-type phrase read via werr.TypeNameOf.
func UnconsTyped[T any](v Value, headSentinel error, name, role string) (T, Value, error) {
	var zero T
	head, tail, err := Uncons(v, name, role)
	if err != nil {
		return zero, nil, err
	}
	typed, ok := head.(T)
	if !ok {
		return zero, nil, werr.WrapForeignErrorf(headSentinel,
			"%s: %s: expected %s but got %T",
			name, role, werr.TypeNameOf(headSentinel), head)
	}
	return typed, tail, nil
}

// CarAs asserts t.Car() has concrete type T. Use this when the caller
// already holds a Tuple in hand and only needs a typed head — the tail
// is left implicit. For typed head + tail in one call, use UnconsTyped.
func CarAs[T any](t Tuple, headSentinel error, name, role string) (T, error) {
	var zero T
	head := t.Car()
	typed, ok := head.(T)
	if !ok {
		return zero, werr.WrapForeignErrorf(headSentinel,
			"%s: %s: expected %s but got %T",
			name, role, werr.TypeNameOf(headSentinel), head)
	}
	return typed, nil
}

// NthCons advances n cons cells along the cdr chain and returns the
// remaining list (or improper tail). It is the unified primitive
// behind list-ref (NthCons(...).Car()) and list-tail (NthCons(...)).
// Returns ErrIndexOutOfRange if n is negative or exceeds the list length.
//
// At n=0 the input is returned unchanged, including for EmptyList — this
// matches R7RS semantics where (list-tail x 0) is x.
func NthCons(lst Value, n int64, name string) (Value, error) {
	if n < 0 {
		return nil, werr.WrapForeignErrorf(werr.ErrIndexOutOfRange,
			"%s: index must be non-negative", name)
	}
	current := lst
	for i := range n {
		if IsEmptyList(current) {
			return nil, werr.WrapForeignErrorf(werr.ErrIndexOutOfRange,
				"%s: index %d out of bounds at depth %d", name, n, i)
		}
		t, ok := current.(Tuple)
		if !ok {
			return nil, werr.WrapForeignErrorf(werr.ErrIndexOutOfRange,
				"%s: index %d out of bounds: improper tail at depth %d", name, n, i)
		}
		current = t.Cdr()
	}
	return current, nil
}

// IsList returns true if the value is a proper list.
// A proper list is either EmptyList or a chain of pairs ending with EmptyList.
// Returns false for nil, improper lists (dotted pairs), and non-list values.
func IsList(v Value) bool {
	if v == nil {
		return false
	}
	if IsEmptyList(v) {
		return true
	}
	tuple, ok := v.(Tuple)
	if ok {
		return tuple.IsList()
	}
	return false
}

// IsEmptyList returns true if the value is the empty list.
// Returns false for nil values. For Tuple types, delegates to their IsEmptyList method.
func IsEmptyList(v Value) bool {
	if v == nil {
		return false
	}
	pr, ok := v.(Tuple)
	if ok {
		return pr.IsEmptyList()
	}
	return false
}

// Single returns the sole element of a single-element Tuple, or false if
// the Tuple has zero or more than one element. This avoids ForEach and its
// closure allocation for the common case of 1-element rest-arg lists.
func Single(t Tuple) (Value, bool) {
	if t.IsEmptyList() {
		return nil, false
	}
	if IsEmptyList(t.Cdr()) {
		return t.Car(), true
	}
	return nil, false
}

// VectorToList converts a Vector to a proper list preserving element order.
// Returns EmptyList for nil or void vectors.
func VectorToList(vs *Vector) Tuple {
	if IsVoid(vs) {
		return EmptyList
	}
	return List(vs.Elems()...)
}
