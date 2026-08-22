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
	"crypto/rand"
	"encoding/base32"
	"errors"
	"fmt"
	"strings"

	"github.com/aalpar/wile/pkg/werr"
)

// byteCnt is the number of bytes used for generating temporary variable names.
const (
	byteCnt = 128 / 8
)

// deepMarker is emitted by SchemeString when a value nests deeper than
// DefaultMaxWriteDepth. Unlike the writer (which returns an error and refuses
// to emit unreadable output), SchemeString satisfies the Value interface's
// SchemeString() string contract and cannot raise, so it degrades to this
// marker instead of overflowing the host Go stack. It is intentionally
// distinct from the cycle marker "..." so a truncated-because-deep subtree is
// distinguishable from a truncated-because-cyclic one in debugging output.
// The nesting bound reuses DefaultMaxWriteDepth (the single host-safe nesting
// number, counted root = 1, +1 per container descent, matching the writer and
// readSyntax); like every SchemeString marker it is not intended to re-read.
const deepMarker = "#<deep>"

// formatIndexable builds the Scheme external representation for a
// fixed-size indexable collection.  Format: prefix<elem1> <elem2> ... )
// with elements separated by single spaces and no padding around elements.
//
// visited threads the cycle-detection set into nested Pair/Vector elements
// so a self-referential or cross-referential structure renders a bounded
// "..." marker instead of overflowing the Go stack. Callers whose elements
// can never be compound (e.g. ByteVector, whose elements are bytes) may pass
// nil; schemeStringChild only writes to the set for *Pair/*Vector children.
//
// depth is the nesting level of the *elements* (the container's own level + 1),
// threaded into schemeStringChild so a deeply nested acyclic structure is
// bounded at DefaultMaxWriteDepth. Every element is a sibling at the same level,
// so length does not consume depth: a flat collection of any size renders in
// full (only genuine nesting counts, matching the writer).
func formatIndexable(prefix string, length int, get func(int) Value, visited MapSet[Value], depth int) string {
	q := &strings.Builder{}
	q.WriteString(prefix)
	if length > 0 {
		q.WriteString(schemeStringChild(get(0), visited, depth))
		for i := 1; i < length; i++ {
			q.WriteString(" ")
			q.WriteString(schemeStringChild(get(i), visited, depth))
		}
	}
	q.WriteString(")")
	return q.String()
}

// schemeStringChild renders a single element of a compound structure,
// threading the shared visited set through nested Pair/Vector/Hashtable/Box/
// AtomicBox so that cross-collection cycles (vector→pair→vector,
// pair→hashtable→pair, and the like) are bounded. Marking is path-scoped: each
// compound renderer unmarks itself on exit, so structural sharing (an acyclic
// DAG) renders in full and only a true self-cycle collapses to "...".
// Non-compound elements render via SchemeString; void and nil pointers render
// as "#<void>".
//
// The visited set is keyed on pointer-identity Value types only (*Pair,
// *Vector, *Hashtable, *Box, *AtomicBox — the only types inserted). Never
// insert a value-equality or non-comparable Value: the former would alias
// distinct nodes to one slot (false cycles), the latter would panic on map
// insertion.
//
// A nil visited set means "no cycle tracking requested" — used by callers
// whose elements can never be compound (e.g. ByteVector, whose elements are
// bytes). Should a compound child be encountered anyway, a fresh set is
// allocated lazily for that subtree, so a nil set can never cause a nil-map
// write. This keeps the nil contract a safe "no tracking" rather than an
// unchecked "I promise no compound children" caller obligation.
func schemeStringChild(child Value, visited MapSet[Value], depth int) string {
	// Host-safety bound: this is the single chokepoint through which every
	// compound descent flows (Pair car / improper cdr, Vector element,
	// Hashtable key/value), so one guard here bounds all container types.
	// depth is this child's nesting level; beyond the bound we degrade to
	// deepMarker rather than recurse another Go frame. Guarding before the
	// type switch also uniformly caps leaves at the same level (a leaf can
	// only reach this depth nested under >DefaultMaxWriteDepth containers,
	// which are being truncated anyway).
	if depth > DefaultMaxWriteDepth {
		return deepMarker
	}
	switch c := child.(type) {
	case *Pair:
		if c == nil {
			return "#<void>"
		}
		if visited == nil {
			visited = NewMapSet[Value](0)
		}
		return c.schemeStringWithVisited(visited, depth)
	case *Vector:
		if c == nil {
			return "#<void>"
		}
		if visited == nil {
			visited = NewMapSet[Value](0)
		}
		return c.schemeStringWithVisited(visited, depth)
	case *Hashtable:
		if c == nil {
			return "#<void>"
		}
		if visited == nil {
			visited = NewMapSet[Value](0)
		}
		return c.schemeStringWithVisited(visited, depth)
	case *Box:
		if c == nil {
			return "#<box:void>"
		}
		if visited == nil {
			visited = NewMapSet[Value](0)
		}
		return c.schemeStringWithVisited(visited, depth)
	case *AtomicBox:
		if c == nil {
			return "#<atomic:void>"
		}
		if visited == nil {
			visited = NewMapSet[Value](0)
		}
		return c.schemeStringWithVisited(visited, depth)
	}
	if IsVoid(child) {
		return "#<void>"
	}
	return child.SchemeString()
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

// EqualTo compares two values for structural equality (R7RS equal?).
// It is the package-level spelling of Equal; see equal.go for the traversal.
func EqualTo(a, b Value) bool {
	return Equal(a, b)
}

// EqIdentity implements R7RS eq? semantics: pointer identity for all types
// except symbols, which compare by name/key (R7RS §6.1, §6.5). It is the single
// source of truth for eq?-identity — machine (the VM's promoted OpEqQ and
// continuation-mark scans) and registry/helpers both route through it. Living in
// values/ dissolves the machine↛registry import barrier that previously forced a
// duplicate copy. Kept small and branch-light so it inlines into the hot callers.
func EqIdentity(a, b Value) bool {
	sa, ok := a.(*Symbol)
	if ok {
		sb, ok2 := b.(*Symbol)
		if ok2 {
			return sa.Key == sb.Key
		}
		return false
	}
	return a == b
}

// NewTemporaryVariableName generates a unique symbol for use as a temporary variable.
// The symbol name has the format "__T_<base32-encoded-random-bytes>".
// Uses 128 bits of cryptographic randomness to ensure uniqueness.
// Thread-safe: uses crypto/rand which is safe for concurrent use.
// Panics if random number generation fails.
func NewTemporaryVariableName() *Symbol {
	bs := make([]byte, byteCnt)
	_, err := rand.Read(bs)
	if err != nil {
		panic(werr.WrapForeignErrorf(
			werr.ErrRandomGenerationFailed,
			"error reading random stream: %v",
			err,
		))
	}
	q := NewSymbol(
		fmt.Sprintf("__T_%s", base32.StdEncoding.WithPadding(base32.NoPadding).EncodeToString(bs)),
	)
	return q
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

// IsVoid returns true if the value represents the absence of a meaningful result.
//
// # Void, EmptyList, and nil Semantics
//
// The value system distinguishes three "absence/empty" concepts:
//
//   - Void (voidType{} singleton): no meaningful result (e.g., set!, display).
//     Canonical check: values.IsVoid(v) — handles both nil and the Void singleton.
//
//   - EmptyList (emptyListType{} singleton): the empty list () — a valid first-class
//     Scheme value. Implements Tuple but not *Pair. Canonical check: values.IsEmptyList(v) — handles nil safely (returns false).
//
//   - Go nil (nil interface): uninitialized / absent in Go — not a Scheme value.
//     IsVoid(nil) returns true; IsEmptyList(nil) returns false.
//
// Anti-patterns to avoid:
//
//   - v == values.EmptyList or v != values.EmptyList → use values.IsEmptyList(v)
//   - v == values.Void → use values.IsVoid(v)
//   - v == nil || values.IsVoid(v) → redundant; values.IsVoid(v) handles nil
//
// Note: typed nil pointers (e.g., var p *Pair = nil) are handled by the
// type's IsVoid() method, which checks for nil receiver.
func IsVoid(v Value) bool {
	return v == nil || v.IsVoid()
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

// ExactInteger extracts an exact integer from a Scheme value.
// Returns the int64 value and true if the value is an exact integer that fits in int64.
// Returns 0 and false otherwise.
//
// Accepts:
//   - *Integer: direct int64 value
//   - *BigInteger: if it fits in int64
//   - *Rational: if denominator is 1 and numerator fits in int64
//
// R7RS defines exact integers to include rationals like 2/1 that are mathematically
// integers. Call sites should check for non-negativity if required (e.g., for indexes).
func ExactInteger(v Value) (int64, bool) {
	switch n := v.(type) {
	case *Integer:
		return n.Value, true
	case *BigInteger:
		if n.value.IsInt64() {
			return n.value.Int64(), true
		}
		return 0, false
	case *Rational:
		if n.IsInteger() {
			num := n.Num()
			if num.IsInt64() {
				return num.Int64(), true
			}
		}
		return 0, false
	default:
		return 0, false
	}
}

type MapSet[T comparable] map[T]struct{}

func NewMapSet[T comparable](sz int) MapSet[T] {
	return make(MapSet[T], sz)
}

func (p MapSet[T]) Set(s T) {
	p[s] = struct{}{}
}

func (p MapSet[T]) SetAll(vs ...T) {
	for _, v := range vs {
		p.Set(v)
	}
}

func (p MapSet[T]) Unset(s T) {
	delete(p, s)
}

func (p MapSet[T]) Add(s T) bool {
	_, exists := p[s]
	p[s] = struct{}{}
	return !exists
}

func (p MapSet[T]) ContainsOne(s T) bool {
	_, q := p[s]
	return q
}

// StringSet and IntSet are the two instantiations common enough to have earned
// a name. They are ALIASES, not defined types, which is what makes them free:
// a defined type does not inherit MapSet's method set, so each of the four would
// have to be restated here and kept from drifting. An alias has one method set
// because it is one type.
//
// The cost, stated: aliases are interchangeable with MapSet[string] /
// MapSet[int] everywhere, so a signature naming StringSet documents intent
// without enforcing it, and a type switch cannot have arms for both. Neither is
// load-bearing anywhere in the tree — nothing switches on these, and no
// signature relies on a string set being refused where a MapSet[string] is
// expected. If that changes, the way back is `type StringSet MapSet[string]`
// plus five delegating methods, and the compiler will name every site.

type StringSet = MapSet[string]

func NewStringSet(sz int) StringSet {
	return make(StringSet, sz)
}

type IntSet = MapSet[int]

func NewIntSet(sz int) IntSet {
	return make(IntSet, sz)
}
