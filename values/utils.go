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
	"fmt"
	"strings"

	"github.com/aalpar/wile/werr"
)

// byteCnt is the number of bytes used for generating temporary variable names.
const (
	byteCnt = 128 / 8
)

// formatIndexable builds the Scheme external representation for a
// fixed-size indexable collection.  Format: prefix<elem1> <elem2> ... )
// with elements separated by single spaces and no padding around elements.
func formatIndexable(prefix string, length int, get func(int) Value) string {
	q := &strings.Builder{}
	q.WriteString(prefix)
	if length > 0 {
		q.WriteString(get(0).SchemeString())
		for i := 1; i < length; i++ {
			q.WriteString(" ")
			q.WriteString(get(i).SchemeString())
		}
	}
	q.WriteString(")")
	return q.String()
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
// returns an error, that error is returned unchanged.
//
// This is the canonical proper-list eliminator — every site that walks a
// list and rejects improper tails should funnel through this function so
// the rejection logic is defined exactly once. registry/helpers.ForEachList
// delegates here; new code in any layer should call ForEachProperList
// directly when it cannot import the helpers package (e.g., machine/).
func ForEachProperList(ctx context.Context, t Tuple, name string, fn ForEachFunc) error {
	v, err := t.ForEach(ctx, fn)
	if err != nil {
		return err
	}
	if !IsEmptyList(v) {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "%s: expected a proper list", name)
	}
	return nil
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

// equalPairKey identifies a pair of compound values being compared.
// Go compares interface values in arrays by type and pointer for pointer types,
// so [2]any{pairA, pairB} works as a map key without unsafe.
type equalPairKey [2]Value

// EqualTo compares two values for structural equality.
// Handles nil and void values specially: nil equals nil, void equals void.
// For compound types (Pair, Vector), uses optimistic bisimilarity
// with a visited set to terminate on circular structures per R7RS §6.1.
// This is the same technique used by Chez Scheme and Racket: when a
// (pointer-a, pointer-b) pair is re-encountered during recursion, return true.
// wrapperValueEqualTo compares two optional Value fields for structural equality.
// Used by wrapper types (Box, CompileTimeValue) whose EqualTo delegates to inner values.
func wrapperValueEqualTo(pVal, oVal Value) bool {
	if pVal == oVal {
		return true
	}
	if pVal == nil || oVal == nil {
		return false
	}
	return pVal.EqualTo(oVal)
}

func EqualTo(a, b Value) bool {
	if a == nil || b == nil {
		return a == b
	}
	if a.IsVoid() || b.IsVoid() {
		return a.IsVoid() == b.IsVoid()
	}
	visited := make(map[equalPairKey]bool)
	return equalToDeep(a, b, visited)
}

// equalToDeep dispatches compound types to cycle-aware helpers,
// and delegates everything else to a.EqualTo(b).
//
// Bisimulation equivalence (Milner 1989). equal? on cyclic structures
// is the greatest fixpoint of the structural matching relation.
//
//	R = gfp(F) where F(R) = { (a,b) : structure(a) matches structure(b)
//	                           under R for all sub-components }
//
//	visited : map[equalPairKey]bool implements the coinductive hypothesis.
//	When (ptr(a), ptr(b)) ∈ visited, return true (optimistic assumption).
//	This correctly computes gfp because the greatest fixpoint is the
//	union of all consistent relations.
//
//	Invariant: visited keys are pointer pairs, not structural. Two
//	  distinct objects with identical structure are compared structurally,
//	  not short-circuited by visited.
//	Constrains: pairEqualToDeep, vectorEqualToDeep (must propagate
//	  visited through recursive calls).
//	Constrained by: EqualTo (top-level entry creates the visited map),
//	  Hashable contract (equal values must hash identically — the hash
//	  function cannot depend on pointer identity).
//
// See BIBLIOGRAPHY.md "Bisimulation Equivalence for equal?".
func equalToDeep(a, b Value, visited map[equalPairKey]bool) bool {
	if a == nil || b == nil {
		return a == b
	}
	if a.IsVoid() || b.IsVoid() {
		return a.IsVoid() == b.IsVoid()
	}
	switch pa := a.(type) {
	case *Pair:
		// Must check concrete *Pair type, not Tuple interface
		pb, ok := b.(*Pair)
		if !ok {
			return false
		}
		return pairEqualToDeep(pa, pb, visited)
	case *Vector:
		pb, ok := b.(*Vector)
		if !ok {
			return false
		}
		return vectorEqualToDeep(pa, pb, visited)
	default:
		return a.EqualTo(b)
	}
}

// compareIndexable is a generic helper for comparing indexable collections with
// cycle detection. Used by vectorEqualToDeep and arrayListEqualToDeep.
func compareIndexable[T Value](
	a, b T,
	length func(T) int,
	getElement func(T, int) Value,
	checkVoid func(T, int) bool,
	visited map[equalPairKey]bool,
) bool {
	// Use type-erased pointers as map keys
	aPtr := a
	bPtr := b

	if length(a) != length(b) {
		return false
	}
	key := equalPairKey{aPtr, bPtr}
	if visited[key] {
		return true
	}
	visited[key] = true

	for i := 0; i < length(a); i++ {
		// Handle void elements if applicable
		if checkVoid != nil {
			aVoid := checkVoid(a, i)
			bVoid := checkVoid(b, i)
			if aVoid || bVoid {
				if aVoid && bVoid {
					continue
				}
				return false
			}
		}
		if !equalToDeep(getElement(a, i), getElement(b, i), visited) {
			return false
		}
	}
	return true
}

// pairEqualToDeep compares two Pairs with cycle detection.
// Mirrors the iterative structure of Pair.EqualTo but records visited
// pointer pairs and recurses elements via equalToDeep.
//
// Implementation note: Must use *Pair (not Tuple) for two reasons:
// 1. Cycle detection via pointer identity comparison (p == v, p0 == v0)
// 2. Map keys require concrete type (equalPairKey uses [2]Value with *Pair pointers)
func pairEqualToDeep(p, v *Pair, visited map[equalPairKey]bool) bool {
	if p == v {
		return true
	}
	p0 := p
	v0 := v
	for {
		key := equalPairKey{p0, v0}
		if visited[key] {
			return true
		}
		visited[key] = true

		if !equalToDeep(p0.Car(), v0.Car(), visited) {
			return false
		}
		// nil/void cdr: a pair constructed with nil cdr (instead of EmptyList)
		// is malformed but must be handled. Two nil cdrs are equal; a nil cdr
		// and a non-nil cdr are not.
		if IsVoid(p0.Cdr()) || IsVoid(v0.Cdr()) {
			if IsVoid(p0.Cdr()) && IsVoid(v0.Cdr()) {
				return true
			}
			return p0.Cdr() == v0.Cdr()
		}
		if p0.Cdr() == v0.Cdr() {
			return true
		}
		// Type assertions to *Pair required for iterative traversal with pointer
		// identity comparison for cycle detection.
		pv0, _ := p0.Cdr().(*Pair)
		vv0, _ := v0.Cdr().(*Pair)
		if pv0 == nil || vv0 == nil {
			return equalToDeep(p0.Cdr(), v0.Cdr(), visited)
		}
		p0 = pv0
		v0 = vv0
	}
}

// vectorEqualToDeep compares two Vectors with cycle detection.
func vectorEqualToDeep(p, other *Vector, visited map[equalPairKey]bool) bool {
	if p == nil || other == nil {
		return p == other
	}
	return compareIndexable(
		p, other,
		func(v *Vector) int { return len(*v) },
		func(v *Vector, i int) Value { return (*v)[i] },
		nil, // Vectors don't have void elements
		visited,
	)
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
	return List([]Value(*vs)...)
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
