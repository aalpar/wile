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

package helpers

import (
	"context"
	"errors"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// ForEachList calls fn on each element of t and returns ErrNotAList if the tail
// is not an empty list (i.e., t is an improper list). If fn returns an error,
// that error is returned unchanged.
//
// Delegates to values.ForEachProperList so the proper-list rejection logic
// is defined in exactly one place. Callers that cannot import this package
// (e.g., machine/) should call values.ForEachProperList directly — they are
// equivalent.
func ForEachList(ctx context.Context, t values.Tuple, name string, fn func(context.Context, int, bool, values.Value) error) error {
	return values.ForEachProperList(ctx, t, name, fn)
}

// Uncons is the helper-package alias for values.Uncons. See that function
// for semantics. machine/ and other layers that cannot import helpers
// should call values.Uncons directly — they are equivalent.
func Uncons(v values.Value, name, role string) (values.Value, values.Value, error) {
	return values.Uncons(v, name, role)
}

// UnconsTyped is the helper-package alias for values.UnconsTyped.
func UnconsTyped[T any](v values.Value, headSentinel error, name, role string) (T, values.Value, error) {
	return values.UnconsTyped[T](v, headSentinel, name, role)
}

// CarAs is the helper-package alias for values.CarAs.
func CarAs[T any](t values.Tuple, headSentinel error, name, role string) (T, error) {
	return values.CarAs[T](t, headSentinel, name, role)
}

// NthCons is the helper-package alias for values.NthCons.
func NthCons(lst values.Value, n int64, name string) (values.Value, error) {
	return values.NthCons(lst, n, name)
}

// ListToVector is a helper that converts a list argument to a vector.
func ListToVector(mc machine.CallContext, name string) error {
	o := mc.Arg(0)
	if values.IsEmptyList(o) {
		mc.SetValue(values.NewVector())
		return nil
	}
	pr, ok := o.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "%s: expected a list but got %T", name, o)
	}
	var elems []values.Value
	err := ForEachList(mc.Context(), pr, name, func(_ context.Context, _ int, _ bool, v values.Value) error {
		elems = append(elems, v)
		return nil
	})
	if err != nil {
		return err
	}
	mc.SetValue(values.NewVector(elems...))
	return nil
}

// CollectVectors extracts a list of vectors from a rest argument, validates that
// each element is a vector, and returns the minimum length. An empty rest list
// returns (nil, 0, nil); the caller decides whether that is an error.
// Used by vector-append (prim_vectors.go).
func CollectVectors(rest values.Value, name string) ([]*values.Vector, int, error) {
	var vectors []*values.Vector
	current := rest
	for !values.IsEmptyList(current) {
		tuple, ok := current.(values.Tuple)
		if !ok {
			return nil, 0, werr.WrapForeignErrorf(werr.ErrNotAList, "%s: improper argument list", name)
		}
		v, ok := tuple.Car().(*values.Vector)
		if !ok {
			return nil, 0, werr.WrapForeignErrorf(werr.ErrNotAVector, "%s: expected a vector but got %T", name, tuple.Car())
		}
		vectors = append(vectors, v)
		current = tuple.Cdr()
	}
	if len(vectors) == 0 {
		return nil, 0, nil
	}
	minLen := vectors[0].Length()
	for _, v := range vectors[1:] {
		if v.Length() < minLen {
			minLen = v.Length()
		}
	}
	return vectors, minLen, nil
}

// MemberLookup is a helper for list membership primitives (memq, memv).
// Takes obj at index 0, list at index 1. Uses eq predicate to find match.
// On match, returns the tail of the list starting at the matched element.
//
// The walk goes through ForEachList for the two properties a hand-rolled cdr
// loop cannot have: the amortized context poll, so an embedder deadline and a
// REPL interrupt are observed, and Brent cycle detection, so a circular list
// terminates instead of pinning a core. WithMaxCallDepth is inapplicable here —
// nothing recurses — which left the context as the only lever.
//
// ForEachList hands the element, never the cell, so the cell the element came
// from is tracked in cursor: MemberLookup must return the sublist starting at
// the match. AssocLookup needs no cursor only because its result IS the element.
func MemberLookup(
	mc machine.CallContext,
	name string,
	eq func(a, b values.Value) bool,
) error {
	obj := mc.Arg(0)
	lst := mc.Arg(1)
	if values.IsEmptyList(lst) {
		mc.SetValue(values.FalseValue)
		return nil
	}
	cursor, ok := lst.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "%s: expected a list but got %T", name, lst)
	}
	err := ForEachList(mc.Context(), cursor, name, func(_ context.Context, _ int, _ bool, elem values.Value) error {
		if eq(elem, obj) {
			mc.SetValue(cursor)
			return werr.ErrStopIteration
		}
		next, nextOk := cursor.Cdr().(values.Tuple)
		if nextOk {
			cursor = next
		}
		return nil
	})
	if errors.Is(err, werr.ErrStopIteration) {
		return nil
	}
	if err != nil {
		return err
	}
	mc.SetValue(values.FalseValue)
	return nil
}

// AssocLookup is a helper for alist lookup primitives (assq, assv, assoc).
// Takes key at index 0, alist at index 1. Uses eq predicate to find match.
func AssocLookup(
	mc machine.CallContext,
	name string,
	eq func(a, b values.Value) bool,
) error {
	obj := mc.Arg(0)
	alist := mc.Arg(1)
	if values.IsEmptyList(alist) {
		mc.SetValue(values.FalseValue)
		return nil
	}
	pr, ok := alist.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "%s: expected a list but got %T", name, alist)
	}
	err := ForEachList(mc.Context(), pr, name, func(_ context.Context, _ int, _ bool, elem values.Value) error {
		entry, ok := elem.(values.Tuple)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAPair, "%s: expected a pair in alist but got %T", name, elem)
		}
		// The empty list satisfies values.Tuple, so the assertion above admits it
		// and entry.Car() would panic — an argument-domain fault ('(()) is outside
		// R7RS §6.3's "list of pairs") raised through the host boundary as an
		// uncatchable internal error. Reject it here so it reads as the domain
		// error it is, which is also the answer the Scheme-level sibling assoc
		// already gives on the same input.
		if values.IsEmptyList(elem) {
			return werr.WrapForeignErrorf(werr.ErrNotAPair, "%s: expected a pair in alist but got the empty list", name)
		}
		if eq(entry.Car(), obj) {
			mc.SetValue(entry)
			return werr.ErrStopIteration
		}
		return nil
	})
	if errors.Is(err, werr.ErrStopIteration) {
		return nil
	}
	if err != nil {
		return err
	}
	mc.SetValue(values.FalseValue)
	return nil
}
