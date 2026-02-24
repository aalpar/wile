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

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
)

// ListToVector is a helper that converts a list argument to a vector.
func ListToVector(mc *machine.MachineContext, name string) error {
	o := mc.Arg(0)
	if values.IsEmptyList(o) {
		mc.SetValue(values.NewVector())
		return nil
	}
	pr, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a list but got %T", name, o)
	}
	var elems values.Vector
	v, err := pr.ForEach(mc.Context(), func(_ context.Context, _ int, _ bool, v values.Value) error {
		elems = append(elems, v)
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a proper list", name)
	}
	mc.SetValue(values.NewVector(elems...))
	return nil
}

// CollectVectors extracts a non-empty list of vectors from a rest argument,
// validates that each element is a vector, and returns the minimum length.
// Used by vector-map, vector-for-each, and vector-append.
func CollectVectors(rest values.Value, name string) ([]*values.Vector, int, error) {
	var vectors []*values.Vector
	current := rest
	for !values.IsEmptyList(current) {
		tuple, ok := current.(values.Tuple)
		if !ok {
			return nil, 0, values.WrapForeignErrorf(values.ErrNotAList, "%s: improper argument list", name)
		}
		v, ok := tuple.Car().(*values.Vector)
		if !ok {
			return nil, 0, values.WrapForeignErrorf(values.ErrNotAVector, "%s: expected a vector but got %T", name, tuple.Car())
		}
		vectors = append(vectors, v)
		current = tuple.Cdr()
	}
	if len(vectors) == 0 {
		return nil, 0, nil
	}
	minLen := len(*vectors[0])
	for _, v := range vectors[1:] {
		if len(*v) < minLen {
			minLen = len(*v)
		}
	}
	return vectors, minLen, nil
}

// CollectStrings extracts zero or more strings from a rest argument,
// validates that each element is a string, converts them to rune slices, and returns
// the minimum length of the strings. Callers are responsible for rejecting an empty
// argument list if a non-empty list is required. Used by string-map and string-for-each.
func CollectStrings(rest values.Value, name string) ([]*values.String, [][]rune, int, error) {
	var strs []*values.String
	current := rest
	for !values.IsEmptyList(current) {
		tuple, ok := current.(values.Tuple)
		if !ok {
			return nil, nil, 0, values.WrapForeignErrorf(values.ErrNotAList, "%s: improper argument list", name)
		}
		s, err := RequireType[*values.String](tuple.Car(), values.ErrNotAString, name)
		if err != nil {
			return nil, nil, 0, err
		}
		strs = append(strs, s)
		current = tuple.Cdr()
	}
	if len(strs) == 0 {
		return nil, nil, 0, nil
	}
	runeSlices := make([][]rune, len(strs))
	minLen := -1
	for i, s := range strs {
		runeSlices[i] = s.Runes()
		if minLen < 0 || len(runeSlices[i]) < minLen {
			minLen = len(runeSlices[i])
		}
	}
	return strs, runeSlices, minLen, nil
}

// MemberLookup is a helper for list membership primitives (memq, memv).
// Takes obj at index 0, list at index 1. Uses eq predicate to find match.
// On match, returns the tail of the list starting at the matched element.
func MemberLookup(
	mc *machine.MachineContext,
	name string,
	eq func(a, b values.Value) bool,
) error {
	obj := mc.Arg(0)
	lst := mc.Arg(1)
	for !values.IsEmptyList(lst) {
		pr, ok := lst.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a list but got %T", name, lst)
		}
		if eq(pr.Car(), obj) {
			mc.SetValue(pr)
			return nil
		}
		lst = pr.Cdr()
	}
	mc.SetValue(values.FalseValue)
	return nil
}

// AssocLookup is a helper for alist lookup primitives (assq, assv, assoc).
// Takes key at index 0, alist at index 1. Uses eq predicate to find match.
func AssocLookup(
	mc *machine.MachineContext,
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
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a list but got %T", name, alist)
	}
	v, err := pr.ForEach(mc.Context(), func(_ context.Context, _ int, _ bool, elem values.Value) error {
		entry, ok := elem.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "%s: expected a pair in alist but got %T", name, elem)
		}
		if eq(entry.Car(), obj) {
			mc.SetValue(entry)
			return values.ErrStopIteration
		}
		return nil
	})
	if errors.Is(err, values.ErrStopIteration) {
		return nil
	}
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a proper list", name)
	}
	mc.SetValue(values.FalseValue)
	return nil
}
