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

	"github.com/aalpar/wile/go/machine"
	"github.com/aalpar/wile/go/values"
)

// ListToVector is a helper that converts a list argument to a vector.
func ListToVector(mc *machine.MachineContext, name string) error {
	o := mc.Arg(0)
	if values.IsEmptyList(o) {
		mc.SetValue(values.NewVector())
		return nil
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a list but got %T", name, o)
	}
	var elems values.Vector
	v, err := pr.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, v values.Value) error {
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

// AssocLookup is a helper for alist lookup primitives (assq, assv, assoc).
// Takes key at index 0, alist at index 1. Uses eq predicate to find match.
func AssocLookup(
	mc *machine.MachineContext,
	name string,
	eq func(a, b values.Value) bool,
) error {
	obj := mc.Arg(0)
	alist := mc.Arg(1)
	pr, ok := alist.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a list but got %T", name, alist)
	}
	v, err := pr.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, elem values.Value) error {
		entry, ok := elem.(*values.Pair)
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
