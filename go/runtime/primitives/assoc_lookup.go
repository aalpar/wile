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

package primitives

import (
	"context"
	"errors"

	"wile/machine"
	"wile/values"
)

// assocLookup is a helper for alist lookup primitives (assq, assv, assoc).
// Takes key at index 0, alist at index 1. Uses eq predicate to find match.
func assocLookup(
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
