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

// numericChainCompare is a helper for numeric chain comparison primitives.
// First arg at index 0, rest at index 1. Returns true if all consecutive
// pairs satisfy the comparator, false otherwise.
func numericChainCompare(
	mc *machine.MachineContext,
	name string,
	fails func(prev, curr values.Number) bool,
) error {
	o0 := mc.Arg(0)
	prev, ok := o0.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, o0)
	}
	rest := mc.Arg(1)
	pr, ok := rest.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "%s: expected a pair but got %T", name, rest)
	}
	v, err := pr.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, v values.Value) error {
		curr, ok := v.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, v)
		}
		if fails(prev, curr) {
			return values.ErrCannotCompare
		}
		prev = curr
		return nil
	})
	if errors.Is(err, values.ErrCannotCompare) {
		mc.SetValue(values.FalseValue)
		return nil
	}
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a proper list", name)
	}
	mc.SetValue(values.TrueValue)
	return nil
}
