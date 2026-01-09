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

	"wile/machine"
	"wile/values"
)

// numericExtremum is a helper for min/max primitives.
// First arg at index 0, rest at index 1. Returns the extremum value
// where isBetter returns true if candidate should replace current.
func numericExtremum(
	mc *machine.MachineContext,
	name string,
	isBetter func(candidate, current values.Number) bool,
) error {
	first := mc.Arg(0)
	best, ok := first.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, first)
	}
	rest := mc.Arg(1)
	pr, ok := rest.(*values.Pair)
	if !ok {
		if values.IsEmptyList(rest) {
			mc.SetValue(best)
			return nil
		}
		return values.WrapForeignErrorf(values.ErrNotAPair, "%s: expected a pair but got %T", name, rest)
	}
	v, err := pr.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, v values.Value) error {
		curr, ok := v.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, v)
		}
		if isBetter(curr, best) {
			best = curr
		}
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: not a proper list", name)
	}
	mc.SetValue(best)
	return nil
}
