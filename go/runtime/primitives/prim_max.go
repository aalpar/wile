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

// PrimMax implements the max primitive.
// Returns the maximum of the given numbers.
func PrimMax(_ context.Context, mc *machine.MachineContext) error {
	first := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	max, ok := first.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "max: expected a number but got %T", first)
	}
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	pr, ok := rest.(*values.Pair)
	if !ok {
		if values.IsEmptyList(rest) {
			mc.SetValue(max)
			return nil
		}
		return values.WrapForeignErrorf(values.ErrNotAPair, "max: expected a pair but got %T", rest)
	}
	v, err := pr.ForEach(nil, func(i int, hasNext bool, v values.Value) error {
		curr, ok := v.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "max: expected a number but got %T", pr.Car())
		}
		if max.LessThan(curr) {
			max = curr
		}
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "max: not a proper list")
	}
	mc.SetValue(max)
	return nil
}
