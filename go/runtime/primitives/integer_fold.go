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

// integerFold is a helper for integer fold operations (gcd, lcm).
// Takes rest args at index 0, applies absolute value, then folds with combiner.
func integerFold(
	mc *machine.MachineContext,
	name string,
	identity int64,
	combiner func(acc, val int64) int64,
) error {
	o := mc.Arg(0)
	pr, ok := o.(*values.Pair)
	if !ok {
		if values.IsEmptyList(o) {
			mc.SetValue(values.NewInteger(identity))
			return nil
		}
		return values.WrapForeignErrorf(values.ErrNotAPair, "%s: expected a list but got %T", name, o)
	}
	if values.IsEmptyList(pr) {
		mc.SetValue(values.NewInteger(identity))
		return nil
	}
	first, ok := pr.Car().(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %T", name, pr.Car())
	}
	result := first.Value
	if result < 0 {
		result = -result
	}
	rest, ok := pr.Cdr().(*values.Pair)
	if !ok {
		mc.SetValue(values.NewInteger(result))
		return nil
	}
	v, err := rest.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, next values.Value) error {
		n, ok := next.(*values.Integer)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %T", name, next)
		}
		val := n.Value
		if val < 0 {
			val = -val
		}
		result = combiner(result, val)
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: not a proper list", name)
	}
	mc.SetValue(values.NewInteger(result))
	return nil
}
