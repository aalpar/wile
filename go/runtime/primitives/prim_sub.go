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

// PrimSub implements the - primitive.
// With one argument returns its negation: (- x) => -x
// With multiple arguments subtracts from left to right: (- x y z) => x - y - z
func PrimSub(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	nbr0, ok := o0.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "sub: expected a number but got %T", o0)
	}
	o1 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	pr, ok := o1.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "sub: expected a pair but got %T", o1)
	}
	if values.IsEmptyList(pr) {
		mc.SetValue(values.NewInteger(0).Subtract(nbr0))
		return nil
	}
	o2 := pr.Car()
	nbr2, ok := o2.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "sub: expected a number but got %T", o2)
	}
	nbr2 = nbr0.Subtract(nbr2)
	pr, ok = pr.Cdr().(*values.Pair)
	if !ok {
		mc.SetValue(nbr2)
		return nil
	}
	v, err := pr.ForEach(nil, func(_ context.Context, i int, hasNext bool, o values.Value) error {
		v, ok := o.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "sub: expected a number but got %T", o)
		}
		nbr2 = nbr2.Subtract(v)
		return nil
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "sub: error processing pair for - operation: %s", pr.SchemeString())
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "sub: expected a list but got %s", v.SchemeString())
	}
	mc.SetValue(nbr2)
	return nil
}
