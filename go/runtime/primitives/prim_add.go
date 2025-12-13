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

// PrimAdd implements the + primitive.
// With no arguments returns 0. With one argument returns that argument.
// With multiple arguments returns their sum.
func PrimAdd(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "add: expected a pair but got %T", o)
	}
	if values.IsEmptyList(pr) {
		mc.SetValue(values.NewInteger(0))
		return nil
	}
	o = pr.Car()
	nbr, ok := o.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "add: expected a number but got %T", o)
	}
	pr, ok = pr.Cdr().(*values.Pair)
	if !ok {
		mc.SetValue(nbr)
		return nil
	}
	v, err := pr.ForEach(nil, func(_ context.Context, i int, hasNext bool, o values.Value) error {
		v, ok := o.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "add: expected a number but got %T", o)
		}
		nbr = nbr.Add(v)
		return nil
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "add: error processing pair for + operation: %s", pr.SchemeString())
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "add: expected a list but got %s", v.SchemeString())
	}
	mc.SetValue(nbr)
	return nil
}
