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

// PrimGcd implements the (gcd) primitive.
// Returns greatest common divisor.
func PrimGcd(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	pr, ok := o.(*values.Pair)
	if !ok {
		if values.IsEmptyList(o) {
			mc.SetValue(values.NewInteger(0))
			return nil
		}
		return values.WrapForeignErrorf(values.ErrNotAPair, "gcd: expected a list but got %T", o)
	}
	if values.IsEmptyList(pr) {
		mc.SetValue(values.NewInteger(0))
		return nil
	}
	first, ok := pr.Car().(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "gcd: expected an integer but got %T", pr.Car())
	}
	result := first.Value
	if result < 0 {
		result = -result
	}
	rest := pr.Cdr().(*values.Pair)
	rest.ForEach(nil, func(_ context.Context, i int, hasNext bool, next values.Value) error { //nolint:errcheck
		n, ok := next.(*values.Integer)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "gcd: expected an integer but got %T", pr.Car())
		}
		v := n.Value
		if v < 0 {
			v = -v
		}
		result = GcdInt(result, v)
		return nil
	})
	mc.SetValue(values.NewInteger(result))
	return nil
}
