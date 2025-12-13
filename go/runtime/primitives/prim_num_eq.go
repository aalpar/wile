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

// PrimNumEq implements the = primitive for numeric equality.
// Returns #t if all arguments are numerically equal.
// Variadic: (= x y z ...) checks x=y and y=z and ...
func PrimNumEq(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	prev, ok := o0.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "eq?: expected a number but got %T", o0)
	}
	// Index 1 is the rest list (second arg and beyond)
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	pr, ok := rest.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "eq?: expected a pair but got %T", rest)
	}
	v, err := pr.ForEach(nil, func(_ context.Context, i int, hasNext bool, o values.Value) error {
		curr, ok := o.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "eq?: expected a number but got %T", o)
		}
		// subtraction does not equal zero - means not equal
		if !prev.Subtract(curr).IsZero() {
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
		return values.WrapForeignErrorf(values.ErrNotAList, "eq?: expected a proper list")
	}
	mc.SetValue(values.TrueValue)
	return nil
}
