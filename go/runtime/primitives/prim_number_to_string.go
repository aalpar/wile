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
	"strconv"

	"wile/machine"
	"wile/values"
)

// PrimNumberToString implements the number->string primitive.
// Converts a number to its string representation with optional radix.
func PrimNumberToString(_ context.Context, mc *machine.MachineContext) error {
	n := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	radix := 10
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(*values.Pair)
		if ok && !values.IsEmptyList(pr) {
			r, ok := pr.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotANumber, "number->string: expected an integer radix but got %T", pr.Car())
			}
			radix = int(r.Value)
			if radix != 2 && radix != 8 && radix != 10 && radix != 16 {
				return values.NewForeignError("number->string: radix must be 2, 8, 10, or 16")
			}
		}
	}
	switch v := n.(type) {
	case *values.Integer:
		mc.SetValue(values.NewString(strconv.FormatInt(v.Value, radix)))
	case *values.Float:
		mc.SetValue(values.NewString(strconv.FormatFloat(v.Value, 'g', -1, 64)))
	case *values.Rational:
		mc.SetValue(values.NewString(v.SchemeString()))
	case *values.Complex:
		mc.SetValue(values.NewString(v.SchemeString()))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "number->string: expected a number but got %T", n)
	}
	return nil
}
