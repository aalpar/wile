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

// PrimIntegerQ implements the integer? predicate.
// Returns #t if the argument is an integer (exact or inexact).
// Inexact integers are floating-point numbers with zero fractional part.
func PrimIntegerQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(values.TrueValue)
	case *values.Rational:
		if v.IsInteger() {
			mc.SetValue(values.TrueValue)
		} else {
			mc.SetValue(values.FalseValue)
		}
	case *values.Float:
		f := v.Value
		if f == float64(int64(f)) {
			mc.SetValue(values.TrueValue)
		} else {
			mc.SetValue(values.FalseValue)
		}
	case *values.Complex:
		if imag(v.Value) == 0 {
			r := real(v.Value)
			if r == float64(int64(r)) {
				mc.SetValue(values.TrueValue)
			} else {
				mc.SetValue(values.FalseValue)
			}
		} else {
			mc.SetValue(values.FalseValue)
		}
	default:
		mc.SetValue(values.FalseValue)
	}
	return nil
}
