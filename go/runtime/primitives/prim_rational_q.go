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

// PrimRationalQ implements the (rational?) primitive.
// Returns #t if argument is rational number.
func PrimRationalQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer, *values.Rational:
		mc.SetValue(values.TrueValue)
	case *values.Float:
		// Float is rational if it's finite (not NaN or Inf)
		f := v.Value
		if f != f { // NaN check
			mc.SetValue(values.FalseValue)
		} else if f > 1e308 || f < -1e308 { // Infinity check (approximate)
			mc.SetValue(values.FalseValue)
		} else {
			mc.SetValue(values.TrueValue)
		}
	case *values.Complex:
		if imag(v.Value) == 0 {
			r := real(v.Value)
			if r != r { // NaN check
				mc.SetValue(values.FalseValue)
			} else if r > 1e308 || r < -1e308 {
				mc.SetValue(values.FalseValue)
			} else {
				mc.SetValue(values.TrueValue)
			}
		} else {
			mc.SetValue(values.FalseValue)
		}
	default:
		mc.SetValue(values.FalseValue)
	}
	return nil
}
