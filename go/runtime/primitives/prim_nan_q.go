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
	"math"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// PrimNanQ implements the nan? primitive.
// Returns #t if the number is NaN, #f otherwise.
func PrimNanQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(values.FalseValue)
	case *values.Rational:
		mc.SetValue(values.FalseValue)
	case *values.Float:
		mc.SetValue(utils.BoolToBoolean(math.IsNaN(v.Value)))
	case *values.Complex:
		real := real(v.Value)
		imag := imag(v.Value)
		isNaN := math.IsNaN(real) || math.IsNaN(imag)
		mc.SetValue(utils.BoolToBoolean(isNaN))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "nan?: expected a number but got %T", o)
	}
	return nil
}
