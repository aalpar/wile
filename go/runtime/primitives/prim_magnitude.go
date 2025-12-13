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
	"math/cmplx"

	"wile/machine"
	"wile/values"
)

// PrimMagnitude implements the (magnitude) primitive.
// Returns the magnitude (absolute value) of a complex number.
func PrimMagnitude(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Complex:
		mc.SetValue(values.NewFloat(cmplx.Abs(v.Value)))
	case *values.Integer:
		val := v.Value
		if val < 0 {
			val = -val
		}
		mc.SetValue(values.NewFloat(float64(val)))
	case *values.Float:
		mc.SetValue(values.NewFloat(math.Abs(v.Value)))
	case *values.Rational:
		mc.SetValue(values.NewFloat(math.Abs(v.Float64())))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "magnitude: expected a number but got %T", o)
	}
	return nil
}
