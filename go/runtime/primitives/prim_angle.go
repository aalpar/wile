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

// PrimAngle implements the angle primitive.
// Returns the angle of a complex number in polar form.
func PrimAngle(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Complex:
		mc.SetValue(values.NewFloat(cmplx.Phase(v.Value)))
	case *values.Integer:
		if v.Value >= 0 {
			mc.SetValue(values.NewFloat(0))
		} else {
			mc.SetValue(values.NewFloat(math.Pi))
		}
	case *values.Float:
		if v.Value >= 0 {
			mc.SetValue(values.NewFloat(0))
		} else {
			mc.SetValue(values.NewFloat(math.Pi))
		}
	case *values.Rational:
		if v.Rat().Sign() >= 0 {
			mc.SetValue(values.NewFloat(0))
		} else {
			mc.SetValue(values.NewFloat(math.Pi))
		}
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "angle: expected a number but got %T", o)
	}
	return nil
}
