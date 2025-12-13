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

// PrimSqrt implements the (sqrt) primitive.
// Returns the square root of a number.
func PrimSqrt(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		if v.Value < 0 {
			mc.SetValue(values.NewComplex(cmplx.Sqrt(complex(float64(v.Value), 0))))
		} else {
			mc.SetValue(values.NewFloat(math.Sqrt(float64(v.Value))))
		}
	case *values.Float:
		if v.Value < 0 {
			mc.SetValue(values.NewComplex(cmplx.Sqrt(complex(v.Value, 0))))
		} else {
			mc.SetValue(values.NewFloat(math.Sqrt(v.Value)))
		}
	case *values.Rational:
		f := v.Float64()
		if f < 0 {
			mc.SetValue(values.NewComplex(cmplx.Sqrt(complex(f, 0))))
		} else {
			mc.SetValue(values.NewFloat(math.Sqrt(f)))
		}
	case *values.Complex:
		mc.SetValue(values.NewComplex(cmplx.Sqrt(v.Value)))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "sqrt: expected a number but got %T", o)
	}
	return nil
}
