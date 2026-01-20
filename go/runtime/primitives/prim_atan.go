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

// PrimAtan implements the (atan z) and (atan y x) primitives.
// One-argument form: Returns the arc tangent of z. Accepts all numeric types per R7RS.
// Two-argument form: Returns the angle in radians from the positive x-axis to (x, y).
// The two-argument form only accepts real numbers (per R7RS).
func PrimAtan(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	rest := mc.Arg(1)

	if rest == values.EmptyList {
		// One-argument form: complex atan
		z, err := ToComplex128(o)
		if err != nil {
			return values.WrapForeignErrorf(err, "atan: %v", err)
		}
		mc.SetValue(ComplexOrFloat(cmplx.Atan(z)))
	} else {
		// Two-argument form: atan2(y, x) - only accepts real numbers
		y, err := ToFloat64(o)
		if err != nil {
			return values.WrapForeignErrorf(err, "atan: %v", err)
		}
		xArg, ok := rest.(*values.Pair)
		if !ok {
			return values.NewForeignError("atan: expected a list for rest arguments")
		}
		x, err := ToFloat64(xArg.Car())
		if err != nil {
			return values.WrapForeignErrorf(err, "atan: %v", err)
		}
		mc.SetValue(values.NewFloat(math.Atan2(y, x)))
	}
	return nil
}
