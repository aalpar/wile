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

// PrimMakeRectangular implements make-rectangular.
// Creates a complex number from real and imaginary parts: x+yi
func PrimMakeRectangular(_ context.Context, mc *machine.MachineContext) error {
	r := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	i := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	var realPart, imagPart float64
	switch v := r.(type) {
	case *values.Integer:
		realPart = float64(v.Value)
	case *values.Float:
		realPart = v.Value
	case *values.Rational:
		realPart = v.Float64()
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "make-rectangular: expected a real number but got %T", r)
	}
	switch v := i.(type) {
	case *values.Integer:
		imagPart = float64(v.Value)
	case *values.Float:
		imagPart = v.Value
	case *values.Rational:
		imagPart = v.Float64()
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "make-rectangular: expected a real number but got %T", i)
	}
	mc.SetValue(values.NewComplexFromParts(realPart, imagPart))
	return nil
}
