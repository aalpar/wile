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

// PrimRealPart implements the (real-part) primitive.
// Returns real part of complex number.
func PrimRealPart(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Complex:
		mc.SetValue(values.NewFloat(real(v.Value)))
	case *values.Integer:
		mc.SetValue(values.NewFloat(float64(v.Value)))
	case *values.Float:
		mc.SetValue(v)
	case *values.Rational:
		mc.SetValue(values.NewFloat(v.Float64()))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "real-part: expected a number but got %T", o)
	}
	return nil
}
