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
	"wile/values"
)

// PrimCeiling implements the (ceiling) primitive.
// Returns the smallest integer not less than the argument.
func PrimCeiling(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(v)
	case *values.Float:
		mc.SetValue(values.NewFloat(math.Ceil(v.Value)))
	case *values.Rational:
		f := v.Float64()
		mc.SetValue(values.NewFloat(math.Ceil(f)))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "ceiling: expected a real number but got %T", o)
	}
	return nil
}
