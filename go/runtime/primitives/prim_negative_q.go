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
	"wile/utils"
	"wile/values"
)

// PrimNegativeQ implements the negative? primitive.
//
// R7RS §6.2.6: Returns #t if the real number is negative.
func PrimNegativeQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(utils.BoolToBoolean(v.Value < 0))
	case *values.BigInteger:
		mc.SetValue(utils.BoolToBoolean(v.IsNegative()))
	case *values.BigFloat:
		mc.SetValue(utils.BoolToBoolean(v.IsNegative()))
	case *values.Float:
		mc.SetValue(utils.BoolToBoolean(v.Value < 0))
	case *values.Rational:
		mc.SetValue(utils.BoolToBoolean(v.Rat().Sign() < 0))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "negative?: expected a real number but got %T", o)
	}
	return nil
}
