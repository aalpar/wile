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

// PrimInfiniteQ implements the (infinite?) primitive.
//
// R7RS §6.2.6: Returns #t if the number is infinite.
func PrimInfiniteQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Integer, *values.BigInteger, *values.BigFloat, *values.Rational:
		// Exact numbers and BigFloat are never infinite
		mc.SetValue(values.FalseValue)
	case *values.Float:
		mc.SetValue(utils.BoolToBoolean(math.IsInf(v.Value, 0)))
	case *values.Complex:
		real := real(v.Value)
		imag := imag(v.Value)
		isInfinite := math.IsInf(real, 0) || math.IsInf(imag, 0)
		mc.SetValue(utils.BoolToBoolean(isInfinite))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "infinite?: expected a number but got %T", o)
	}
	return nil
}
