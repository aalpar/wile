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
	"math/big"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// PrimOddQ implements the odd? primitive.
//
// R7RS §6.2.6: Returns #t if the integer is odd, #f otherwise.
// Accepts any integer, including inexact integers (e.g., 3.0).
func PrimOddQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(utils.BoolToBoolean(v.Value%2 != 0))
	case *values.BigInteger:
		// Check if the last bit is set (odd)
		mc.SetValue(utils.BoolToBoolean(v.BigInt().Bit(0) == 1))
	case *values.Float:
		// Must be an integer value (no fractional part)
		if math.IsInf(v.Value, 0) || math.IsNaN(v.Value) {
			return values.WrapForeignErrorf(values.ErrNotANumber, "odd?: expected an integer but got %v", v.Value)
		}
		if math.Floor(v.Value) != v.Value {
			return values.WrapForeignErrorf(values.ErrNotANumber, "odd?: expected an integer but got %v", v.Value)
		}
		// Convert to big.Int for reliable odd check on large floats
		bf := new(big.Float).SetFloat64(v.Value)
		bi, _ := bf.Int(nil)
		mc.SetValue(utils.BoolToBoolean(bi.Bit(0) == 1))
	case *values.BigFloat:
		// Must be an integer value
		if !v.BigFloatValue().IsInt() {
			return values.WrapForeignErrorf(values.ErrNotANumber, "odd?: expected an integer but got %v", v.BigFloatValue())
		}
		bi, _ := v.BigFloatValue().Int(nil)
		mc.SetValue(utils.BoolToBoolean(bi.Bit(0) == 1))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "odd?: expected an integer but got %T", o)
	}
	return nil
}
