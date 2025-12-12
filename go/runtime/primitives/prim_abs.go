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
	"wile/values"
)

// PrimAbs implements the abs primitive.
// Returns the absolute value of a real number.
func PrimAbs(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		if v.Value < 0 {
			mc.SetValue(values.NewInteger(-v.Value))
		} else {
			mc.SetValue(v)
		}
	case *values.Float:
		mc.SetValue(values.NewFloat(math.Abs(v.Value)))
	case *values.Rational:
		result := new(big.Rat).Abs(v.Rat())
		mc.SetValue(values.NewRationalFromRat(result))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "abs: expected a real number but got %T", o)
	}
	return nil
}
