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
	"math/big"

	"wile/machine"
	"wile/values"
)

// PrimExact implements the (exact) primitive.
// Converts an inexact number to an exact representation.
func PrimExact(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer, *values.Rational:
		mc.SetValue(v)
	case *values.Float:
		// Convert float to rational
		r := new(big.Rat).SetFloat64(v.Value)
		if r == nil {
			return values.NewForeignError("exact: cannot convert infinity or NaN to exact")
		}
		mc.SetValue(values.NewRationalFromRat(r))
	case *values.Complex:
		return values.NewForeignError("exact: complex numbers not supported")
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "exact: expected a number but got %T", o)
	}
	return nil
}
