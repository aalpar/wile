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

// PrimDenominator implements the (denominator) primitive.
// Returns the denominator of a rational number.
func PrimDenominator(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(values.NewInteger(1))
	case *values.Rational:
		denom := v.Denom()
		if denom.IsInt64() {
			mc.SetValue(values.NewInteger(denom.Int64()))
		} else {
			mc.SetValue(values.NewRationalFromBigInt(denom, big.NewInt(1)))
		}
	case *values.Float:
		r := new(big.Rat).SetFloat64(v.Value)
		if r == nil {
			return values.NewForeignError("denominator: cannot get denominator of infinity or NaN")
		}
		denom := r.Denom()
		if denom.IsInt64() {
			mc.SetValue(values.NewInteger(denom.Int64()))
		} else {
			mc.SetValue(values.NewRationalFromBigInt(denom, big.NewInt(1)))
		}
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "denominator: expected a rational number but got %T", o)
	}
	return nil
}
