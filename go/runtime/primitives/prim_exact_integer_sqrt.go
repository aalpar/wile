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

// PrimExactIntegerSqrt implements the (exact-integer-sqrt) primitive.
// Returns the integer square root and remainder as two values.
func PrimExactIntegerSqrt(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, ok := o.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "exact-integer-sqrt: expected an exact integer but got %T", o)
	}
	if v.Value < 0 {
		return values.NewForeignError("exact-integer-sqrt: expected a non-negative integer")
	}
	s := int64(math.Sqrt(float64(v.Value)))
	// Correct for potential floating point errors
	for s*s > v.Value {
		s--
	}
	for (s+1)*(s+1) <= v.Value {
		s++
	}
	r := v.Value - s*s
	mc.SetValues(values.NewInteger(s), values.NewInteger(r))
	return nil
}
