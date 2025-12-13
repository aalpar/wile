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

// PrimFloorDiv implements the (floor/) primitive.
// Returns floor quotient and remainder.
func PrimFloorDiv(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	o1 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	n0, ok := o0.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "floor/: expected an integer but got %T", o0)
	}
	n1, ok := o1.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "floor/: expected an integer but got %T", o1)
	}
	if n1.Value == 0 {
		return values.NewForeignError("floor/: division by zero")
	}
	q, r := FloorDivide(n0.Value, n1.Value)
	mc.SetValues(values.NewInteger(q), values.NewInteger(r))
	return nil
}
