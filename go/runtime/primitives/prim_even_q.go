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

// PrimEvenQ implements the (even?) primitive.
// Returns #t if the integer is even.
func PrimEvenQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, ok := o.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "even?: expected an integer but got %T", o)
	}
	mc.SetValue(utils.BoolToBoolean(v.Value%2 == 0))
	return nil
}
