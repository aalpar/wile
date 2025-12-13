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

// PrimListQ implements the (list?) primitive.
// Returns #t if the argument is a proper list, #f otherwise.
func PrimListQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	if values.IsEmptyList(o) {
		mc.SetValue(values.TrueValue)
		return nil
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(utils.BoolToBoolean(pr.IsList()))
	return nil
}
