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

// PrimEqualQ implements the equal? predicate for structural equality.
// Returns #t if both arguments have the same structure and values.
func PrimEqualQ(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	o1 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	mc.SetValue(utils.BoolToBoolean(values.EqualTo(o0, o1)))
	return nil
}
