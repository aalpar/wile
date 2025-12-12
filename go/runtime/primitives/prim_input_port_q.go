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

// PrimInputPortQ implements the (input-port?) primitive.
// Returns #t if argument is input port.
func PrimInputPortQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch o.(type) {
	case *values.CharacterInputPort, *values.StringInputPort, *values.BytevectorInputPort, *values.BinaryInputPort:
		mc.SetValue(values.TrueValue)
	default:
		mc.SetValue(values.FalseValue)
	}
	return nil
}
