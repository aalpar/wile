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

	"wile/environment"
	"wile/machine"
	"wile/values"
)

// PrimNullEnvironment implements the null-environment primitive.
// Returns an empty R5RS environment with no bindings.
func PrimNullEnvironment(_ context.Context, mc *machine.MachineContext) error {
	version := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	versionInt, ok := version.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInteger, "null-environment: expected an integer but got %T", version)
	}

	// R7RS specifies version 5 (for R5RS)
	switch versionInt.Value {
	case 5, 7:
		// Create a new empty top-level environment with only syntax bindings
		// For now, we return a fresh top-level environment
		newEnv := environment.NewTopLevelEnvironmentFrame()
		mc.SetValue(values.NewSchemeEnvironment("null-environment", newEnv))
		return nil
	default:
		return values.NewForeignError("null-environment: unsupported version, expected 5 or 7")
	}
}
