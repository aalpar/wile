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

// PrimSchemeReportEnvironment implements the (scheme-report-environment) primitive.
// Returns R5RS env.
func PrimSchemeReportEnvironment(_ context.Context, mc *machine.MachineContext) error {
	version := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	versionInt, ok := version.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInteger, "scheme-report-environment: expected an integer but got %T", version)
	}

	// R7RS specifies version 5 (for R5RS) or 7 (for R7RS)
	switch versionInt.Value {
	case 5, 7:
		// Return the current top-level environment
		// In a full implementation, this would return a restricted environment
		topLevel := mc.EnvironmentFrame().TopLevel()
		mc.SetValue(values.NewSchemeEnvironment("scheme-report-environment", topLevel))
		return nil
	default:
		return values.NewForeignError("scheme-report-environment: unsupported version, expected 5 or 7")
	}
}
