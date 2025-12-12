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

// PrimMakeCompileTimeValue implements the make-compile-time-value primitive.
// Wraps a value for compile-time storage.
// (make-compile-time-value value) -> compile-time-value
//
// This creates a CompileTimeValue wrapper around the given value. CompileTimeValue
// is used to distinguish regular runtime values from values that should be stored
// in the expand phase and accessed during macro expansion.
//
// When syntax-local-value retrieves a CompileTimeValue, it automatically unwraps
// it to return the underlying value.
func PrimMakeCompileTimeValue(ctx context.Context, mc *machine.MachineContext) error {
	v := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

	ctv := values.NewCompileTimeValue(v)
	mc.SetValue(ctv)
	return nil
}
