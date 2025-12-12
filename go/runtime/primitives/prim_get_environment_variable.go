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
	"os"

	"wile/machine"
	"wile/values"
)

// PrimGetEnvironmentVariable implements the (get-environment-variable) primitive.
// Gets environment variable value.
func PrimGetEnvironmentVariable(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	name, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "get-environment-variable: expected a string but got %T", o)
	}
	val, exists := os.LookupEnv(name.Value)
	if exists {
		mc.SetValue(values.NewString(val))
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}
