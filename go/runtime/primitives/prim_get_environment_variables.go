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
	"strings"

	"wile/machine"
	"wile/values"
)

// PrimGetEnvironmentVariables implements the (get-environment-variables) primitive.
// Returns all environment variables.
func PrimGetEnvironmentVariables(_ context.Context, mc *machine.MachineContext) error {
	env := os.Environ()
	list := values.EmptyList
	for i := len(env) - 1; i >= 0; i-- {
		parts := strings.SplitN(env[i], "=", 2)
		if len(parts) == 2 {
			pair := values.NewCons(values.NewString(parts[0]), values.NewString(parts[1]))
			list = values.NewCons(pair, list)
		}
	}
	mc.SetValue(list)
	return nil
}
