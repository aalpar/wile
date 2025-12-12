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

// PrimExit implements the (exit) primitive.
// Exits the program with an optional status code.
func PrimExit(_ context.Context, mc *machine.MachineContext) error {
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	code := 0
	if rest != values.EmptyList {
		if pr, ok := rest.(*values.Pair); ok && !values.IsEmptyList(pr) {
			switch v := pr.Car().(type) {
			case *values.Integer:
				code = int(v.Value)
			case *values.Boolean:
				if !v.Value {
					code = 1
				}
			}
		}
	}
	os.Exit(code)
	return nil
}
