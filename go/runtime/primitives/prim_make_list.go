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

func PrimMakeList(_ context.Context, mc *machine.MachineContext) error {
	kVal := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	restVal := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	k, ok := kVal.(*values.Integer)
	if !ok {
		return values.NewForeignError("make-list: expected an integer for k")
	}
	count := int(k.Value)
	if count < 0 {
		return values.NewForeignError("make-list: k must be non-negative")
	}

	// Default fill value is unspecified; we use #f
	fill := values.Value(values.FalseValue)

	// Check for optional fill argument
	if rest, ok := restVal.(*values.Pair); ok && !rest.IsEmptyList() {
		fill = rest.Car()
	}

	// Build list from tail to head
	result := values.Value(values.EmptyList)
	for i := 0; i < count; i++ {
		result = values.NewCons(fill, result)
	}

	mc.SetValue(result)
	return nil
}
