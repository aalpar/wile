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

// PrimFeatures implements the (features) primitive.
// Returns list of implementation features.
func PrimFeatures(_ context.Context, mc *machine.MachineContext) error {
	features := machine.AllFeatures()

	// Build a list of symbols
	result := values.EmptyList
	// Build the list in reverse order to get correct ordering
	for i := len(features) - 1; i >= 0; i-- {
		sym := mc.EnvironmentFrame().InternSymbol(values.NewSymbol(features[i]))
		result = values.NewCons(sym, result)
	}

	mc.SetValue(result)
	return nil
}
