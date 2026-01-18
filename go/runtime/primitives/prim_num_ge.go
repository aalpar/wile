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

// PrimNumGe implements the >= primitive.
//
// R7RS §6.2.6: Returns #t if its arguments are monotonically nonincreasing.
// IEEE 754: Any comparison with NaN returns #f.
func PrimNumGe(_ context.Context, mc *machine.MachineContext) error {
	return numericChainCompare(mc, ">=", func(prev, curr values.Number) bool {
		// NaN fails all comparisons per IEEE 754
		if isNaN(prev) || isNaN(curr) {
			return true // fails the comparison
		}
		return prev.LessThan(curr)
	})
}
