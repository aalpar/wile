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
)

// PrimLcm implements the lcm primitive.
func PrimLcm(_ context.Context, mc *machine.MachineContext) error {
	return integerFold(mc, "lcm", 1, func(acc, val int64) int64 {
		g := GcdInt(acc, val)
		if g == 0 {
			return 0 // lcm(0, 0) = 0
		}
		return acc / g * val
	})
}
