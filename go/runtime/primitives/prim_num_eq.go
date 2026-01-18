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
	"math"

	"wile/machine"
	"wile/values"
)

// numericEquals compares two numbers for equality.
//
// R7RS §6.2.5: The = procedure returns #t if its arguments are numerically
// equal. For IEEE 754 floats: infinities of the same sign are equal,
// NaN is not equal to anything (including itself).
func numericEquals(a, b values.Number) bool {
	// Handle Float specially due to IEEE 754 infinity and NaN
	af, aIsFloat := a.(*values.Float)
	bf, bIsFloat := b.(*values.Float)
	if aIsFloat && bIsFloat {
		// NaN != NaN per IEEE 754
		if math.IsNaN(af.Value) || math.IsNaN(bf.Value) {
			return false
		}
		// Direct comparison handles infinities correctly
		return af.Value == bf.Value
	}
	// For mixed types or non-Float, use subtraction
	// (works correctly except for Float infinities, already handled)
	return a.Subtract(b).IsZero()
}

// PrimNumEq implements the = primitive.
//
// R7RS §6.2.6: Returns #t if its arguments are numerically equal.
func PrimNumEq(_ context.Context, mc *machine.MachineContext) error {
	return numericChainCompare(mc, "=", func(prev, curr values.Number) bool {
		return !numericEquals(prev, curr)
	})
}
