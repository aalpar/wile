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
	"math/big"

	"wile/machine"
	"wile/values"
)

// PrimModulo implements the modulo primitive.
// Returns the modulo of two integers with the sign of the divisor.
// Accepts exact and inexact integers per R7RS.
func PrimModulo(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)

	// Extract integer values, tracking inexactness
	v0, big0, inexact0, err := extractInteger(o0, "modulo")
	if err != nil {
		return err
	}
	v1, big1, inexact1, err := extractInteger(o1, "modulo")
	if err != nil {
		return err
	}

	inexact := inexact0 || inexact1

	// Handle BigInteger case
	if big0 != nil || big1 != nil {
		b0 := big0
		if b0 == nil {
			b0 = big.NewInt(v0)
		}
		b1 := big1
		if b1 == nil {
			b1 = big.NewInt(v1)
		}
		if b1.Sign() == 0 {
			return values.NewForeignError("modulo: division by zero")
		}
		result := new(big.Int).Rem(b0, b1)
		// Adjust result to have the same sign as b1 (Scheme semantics)
		if (result.Sign() < 0 && b1.Sign() > 0) || (result.Sign() > 0 && b1.Sign() < 0) {
			result.Add(result, b1)
		}
		if inexact {
			f, _ := new(big.Float).SetInt(result).Float64()
			mc.SetValue(values.NewFloat(f))
		} else {
			mc.SetValue(values.NewBigInteger(result))
		}
		return nil
	}

	// Regular integer case
	if v1 == 0 {
		return values.NewForeignError("modulo: division by zero")
	}
	result := v0 % v1
	// Adjust result to have the same sign as v1 (Scheme semantics)
	if (result < 0 && v1 > 0) || (result > 0 && v1 < 0) {
		result += v1
	}
	if inexact {
		mc.SetValue(values.NewFloat(float64(result)))
	} else {
		mc.SetValue(values.NewInteger(result))
	}
	return nil
}
