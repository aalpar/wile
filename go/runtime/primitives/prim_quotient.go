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
	"math/big"

	"wile/machine"
	"wile/values"
)

// PrimQuotient implements the (quotient) primitive.
// Returns truncated integer quotient.
// Accepts exact and inexact integers per R7RS.
func PrimQuotient(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)

	// Extract integer values, tracking inexactness
	v0, big0, inexact0, err := extractInteger(o0, "quotient")
	if err != nil {
		return err
	}
	v1, big1, inexact1, err := extractInteger(o1, "quotient")
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
			return values.NewForeignError("quotient: division by zero")
		}
		result := new(big.Int).Quo(b0, b1)
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
		return values.NewForeignError("quotient: division by zero")
	}
	result := v0 / v1
	if inexact {
		mc.SetValue(values.NewFloat(float64(result)))
	} else {
		mc.SetValue(values.NewInteger(result))
	}
	return nil
}

// extractInteger extracts an integer value from Integer, BigInteger, or Float (if integral).
// Returns (int64Value, bigIntValue, isInexact, error).
// If bigIntValue is non-nil, use that; otherwise use int64Value.
func extractInteger(v values.Value, name string) (int64, *big.Int, bool, error) {
	switch n := v.(type) {
	case *values.Integer:
		return n.Value, nil, false, nil
	case *values.BigInteger:
		return 0, n.BigInt(), false, nil
	case *values.Float:
		// Check if it's an integer value
		if math.IsInf(n.Value, 0) || math.IsNaN(n.Value) {
			return 0, nil, false, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %v", name, n.Value)
		}
		if math.Floor(n.Value) != n.Value {
			return 0, nil, false, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %v", name, n.Value)
		}
		// Check if it fits in int64
		if n.Value >= -9223372036854775808 && n.Value <= 9223372036854775807 {
			return int64(n.Value), nil, true, nil
		}
		// Large float needs BigInt
		bf := new(big.Float).SetFloat64(n.Value)
		bi, _ := bf.Int(nil)
		return 0, bi, true, nil
	default:
		return 0, nil, false, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %T", name, v)
	}
}
