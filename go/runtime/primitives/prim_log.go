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

// PrimLog implements the (log) primitive.
// Returns the natural logarithm of a number, optionally with a specified base.
func PrimLog(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	x, err := ToFloat64(o)
	if err != nil {
		return values.WrapForeignErrorf(err, "log: %v", err)
	}
	if rest == values.EmptyList {
		mc.SetValue(values.NewFloat(math.Log(x)))
	} else {
		baseArg, ok := rest.(*values.Pair)
		if !ok {
			return values.NewForeignError("log: expected a list for rest arguments")
		}
		base, err := ToFloat64(baseArg.Car())
		if err != nil {
			return values.WrapForeignErrorf(err, "log: %v", err)
		}
		mc.SetValue(values.NewFloat(math.Log(x) / math.Log(base)))
	}
	return nil
}
