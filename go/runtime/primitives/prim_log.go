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
	"math/cmplx"

	"wile/machine"
	"wile/values"
)

// PrimLog implements the (log z) and (log z1 z2) primitives.
// Returns the natural logarithm of z, or log base z2 of z1.
// Accepts all numeric types per R7RS. Returns complex for negative reals.
func PrimLog(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	rest := mc.Arg(1)
	z, err := ToComplex128(o)
	if err != nil {
		return values.WrapForeignErrorf(err, "log: %v", err)
	}
	if rest == values.EmptyList {
		mc.SetValue(ComplexOrFloat(cmplx.Log(z)))
	} else {
		baseArg, ok := rest.(*values.Pair)
		if !ok {
			return values.NewForeignError("log: expected a list for rest arguments")
		}
		base, err := ToComplex128(baseArg.Car())
		if err != nil {
			return values.WrapForeignErrorf(err, "log: %v", err)
		}
		// log_base(z) = log(z) / log(base)
		mc.SetValue(ComplexOrFloat(cmplx.Log(z) / cmplx.Log(base)))
	}
	return nil
}
