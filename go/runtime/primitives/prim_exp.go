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

// PrimExp implements the (exp z) primitive.
// Returns e raised to the power z. Accepts all numeric types per R7RS.
func PrimExp(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	z, err := ToComplex128(o)
	if err != nil {
		return values.WrapForeignErrorf(err, "exp: %v", err)
	}
	mc.SetValue(ComplexOrFloat(cmplx.Exp(z)))
	return nil
}
