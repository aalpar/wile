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

// PrimExactQ implements the (exact?) primitive.
//
// R7RS §6.2.6: Returns #t if the number is exact, #f otherwise.
func PrimExactQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch o.(type) {
	case *values.Integer, *values.BigInteger, *values.Rational:
		mc.SetValue(values.TrueValue)
	case *values.Float, *values.BigFloat, *values.Complex:
		mc.SetValue(values.FalseValue)
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "exact?: expected a number but got %T", o)
	}
	return nil
}
