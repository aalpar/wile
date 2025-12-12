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

// PrimMakeVector implements the (make-vector) primitive.
// Creates a vector of the given size, optionally filled with a specified value.
func PrimMakeVector(_ context.Context, mc *machine.MachineContext) error {
	k := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	size, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "make-vector: expected an integer but got %T", k)
	}
	if size.Value < 0 {
		return values.NewForeignError("make-vector: size must be non-negative")
	}
	var fill values.Value = values.FalseValue
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(*values.Pair)
		if ok && !values.IsEmptyList(pr) {
			fill = pr.Car()
		}
	}
	elems := make(values.Vector, size.Value)
	for i := range elems {
		elems[i] = fill
	}
	mc.SetValue(values.NewVector(elems...))
	return nil
}
