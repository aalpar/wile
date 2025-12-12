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
	"errors"

	"wile/machine"
	"wile/values"
)

// PrimMakeParameter implements the (make-parameter) primitive.
// Creates a parameter object with an initial value and optional converter.
//
// (make-parameter init)           ; create with initial value
// (make-parameter init converter) ; create with converter procedure
//
// If a converter is provided, it is applied to the initial value and to
// any value passed when setting the parameter.
func PrimMakeParameter(ctx context.Context, mc *machine.MachineContext) error {
	init := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	var converter values.Value

	// Check for optional converter in rest args
	if !values.IsEmptyList(rest) {
		if pr, ok := rest.(*values.Pair); ok && !values.IsEmptyList(pr) {
			converter = pr.Car()

			// Validate converter is a procedure
			converterCls, ok := converter.(*machine.MachineClosure)
			if !ok {
				return values.NewForeignError("make-parameter: converter must be a procedure")
			}

			// Apply converter to initial value
			sub := mc.NewSubContext()
			if _, err := sub.Apply(converterCls, init); err != nil {
				return values.WrapForeignErrorf(err, "make-parameter: failed to apply converter")
			}
			if err := sub.Run(ctx); err != nil {
				if !errors.Is(err, machine.ErrMachineHalt) {
					return values.WrapForeignErrorf(err, "make-parameter: converter error")
				}
			}
			init = sub.GetValue()
		}
	}

	param := values.NewParameter(init, converter)
	mc.SetValue(param)
	return nil
}
