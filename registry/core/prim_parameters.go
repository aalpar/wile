// Copyright 2026 Aaron Alpar
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

package core

import (
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
)

// PrimMakeParameter implements the (make-parameter) primitive.
// Creates a parameter object with an initial value and optional converter.
//
// (make-parameter init)           ; create with initial value
// (make-parameter init converter) ; create with converter procedure
//
// If a converter is provided, it is applied to the initial value and to
// any value passed when setting the parameter.
func PrimMakeParameter(mc *machine.MachineContext) error {
	init := mc.Arg(0)
	rest := mc.Arg(1)

	var converterCls *machine.MachineClosure

	// Check for optional converter in rest args
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(values.Tuple)
		if ok && !pr.IsEmptyList() {
			// Validate converter is a procedure
			converterCls, ok = pr.Car().(*machine.MachineClosure)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAProcedure, "make-parameter: converter must be a procedure")
			}

			// Apply converter to initial value
			sub := mc.NewSubContext()
			defer machine.ReleaseSubContext(sub)
			_, err := sub.Apply(converterCls, init)
			if err != nil {
				return values.WrapForeignErrorf(err, "make-parameter: failed to apply converter")
			}
			err = sub.Run()
			if err != nil {
				return values.WrapForeignErrorf(err, "make-parameter: converter error")
			}
			init = sub.GetValue()
		}
	}

	param := machine.NewParameter(init, converterCls)
	mc.SetValue(param)
	return nil
}

// PrimParameterQ implements the parameter? predicate.
// Returns #t if the argument is a parameter object.
var PrimParameterQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*machine.Parameter)
	return ok
})
