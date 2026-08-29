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
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// PrimMakeParameter implements the (make-parameter) primitive.
// Creates a parameter object with an initial value and optional converter.
//
// (make-parameter init)           ; create with initial value
// (make-parameter init converter) ; create with converter procedure
//
// If a converter is provided, it is applied to the initial value and to
// any value passed when setting the parameter.
func PrimMakeParameter(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "make-parameter")
	if err != nil {
		return err
	}
	init := mc.Arg(0)
	rest := mc.Arg(1)

	// No converter: build the parameter directly.
	if values.IsEmptyList(rest) {
		mc.SetValue(machine.NewParameter(init, nil, machine.MutableBase))
		return nil
	}
	pr, ok := rest.(values.Tuple)
	if !ok || pr.IsEmptyList() {
		mc.SetValue(machine.NewParameter(init, nil, machine.MutableBase))
		return nil
	}
	converterCls, err := helpers.RequireCallable(pr.Car(), "make-parameter: converter")
	if err != nil {
		return err
	}

	// Converter present: apply it to init on the LIVE chain (not a sub-context) so a
	// continuation captured inside the converter spans the rest of the program. The
	// post-thunk work — wrap the converted value in a Parameter — runs as a chain frame
	// (the finalizer), mirroring how call-with-exit forwards its value: RunBodyUnderConsumer
	// inline-applies the converter to init, then applies the finalizer to its result. Fixes
	// the parameterize-converter case of continuation_subcontext_truncation_red_test.go.
	closureEnv := mc.ClosureEnv()
	finalizer := machine.NewForeignClosure(closureEnv, 1, false, func(finCC machine.CallContext) error {
		finMC, err := machine.RequireMachineContext(finCC, "make-parameter")
		if err != nil {
			return err
		}
		finMC.SetValue(machine.NewParameter(finMC.Arg(0), converterCls, machine.MutableBase))
		return nil
	})
	_, err = mc.RunBodyUnderConsumer(converterCls, finalizer, init)
	return err
}

// PrimParameterRawSet implements the (%parameter-raw-set! param val) primitive.
// Sets a parameter's internal value directly, bypassing the converter.
//
// This is an internal primitive — not part of the public API.
var PrimParameterRawSet = helpers.MakeBinarySetter(werr.ErrNotAParameter, "%parameter-raw-set!", func(param *machine.Parameter, val values.Value) {
	param.SetValue(val)
})

// PrimParameterConvert implements (%parameter-convert param val).
// Applies the parameter's converter to val and returns the result.
// If the parameter has no converter, returns val unchanged.
//
// Used by the parameterize macro to pre-convert the value before storing
// it as a continuation mark. This is an internal primitive.
func PrimParameterConvert(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "%parameter-convert")
	if err != nil {
		return err
	}
	param, err := helpers.RequireArg[*machine.Parameter](mc, 0, werr.ErrNotAParameter, "%parameter-convert")
	if err != nil {
		return err
	}
	val := mc.Arg(1)
	if !param.HasConverter() {
		mc.SetValue(val)
		return nil
	}
	// Apply the converter on the live chain (the apply recipe) rather than a sub-context,
	// so a continuation captured inside the converter spans the rest of the program. The
	// converter's value is this primitive's result (transparent delivery), so no chain
	// frame is needed — identical to apply. Fixes the parameterize-converter case of
	// continuation_subcontext_truncation_red_test.go.
	_, err = mc.ApplyCallable(param.Converter(), val)
	return err
}

// PrimParameterQ implements the parameter? predicate.
// Returns #t if the argument is a parameter object.
var PrimParameterQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*machine.Parameter)
	return ok
})
