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

	var converterCls machine.Closure

	// Check for optional converter in rest args
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(values.Tuple)
		if ok && !pr.IsEmptyList() {
			// Validate converter is a procedure
			converterCls, ok = pr.Car().(machine.Closure)
			if !ok {
				return werr.WrapForeignErrorf(werr.ErrNotAProcedure, "make-parameter: converter must be a procedure")
			}

			// Apply converter to initial value
			sub := mc.NewSubContext()
			defer machine.ReleaseSubContext(sub)
			_, err := sub.ApplyCallable(converterCls, init)
			if err != nil {
				return werr.WrapForeignErrorf(err, "make-parameter: failed to apply converter")
			}
			err = sub.Run()
			if err != nil {
				return werr.WrapForeignErrorf(err, "make-parameter: converter error")
			}
			init = sub.GetValue()
		}
	}

	param := machine.NewParameter(init, converterCls)
	mc.SetValue(param)
	return nil
}

// PrimParameterRawSet implements the (%parameter-raw-set! param val) primitive.
// Sets a parameter's internal value directly, bypassing the converter.
//
// This is an internal primitive — not part of the public API.
func PrimParameterRawSet(mc machine.CallContext) error {
	param, err := helpers.RequireArg[*machine.Parameter](mc, 0, werr.ErrNotAParameter, "%parameter-raw-set!")
	if err != nil {
		return err
	}
	param.SetValue(mc.Arg(1))
	mc.SetValue(values.Void)
	return nil
}

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
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	_, err = sub.ApplyCallable(param.Converter(), val)
	if err != nil {
		return werr.WrapForeignErrorf(err, "%%parameter-convert: failed to apply converter")
	}
	err = sub.Run()
	if err != nil {
		return werr.WrapForeignErrorf(err, "%%parameter-convert: converter error")
	}
	mc.SetValue(sub.GetValue())
	return nil
}

// PrimParameterQ implements the parameter? predicate.
// Returns #t if the argument is a parameter object.
var PrimParameterQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*machine.Parameter)
	return ok
})
