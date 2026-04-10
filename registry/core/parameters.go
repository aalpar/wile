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
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/values"
)

func addParameters(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-parameter", ParamCount: 2, IsVariadic: true, Impl: PrimMakeParameter,
			Doc: "Creates a new parameter object with INIT as its initial value. If CONVERTER is given, it is applied to INIT and all future values.\n\nExamples:\n  (let ((p (make-parameter 10))) (p))  => 10\n  (let ((p (make-parameter 10))) (parameterize ((p 20)) (p)))  => 20", ParamNames: []string{"init", "converter"}, Category: "parameters",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeProcedure}, ReturnType: values.TypeProcedure},
		{Name: "parameter?", ParamCount: 1, Impl: PrimParameterQ,
			Doc: "Returns #t if OBJ is a parameter object created by make-parameter.\n\nExamples:\n  (parameter? (make-parameter 0))  => #t\n  (parameter? car)                 => #f", ParamNames: []string{"obj"}, Category: "parameters",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		// Internal primitive: bypasses converter when restoring a parameter in parameterize.
		// Not part of the public R7RS API.
		{Name: "%parameter-raw-set!", ParamCount: 2, Impl: PrimParameterRawSet,
			Doc: "Internal: sets a parameter's value directly, bypassing the converter. Used by parameterize restore.", ParamNames: []string{"param", "val"}, Category: "parameters",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure, values.TypeAny}, ReturnType: values.TypeVoid},
		// Internal primitive: applies converter without setting the parameter value.
		// Used by parameterize to pre-convert the value before storing as a continuation mark.
		{Name: "%parameter-convert", ParamCount: 2, Impl: PrimParameterConvert,
			Doc: "Internal: applies the parameter's converter to VAL without setting it. Used by parameterize to pre-convert.", ParamNames: []string{"param", "val"}, Category: "parameters",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure, values.TypeAny}},
	}, registry.PhaseRuntime)

	return nil
}
