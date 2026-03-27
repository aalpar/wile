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

func addReflection(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "procedure-arity", ParamCount: 1, Impl: PrimProcedureArity,
			Doc: "Returns the arity of a procedure.", ParamNames: []string{"proc"}, Category: "reflection",
			ParamTypes: []values.ValueType{values.TypeProcedure}},
		{Name: "procedure-name", ParamCount: 1, Impl: PrimProcedureName,
			Doc: "Returns the name of a procedure, or #f if anonymous.", ParamNames: []string{"proc"}, Category: "reflection",
			ParamTypes: []values.ValueType{values.TypeProcedure}},
		{Name: "procedure-source-location", ParamCount: 1, Impl: PrimProcedureSourceLocation,
			Doc: "Returns (file line column) for a procedure, or #f if unavailable.", ParamNames: []string{"proc"}, Category: "reflection",
			ParamTypes: []values.ValueType{values.TypeProcedure}},
		{Name: "procedure-bound-symbols", ParamCount: 1, Impl: PrimProcedureBoundSymbols,
			Doc: "Returns the list of symbols bound in a closure's environment, or #f.", ParamNames: []string{"proc"}, Category: "reflection",
			ParamTypes: []values.ValueType{values.TypeProcedure}},
		{Name: "procedure-type", ParamCount: 1, Impl: PrimProcedureType,
			Doc: "Returns a symbol classifying the procedure type.", ParamNames: []string{"proc"}, Category: "reflection",
			ParamTypes: []values.ValueType{values.TypeProcedure}, ReturnType: values.TypeSymbol},
		{Name: "procedure-documentation", ParamCount: 1, Impl: PrimProcedureDocumentation,
			Doc: "Returns the docstring of a procedure, or #f if none.", ParamNames: []string{"proc"}, Category: "reflection",
			ReturnType: values.TypeAny},
	}, registry.PhaseRuntime)

	return nil
}
