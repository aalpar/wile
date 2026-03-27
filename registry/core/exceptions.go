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

func addExceptions(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "with-exception-handler", ParamCount: 2, Impl: PrimWithExceptionHandler,
			Doc: "Installs handler as the current exception handler for the dynamic extent of thunk. Handler receives the raised object.", ParamNames: []string{"handler", "thunk"}, Category: "exceptions",
			ParamTypes: []values.ValueType{values.TypeProcedure, values.TypeProcedure}},
		{Name: "raise", ParamCount: 1, Impl: PrimRaise,
			Doc: "Raises obj as a non-continuable exception. If the handler returns, a secondary exception is raised.", ParamNames: []string{"obj"}, Category: "exceptions",
			ParamTypes: []values.ValueType{values.TypeAny}},
		{Name: "raise-continuable", ParamCount: 1, Impl: PrimRaiseContinuable,
			Doc: "Raises obj as a continuable exception. The handler's return value becomes the result of the raise expression.", ParamNames: []string{"obj"}, Category: "exceptions",
			ParamTypes: []values.ValueType{values.TypeAny}},
		{Name: "error", ParamCount: 2, IsVariadic: true, Impl: PrimError,
			Doc: "Creates an error object from message and optional irritants, then raises it as a non-continuable exception.", ParamNames: []string{"message", "irritant"}, Category: "exceptions",
			ParamTypes: []values.ValueType{values.TypeString, values.TypeAny}},
		{Name: "error-object?", ParamCount: 1, Impl: PrimErrorObjectQ,
			Doc: "Returns #t if obj is an error object created by error or raised by the implementation.", ParamNames: []string{"obj"}, Category: "exceptions",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "error-object-message", ParamCount: 1, Impl: PrimErrorObjectMessage,
			Doc: "Returns the message string of an error object.", ParamNames: []string{"error-obj"}, Category: "exceptions",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeString},
		{Name: "error-object-irritants", ParamCount: 1, Impl: PrimErrorObjectIrritants,
			Doc: "Returns the list of irritants associated with an error object.", ParamNames: []string{"error-obj"}, Category: "exceptions",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeList},
		{Name: "read-error?", ParamCount: 1, Impl: PrimReadErrorQ,
			Doc: "Returns #t if obj is an error object signaling a read/parse error.", ParamNames: []string{"obj"}, Category: "exceptions",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "file-error?", ParamCount: 1, Impl: PrimFileErrorQ,
			Doc: "Returns #t if obj is an error object signaling a file I/O error.", ParamNames: []string{"obj"}, Category: "exceptions",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
	}, registry.PhaseRuntime)
	return nil
}
