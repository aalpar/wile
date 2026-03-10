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
)

func addExceptions(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "with-exception-handler", ParamCount: 2, Impl: PrimWithExceptionHandler,
			Doc: "Installs an exception handler and calls thunk.", ParamNames: []string{"handler", "thunk"}, Category: "exceptions"},
		{Name: "raise", ParamCount: 1, Impl: PrimRaise,
			Doc: "Raises a non-continuable exception.", ParamNames: []string{"obj"}, Category: "exceptions"},
		{Name: "raise-continuable", ParamCount: 1, Impl: PrimRaiseContinuable,
			Doc: "Raises a continuable exception.", ParamNames: []string{"obj"}, Category: "exceptions"},
		{Name: "error", ParamCount: 2, IsVariadic: true, Impl: PrimError,
			Doc: "Creates an error object and raises it.", ParamNames: []string{"message", "irritant"}, Category: "exceptions"},
		{Name: "error-object?", ParamCount: 1, Impl: PrimErrorObjectQ,
			Doc: "Returns #t if obj is an error object.", ParamNames: []string{"obj"}, Category: "exceptions"},
		{Name: "error-object-message", ParamCount: 1, Impl: PrimErrorObjectMessage,
			Doc: "Returns the message of an error object.", ParamNames: []string{"error-obj"}, Category: "exceptions"},
		{Name: "error-object-irritants", ParamCount: 1, Impl: PrimErrorObjectIrritants,
			Doc: "Returns the irritants of an error object.", ParamNames: []string{"error-obj"}, Category: "exceptions"},
		{Name: "read-error?", ParamCount: 1, Impl: PrimReadErrorQ,
			Doc: "Returns #t if obj is a read error.", ParamNames: []string{"obj"}, Category: "exceptions"},
		{Name: "file-error?", ParamCount: 1, Impl: PrimFileErrorQ,
			Doc: "Returns #t if obj is a file error.", ParamNames: []string{"obj"}, Category: "exceptions"},
	}, registry.PhaseRuntime)
	return nil
}
