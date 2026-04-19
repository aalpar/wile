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
			Doc: "Installs HANDLER as the current exception handler for the dynamic extent of THUNK. HANDLER receives the raised object.\n\nExamples:\n  (with-exception-handler\n    (lambda (e) 42)\n    (lambda () (raise-continuable \"oops\")))  => 42", ParamNames: []string{"handler", "thunk"}, Category: "exceptions",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure, values.TypeProcedure}, ReturnType: values.TypeAny,
			Keywords: []string{"try", "catch", "error handler", "trap"}},
		// raise/error never return normally — ReturnType is a nominal TypeAny.
		{Name: "raise", ParamCount: 1, Impl: PrimRaise,
			Doc: "Raises OBJ as a non-continuable exception. If the handler returns, a secondary exception is raised.\n\nExamples:\n  (guard (e (#t e)) (raise \"oops\"))  => \"oops\"", ParamNames: []string{"obj"}, Category: "exceptions",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny,
			Keywords: []string{"throw", "signal", "error"}},
		{Name: "raise-continuable", ParamCount: 1, Impl: PrimRaiseContinuable,
			Doc: "Raises OBJ as a continuable exception. The handler's return value becomes the result of the raise expression.\n\nExamples:\n  (with-exception-handler\n    (lambda (e) 42)\n    (lambda () (raise-continuable \"note\")))  => 42", ParamNames: []string{"obj"}, Category: "exceptions",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "error", ParamCount: 2, IsVariadic: true, Impl: PrimError,
			Doc: "Creates an error object from MESSAGE and optional irritants, then raises it as a non-continuable exception.\n\nExamples:\n  (guard (e (#t (error-object-message e))) (error \"bad value\" 42))  => \"bad value\"", ParamNames: []string{"message", "irritant"}, Category: "exceptions",
			ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "error-object?", ParamCount: 1, Impl: PrimErrorObjectQ,
			Doc: "Returns #t if OBJ is an error object created by error or raised by the implementation.\n\nExamples:\n  (guard (e (#t (error-object? e))) (error \"oops\"))  => #t", ParamNames: []string{"obj"}, Category: "exceptions",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "error-object-message", ParamCount: 1, Impl: PrimErrorObjectMessage,
			Doc: "Returns the message string of ERROR-OBJ.\n\nExamples:\n  (guard (e (#t (error-object-message e))) (error \"bad value\" 42))  => \"bad value\"", ParamNames: []string{"error-obj"}, Category: "exceptions",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeString},
		{Name: "error-object-irritants", ParamCount: 1, Impl: PrimErrorObjectIrritants,
			Doc: "Returns the list of irritants associated with ERROR-OBJ.\n\nExamples:\n  (guard (e (#t (error-object-irritants e))) (error \"bad\" 1 2))  => (1 2)", ParamNames: []string{"error-obj"}, Category: "exceptions",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeList},
		{Name: "read-error?", ParamCount: 1, Impl: PrimReadErrorQ,
			Doc: "Returns #t if OBJ is an error object signaling a read/parse error.\n\nExamples:\n  (read-error? (guard (e (#t e)) (error \"oops\")))  => #f", ParamNames: []string{"obj"}, Category: "exceptions",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "file-error?", ParamCount: 1, Impl: PrimFileErrorQ,
			Doc: "Returns #t if OBJ is an error object signaling a file I/O error.\n\nExamples:\n  (file-error? (guard (e (#t e)) (error \"oops\")))  => #f", ParamNames: []string{"obj"}, Category: "exceptions",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "current-error-context", ParamCount: 0, Impl: PrimCurrentErrorContext,
			Doc: "Returns the current error context if called inside an exception handler, or #f otherwise.\nThe error context carries diagnostic information captured at the raise site.\n\nExamples:\n  (current-error-context)  => #f", Category: "exceptions",
			ReturnType: values.TypeAny,
			Keywords:   []string{"error diagnostics", "exception context", "stack trace"}},
		{Name: "error-context?", ParamCount: 1, Impl: PrimErrorContextQ,
			Doc: "Returns #t if OBJ is an error context value.\n\nExamples:\n  (error-context? 42)  => #f", ParamNames: []string{"obj"}, Category: "exceptions",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		// Source/marks accessors return a string, context, or #f — TypeAny
		// covers the union.
		{Name: "error-context-source", ParamCount: 1, Impl: PrimErrorContextSource,
			Doc: "Returns the source location string from CTX, or #f if unavailable.\n\nExamples:\n  ;; (error-context-source ctx)  =>  \"file.scm:10:5\"", ParamNames: []string{"ctx"}, Category: "exceptions",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "error-context-stack-trace", ParamCount: 1, Impl: PrimErrorContextStackTrace,
			Doc: "Returns the stack trace from CTX as a list of alists.\nEach alist always has key name; keys file, line, column are present only when source information is available.\n\nExamples:\n  ;; (error-context-stack-trace ctx)  =>  (((name . \"f\") (file . \"test.scm\") ...))", ParamNames: []string{"ctx"}, Category: "exceptions",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeList},
		{Name: "error-context-marks", ParamCount: 1, Impl: PrimErrorContextMarks,
			Doc: "Returns the continuation mark set from CTX, or #f if not captured.\nNote: mark capture is not yet implemented; currently always returns #f.\n\nExamples:\n  ;; (error-context-marks ctx)  =>  #f", ParamNames: []string{"ctx"}, Category: "exceptions",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "error-object-source", ParamCount: 1, Impl: PrimErrorObjectSource,
			Doc: "Returns the source location string from ERROR-OBJ, or #f if unavailable.\nThe source location is populated when the error is raised via (error ...) or (raise ...).\n\nExamples:\n  (guard (e (#t (error-object-source e))) (error \"oops\"))  => \"eval:1:41\"", ParamNames: []string{"error-obj"}, Category: "exceptions",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny,
			Keywords: []string{"error location", "source location", "error diagnostics"}},
		{Name: "error-object-stack-trace", ParamCount: 1, Impl: PrimErrorObjectStackTrace,
			Doc: "Returns the stack trace from ERROR-OBJ as a list of alists, or () if unavailable.\nEach alist always has key name; keys file, line, column are present only when source information is available.\n\nExamples:\n  (guard (e (#t (error-object-stack-trace e))) (error \"oops\"))  => (((name . \"...\") ...))", ParamNames: []string{"error-obj"}, Category: "exceptions",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeList,
			Keywords: []string{"error stack trace", "error diagnostics", "backtrace"}},
	}, registry.PhaseRuntime)
	return nil
}
