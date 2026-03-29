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
			Doc: "Returns a pair (count . variadic?) describing the procedure's arity. Count is the number of required parameters.\n\nExamples:\n  (procedure-arity car)   => (1 . #f)\n  (procedure-arity +)     => (0 . #t)", ParamNames: []string{"proc"}, Category: "reflection",
			ParamTypes: []values.ValueType{values.TypeProcedure}},
		{Name: "procedure-name", ParamCount: 1, Impl: PrimProcedureName,
			Doc: "Returns the name of a procedure as a string, or #f if anonymous.\n\nExamples:\n  (procedure-name car)              => \"car\"\n  (procedure-name (lambda (x) x))  => #f", ParamNames: []string{"proc"}, Category: "reflection",
			ParamTypes: []values.ValueType{values.TypeProcedure}},
		{Name: "procedure-source-location", ParamCount: 1, Impl: PrimProcedureSourceLocation,
			Doc: "Returns a list (file line column) for the procedure's definition site, or #f if unavailable.\n\nExamples:\n  (procedure-source-location car)  => #f  ; foreign procedure", ParamNames: []string{"proc"}, Category: "reflection",
			ParamTypes: []values.ValueType{values.TypeProcedure}},
		{Name: "procedure-bound-symbols", ParamCount: 1, Impl: PrimProcedureBoundSymbols,
			Doc: "Returns the list of symbols bound in a closure's captured environment, or #f for foreign procedures.\n\nExamples:\n  (procedure-bound-symbols car)  => #f  ; foreign procedure", ParamNames: []string{"proc"}, Category: "reflection",
			ParamTypes: []values.ValueType{values.TypeProcedure}},
		{Name: "procedure-type", ParamCount: 1, Impl: PrimProcedureType,
			Doc: "Returns a symbol classifying the procedure: closure, foreign, case-lambda, parameter, or continuation.\n\nExamples:\n  (procedure-type car)              => foreign\n  (procedure-type (lambda (x) x))  => closure", ParamNames: []string{"proc"}, Category: "reflection",
			ParamTypes: []values.ValueType{values.TypeProcedure}, ReturnType: values.TypeSymbol},
		{Name: "procedure-documentation", ParamCount: 1, Impl: PrimProcedureDocumentation,
			Doc: "Returns the docstring of a procedure, or #f if none. Works for both Scheme-defined and foreign procedures.\n\nExamples:\n  (string? (procedure-documentation car))  => #t", ParamNames: []string{"proc"}, Category: "reflection",
			ParamTypes: []values.ValueType{values.TypeProcedure}, ReturnType: values.TypeAny},
		{Name: "apropos", ParamCount: 1, Impl: PrimApropos,
			Doc: "Returns a list of symbols whose name, doc, or category matches the pattern string (case-insensitive substring).\n\nExamples:\n  (pair? (memq 'car (apropos \"car\")))  => #t", ParamNames: []string{"pattern"}, Category: "reflection",
			ParamTypes: []values.ValueType{values.TypeString}, ReturnType: values.TypeList},
		{Name: "doc-topics", ParamCount: 0, Impl: PrimDocTopics,
			Doc: "Returns a sorted list of documentation category name strings.\n\nExamples:\n  (pair? (doc-topics))  => #t", Category: "reflection",
			ReturnType: values.TypeList},
		{Name: "doc-topic", ParamCount: 1, Impl: PrimDocTopic,
			Doc: "Returns a sorted list of symbols in the named documentation category.\n\nExamples:\n  (pair? (doc-topic \"pairs\"))  => #t", ParamNames: []string{"category"}, Category: "reflection",
			ParamTypes: []values.ValueType{values.TypeString}, ReturnType: values.TypeList},
		{Name: "library-description", ParamCount: 1, Impl: PrimLibraryDescription,
			Doc:        "Returns the description string of a loaded library, or #f if none or not loaded.\n\nExamples:\n  ;; (library-description '(scheme base))  => \"R7RS base library\" or #f",
			ParamNames: []string{"library-name"}, Category: "reflection",
			ParamTypes: []values.ValueType{values.TypeList}, ReturnType: values.TypeAny},
	}, registry.PhaseRuntime)

	return nil
}
