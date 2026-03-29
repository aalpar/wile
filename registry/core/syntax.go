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

func addSyntax(r *registry.Registry) error {
	// Syntax objects (R6RS syntax-case support)
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "identifier?", ParamCount: 1, Impl: PrimIdentifierQ,
			Doc: "Returns #t if obj is a syntax object wrapping a symbol (an identifier).\n\nExamples:\n  ;; Used inside syntax-case transformers:\n  ;; (identifier? #'foo)  => #t", ParamNames: []string{"obj"}, Category: "syntax",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "syntax->datum", ParamCount: 1, Impl: PrimSyntaxToDatum,
			Doc: "Recursively strips all syntax information, returning the underlying datum (symbols, pairs, etc.).\n\nExamples:\n  ;; (syntax->datum #'(+ 1 2))  => (+ 1 2)", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.ValueType{values.TypeAny}},
		{Name: "datum->syntax", ParamCount: 2, Impl: PrimDatumToSyntax,
			Doc: "Wraps datum as a syntax object inheriting lexical context (scopes) from template-id.\n\nExamples:\n  ;; (datum->syntax #'here 'my-var)  => syntax object for my-var", ParamNames: []string{"template-id", "datum"}, Category: "syntax",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeAny}},
		{Name: "generate-temporaries", ParamCount: 1, Impl: PrimGenerateTemporaries,
			Doc: "Returns a list of unique temporary identifiers, one per element in stx-list. Used in syntax-case macros.\n\nExamples:\n  ;; (length (generate-temporaries '(a b c)))  => 3", ParamNames: []string{"stx-list"}, Category: "syntax",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeList},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// Identifier comparison
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "bound-identifier=?", ParamCount: 2, Impl: PrimBoundIdentifierEqualQ,
			Doc: "Returns #t if id1 and id2 would bind the same variable if used as binding forms. Compares names and scope sets.\n\nExamples:\n  ;; Used in syntax-case to compare binders.", ParamNames: []string{"id1", "id2"}, Category: "syntax",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "free-identifier=?", ParamCount: 2, Impl: PrimFreeIdentifierEqualQ,
			Doc: "Returns #t if id1 and id2 refer to the same binding when used as free references. Used for literal matching in syntax-rules.\n\nExamples:\n  ;; Used in syntax-rules to match literals like `else' and `=>'.", ParamNames: []string{"id1", "id2"}, Category: "syntax",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeAny}, ReturnType: values.TypeBoolean},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
