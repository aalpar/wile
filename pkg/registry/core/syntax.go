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
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
)

func addSyntax(r *registry.PrimitiveRegistry) error {
	// Syntax objects (R6RS syntax-case support)
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "identifier?", ParamCount: 1, Impl: PrimIdentifierQ,
			Doc: "Returns #t if OBJ is a syntax object wrapping a symbol (an identifier).\n\nExamples:\n  ;; Used inside syntax-case transformers:\n  ;; (identifier? #'foo)  => #t", ParamNames: []string{"obj"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		// Syntax objects have no dedicated ValueType enum; annotate datum
		// accessors/constructors as TypeAny.
		{Name: "syntax->datum", ParamCount: 1, Impl: PrimSyntaxToDatum,
			Doc: "Recursively strips all syntax information from STX, returning the underlying datum (symbols, pairs, etc.).\n\nExamples:\n  ;; (syntax->datum #'(+ 1 2))  => (+ 1 2)", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "datum->syntax", ParamCount: 2, Impl: PrimDatumToSyntax,
			Doc: "Wraps DATUM as a syntax object inheriting lexical context (scopes) from TEMPLATE-ID.\n\nExamples:\n  ;; (datum->syntax #'here 'my-var)  => syntax object for my-var", ParamNames: []string{"template-id", "datum"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "generate-temporaries", ParamCount: 1, Impl: PrimGenerateTemporaries,
			Doc: "Returns a list of unique temporary identifiers, one per element in STX-LIST. Used in syntax-case macros.\n\nExamples:\n  ;; (length (generate-temporaries '(a b c)))  => 3", ParamNames: []string{"stx-list"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeList},
		{Name: "syntax-local-value", ParamCount: 2, IsVariadic: true, InvokesProcedure: true, Impl: PrimSyntaxLocalValue,
			Doc: "Returns the compile-time value bound to identifier ID: a macro transformer, or any value a let-syntax or define-syntax right-hand side evaluated to. Only valid during macro expansion. With no binding, calls FAILURE-THUNK when given, else raises.\n\nExamples:\n  ;; inside a transformer:\n  ;; (syntax-local-value #'k (lambda () #f))", ParamNames: []string{"id", "failure-thunk"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeAny}, ReturnType: values.TypeAny},

		// The one-level accessors (design §2.4): every result stays syntax, so a
		// Scheme syntax-case can walk a form without losing hygiene.
		{Name: "syntax-pair?", ParamCount: 1, Impl: PrimSyntaxPairQ,
			Doc: "Returns #t if OBJ is a non-empty syntax pair.\n\nExamples:\n  ;; (syntax-pair? #'(a b))  => #t", ParamNames: []string{"obj"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "syntax-null?", ParamCount: 1, Impl: PrimSyntaxNullQ,
			Doc: "Returns #t if OBJ is the empty list, as syntax or plain.\n\nExamples:\n  ;; (syntax-null? #'())  => #t", ParamNames: []string{"obj"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "syntax-car", ParamCount: 1, Impl: PrimSyntaxCar,
			Doc: "Returns the car of syntax pair STX, still a syntax object.\n\nExamples:\n  ;; (syntax->datum (syntax-car #'(a b)))  => a", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "syntax-cdr", ParamCount: 1, Impl: PrimSyntaxCdr,
			Doc: "Returns the cdr of syntax pair STX, still a syntax object.\n\nExamples:\n  ;; (syntax->datum (syntax-cdr #'(a b)))  => (b)", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "syntax-vector?", ParamCount: 1, Impl: PrimSyntaxVectorQ,
			Doc: "Returns #t if OBJ is a syntax vector.\n\nExamples:\n  ;; (syntax-vector? #'#(1 2))  => #t", ParamNames: []string{"obj"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "syntax-vector->list", ParamCount: 1, Impl: PrimSyntaxVectorToList,
			Doc: "Returns the elements of syntax vector STX as a syntax list, so a vector pattern reduces to the list matcher.\n\nExamples:\n  ;; (syntax->datum (syntax-vector->list #'#(a 2)))  => (a 2)", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "%syntax-spine", ParamCount: 1, Impl: PrimSyntaxSpine,
			Doc: "Unwraps the pairs and vectors of STX, keeping identifiers as syntax; the form an er-macro-transformer procedure receives.\n\nExamples:\n  ;; (identifier? (car (%syntax-spine #'(a b))))  => #t", ParamNames: []string{"stx"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "%syntax-violation", ParamCount: 3, Impl: PrimSyntaxViolation,
			Doc: "Raises a syntax error \"WHO: MESSAGE\" about form STX, carrying its source location.\n\nExamples:\n  ;; (%syntax-violation 'syntax-case \"no clause matches\" stx)", ParamNames: []string{"who", "message", "stx"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeString, values.TypeAny}, ReturnType: values.TypeAny},
	}, registry.PhaseSetRuntime|registry.PhaseSetExpand)

	// Identifier comparison
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "bound-identifier=?", ParamCount: 2, Impl: PrimBoundIdentifierEqualQ,
			Doc: "Returns #t if ID1 and ID2 would bind the same variable if used as binding forms. Compares names and scope sets.\n\nExamples:\n  ;; Used in syntax-case to compare binders.", ParamNames: []string{"id1", "id2"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "free-identifier=?", ParamCount: 2, Impl: PrimFreeIdentifierEqualQ,
			Doc: "Returns #t if ID1 and ID2 refer to the same binding when used as free references. Used for literal matching in syntax-rules.\n\nExamples:\n  ;; Used in syntax-rules to match literals like `else' and `=>'.", ParamNames: []string{"id1", "id2"}, Category: "syntax",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeAny}, ReturnType: values.TypeBoolean},
	}, registry.PhaseSetRuntime|registry.PhaseSetExpand)

	return nil
}
