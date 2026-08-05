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

func addReflection(r *registry.PrimitiveRegistry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		// procedure-arity returns an integer for fixed-arity procedures, a pair
		// (min . #f) for variadic ones, a list of such descriptors for
		// case-lambda, (0 . #f) for continuations, and #f for callables with
		// unknown arity; TypeAny covers the union.
		{Name: "procedure-arity", ParamCount: 1, Impl: PrimProcedureArity,
			Doc: "Returns PROC's arity. For fixed-arity procedures, returns an integer — the parameter count. For variadic procedures, returns a pair (min . #f) where min is the required-argument count and #f signals no upper limit. Case-lambda returns a list of such arity descriptors. Continuations accept any number of values, so they return (0 . #f).\n\nExamples:\n  (procedure-arity car)   => 1\n  (procedure-arity +)     => (0 . #f)", ParamNames: []string{"proc"}, Category: "reflection",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure}, ReturnType: values.TypeAny},
		// procedure-name, procedure-source-location, procedure-bound-symbols
		// return a string/list or #f when unavailable — TypeAny covers the
		// union.
		{Name: "procedure-name", ParamCount: 1, Impl: PrimProcedureName,
			Doc: "Returns the name of PROC as a string, or #f if anonymous.\n\nExamples:\n  (procedure-name car)              => \"car\"\n  (procedure-name (lambda (x) x))  => #f", ParamNames: []string{"proc"}, Category: "reflection",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure}, ReturnType: values.TypeAny},
		{Name: "procedure-source-location", ParamCount: 1, Impl: PrimProcedureSourceLocation,
			Doc: "Returns a list (file line column) for PROC's definition site, or #f if unavailable.\n\nExamples:\n  (procedure-source-location car)  => #f  ; foreign procedure", ParamNames: []string{"proc"}, Category: "reflection",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure}, ReturnType: values.TypeAny},
		{Name: "procedure-bound-symbols", ParamCount: 1, Impl: PrimProcedureBoundSymbols,
			Doc: "Returns the list of symbols bound in PROC's captured environment, or #f for foreign procedures.\n\nExamples:\n  (procedure-bound-symbols car)  => #f  ; foreign procedure", ParamNames: []string{"proc"}, Category: "reflection",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure}, ReturnType: values.TypeAny},
		{Name: "procedure-type", ParamCount: 1, Impl: PrimProcedureType,
			Doc: "Returns a symbol classifying PROC: closure, foreign, case-lambda, parameter, continuation, or unknown. Both call/cc and call-with-composable-continuation captures classify as continuation.\n\nExamples:\n  (procedure-type car)              => foreign\n  (procedure-type (lambda (x) x))  => closure\n  (call/cc (lambda (k) (procedure-type k)))  => continuation", ParamNames: []string{"proc"}, Category: "reflection",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure}, ReturnType: values.TypeSymbol},
		{Name: "procedure-documentation", ParamCount: 1, Impl: PrimProcedureDocumentation,
			Doc: "Returns the docstring of PROC, or #f if none. Works for both Scheme-defined and foreign procedures.\n\nExamples:\n  (string? (procedure-documentation car))  => #t", ParamNames: []string{"proc"}, Category: "reflection",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure}, ReturnType: values.TypeAny},
		{Name: "apropos", ParamCount: 1, Impl: PrimApropos,
			Doc: "Returns a list of symbols whose name, doc, category, or keywords match PATTERN (case-insensitive substring).\n\nExamples:\n  (pair? (memq 'car (apropos \"car\")))  => #t", ParamNames: []string{"pattern"}, Category: "reflection",
			ParamTypes: []values.TypeConstraint{values.TypeString}, ReturnType: values.TypeList},
		{Name: "doc-topics", ParamCount: 0, Impl: PrimDocTopics,
			Doc: "Returns a sorted list of documentation category name strings.\n\nExamples:\n  (pair? (doc-topics))  => #t", Category: "reflection",
			ReturnType: values.TypeList},
		{Name: "doc-topic", ParamCount: 1, Impl: PrimDocTopic,
			Doc: "Returns a sorted list of symbols in the documentation category named CATEGORY.\n\nExamples:\n  (pair? (doc-topic \"pairs\"))  => #t", ParamNames: []string{"category"}, Category: "reflection",
			ParamTypes: []values.TypeConstraint{values.TypeString}, ReturnType: values.TypeList},
		{Name: "library-description", ParamCount: 1, Impl: PrimLibraryDescription,
			Doc:        "Returns the description string of LIBRARY-NAME, or #f if none or not loaded.\n\nExamples:\n  ;; (library-description '(scheme base))  => \"R7RS base library\" or #f",
			ParamNames: []string{"library-name"}, Category: "reflection",
			ParamTypes: []values.TypeConstraint{values.TypeList}, ReturnType: values.TypeAny},

		// PrimitiveSpec introspection — operate on wile's PrimitiveSpec
		// registry. These let Scheme code enumerate and inspect the
		// primitives registered in the current namespace without going
		// through the Go API. Used by tools/audit/wile-axis-b.scm among others.
		{Name: "registered-primitives", ParamCount: 0, Impl: PrimRegisteredPrimitives,
			Doc:        "Returns a list of primitive-spec values — one per primitive registered in the current namespace. Each spec is an opaque value queryable via primitive-spec-name, -return-type, -param-count, -variadic?, -go-function, -go-source, -category.\n\nExamples:\n  (length (registered-primitives))  ; => 475 (or similar)\n  (primitive-spec-name (car (registered-primitives)))  ; => some name\n\nSee also: `primitive-spec?', `primitive-spec-name'.",
			Category:   "reflection",
			ReturnType: values.TypeList},
		{Name: "primitive-spec?", ParamCount: 1, Impl: PrimPrimitiveSpecQ,
			Doc:        "Returns #t if V is a primitive-spec value (as returned by registered-primitives), #f otherwise.\n\nExamples:\n  (primitive-spec? (car (registered-primitives)))  ; => #t\n  (primitive-spec? 42)  ; => #f",
			ParamNames: []string{"v"}, Category: "reflection",
			ReturnType: values.TypeBoolean},
		{Name: "primitive-spec-name", ParamCount: 1, Impl: PrimPrimitiveSpecName,
			Doc:        "Returns the primitive's Scheme-level name as a string.\n\nExamples:\n  (primitive-spec-name (car (registered-primitives)))  ; => \"cons\"",
			ParamNames: []string{"spec"}, Category: "reflection",
			ReturnType: values.TypeString},
		{Name: "primitive-spec-return-type", ParamCount: 1, Impl: PrimPrimitiveSpecReturnType,
			Doc:        "Returns the primitive's declared ReturnType name (\"integer\", \"pair\", \"any\", ...), or the empty string if no type is declared.\n\nExamples:\n  (primitive-spec-return-type SPEC)  ; => \"pair\" (or \"\")",
			ParamNames: []string{"spec"}, Category: "reflection",
			ReturnType: values.TypeString},
		{Name: "primitive-spec-param-count", ParamCount: 1, Impl: PrimPrimitiveSpecParamCount,
			Doc:        "Returns the primitive's declared ParamCount as an integer.\n\nExamples:\n  (primitive-spec-param-count SPEC)  ; => 2",
			ParamNames: []string{"spec"}, Category: "reflection",
			ReturnType: values.TypeInteger},
		{Name: "primitive-spec-variadic?", ParamCount: 1, Impl: PrimPrimitiveSpecVariadicQ,
			Doc:        "Returns #t if the primitive is declared variadic (IsVariadic), #f otherwise.\n\nExamples:\n  (primitive-spec-variadic? SPEC)  ; => #f",
			ParamNames: []string{"spec"}, Category: "reflection",
			ReturnType: values.TypeBoolean},
		{Name: "primitive-spec-go-function", ParamCount: 1, Impl: PrimPrimitiveSpecGoFunction,
			Doc:        "Returns the fully-qualified Go function name of the primitive's Impl (as resolved via runtime.FuncForPC), or the empty string for binding-only primitives (nil Impl).\n\nExamples:\n  (primitive-spec-go-function SPEC)  ; => \"github.com/aalpar/wile/pkg/registry/core.PrimCons\"",
			ParamNames: []string{"spec"}, Category: "reflection",
			ReturnType: values.TypeString},
		{Name: "primitive-spec-go-source", ParamCount: 1, Impl: PrimPrimitiveSpecGoSource,
			Doc:        "Returns the \"file:line\" source location of the primitive's Impl (absolute path), or the empty string for binding-only primitives.\n\nExamples:\n  (primitive-spec-go-source SPEC)  ; => \"/path/to/prim_pairs.go:22\"",
			ParamNames: []string{"spec"}, Category: "reflection",
			ReturnType: values.TypeString},
		{Name: "primitive-spec-category", ParamCount: 1, Impl: PrimPrimitiveSpecCategory,
			Doc:        "Returns the primitive's Category as a string (e.g., \"reflection\", \"arithmetic\"), or the empty string if unset.\n\nExamples:\n  (primitive-spec-category SPEC)  ; => \"reflection\"",
			ParamNames: []string{"spec"}, Category: "reflection",
			ReturnType: values.TypeString},
	}, registry.PhaseSetRuntime)

	return nil
}
