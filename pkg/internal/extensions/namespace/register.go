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

// Package namespace provides namespace manipulation primitives.
package namespace

import (
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
)

// Extension is the namespace extension.
var Extension = registry.NewDescribedExtension("namespace",
	"Namespace management: environment creation, binding inspection.",
	AddToRegistry)

// Builder aggregates all namespace registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all namespace primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.PrimitiveRegistry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		// TODO(contracts): *environment.Namespace has no ValueType enum
		// entry, so namespace-* primitives fall back to TypeAny for the
		// namespace argument; the impl's RequireArg still rejects mismatches.
		{Name: "namespace?", ParamCount: 1, Impl: PrimNamespaceQ,
			Doc: "Returns #t if OBJ is a namespace (first-class environment).\n\nExamples:\n  (namespace? (make-namespace))  => #t\n  (namespace? 42)                => #f", ParamNames: []string{"obj"}, Category: "namespace",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
		{Name: "namespace-name", ParamCount: 1, Impl: PrimNamespaceName,
			Doc: "Returns the name of NS as a string, or #f if unnamed. A fresh namespace from (make-namespace) is named \"namespace\" by default.\n\nExamples:\n  (namespace-name (make-namespace))  => \"namespace\"", ParamNames: []string{"ns"}, Category: "namespace",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "make-namespace", ParamCount: 1, IsVariadic: true, Impl: PrimMakeNamespace,
			Doc: "Creates a new namespace, optionally pre-loaded with libraries specified by import-specs.\n\nExamples:\n  (make-namespace)                      => #<namespace>\n  (make-namespace '(scheme base))       => #<namespace>  ; with (scheme base) imported", ParamNames: []string{"import-spec"}, Category: "namespace",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "namespace-derive", ParamCount: 1, Impl: PrimNamespaceDerive,
			Doc: "Creates a child namespace that shares interning with NS but has isolated bindings.\n\nExamples:\n  (namespace-derive (make-namespace))  => #<namespace>", ParamNames: []string{"ns"}, Category: "namespace",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "namespace-define!", ParamCount: 3, Impl: PrimNamespaceDefine,
			Doc: "Creates or updates a binding for SYM to VAL in NS.\n\nExamples:\n  (let ((ns (make-namespace))) (namespace-define! ns 'x 42) (namespace-ref ns 'x))  => 42", ParamNames: []string{"ns", "sym", "val"}, Category: "namespace",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeSymbol, values.TypeAny},
			ReturnType: values.TypeVoid},
		{Name: "namespace-ref", ParamCount: 2, IsVariadic: true, Impl: PrimNamespaceRef,
			Doc: "Returns the value bound to SYM in NS. With a third argument, returns it as default if SYM is unbound.\n\nExamples:\n  (let ((ns (make-namespace))) (namespace-define! ns 'x 10) (namespace-ref ns 'x))      => 10\n  (let ((ns (make-namespace))) (namespace-ref ns 'missing 'default))  => default", ParamNames: []string{"ns", "sym"}, Category: "namespace",
			// The rest is (sym [default]): a symbol followed by a value of any
			// type, so no single constraint describes it.
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "namespace-bound?", ParamCount: 2, Impl: PrimNamespaceBound,
			Doc: "Returns #t if SYM has a binding in NS.\n\nExamples:\n  (let ((ns (make-namespace))) (namespace-define! ns 'x 1) (namespace-bound? ns 'x))  => #t", ParamNames: []string{"ns", "sym"}, Category: "namespace",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeSymbol},
			ReturnType: values.TypeBoolean},
		{Name: "namespace-undefine!", ParamCount: 2, Impl: PrimNamespaceUndefine,
			Doc: "Removes the user-level binding for SYM from NS. A sealed binding (a primitive or bootstrap procedure, which lives in the immutable engine-shared base) cannot be undefined and raises an error.\n\nExamples:\n  (let ((ns (make-namespace))) (namespace-define! ns 'x 1) (namespace-undefine! ns 'x) (namespace-bound? ns 'x))  => #f", ParamNames: []string{"ns", "sym"}, Category: "namespace",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeSymbol},
			ReturnType: values.TypeVoid},
		{Name: "namespace-bound-names", ParamCount: 1, Impl: PrimNamespaceBoundNames,
			Doc: "Returns a list of all symbols that have bindings in NS.\n\nExamples:\n  (let ((ns (make-namespace))) (namespace-define! ns 'x 1) (namespace-bound-names ns))  ; => (x)", ParamNames: []string{"ns"}, Category: "namespace",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeList},
		{Name: "namespace-require", ParamCount: 2, Impl: PrimNamespaceRequire,
			Doc: "Dynamically imports a library specified by LIB-SPEC into NS.\n\nExamples:\n  (let ((ns (make-namespace))) (namespace-require ns '(scheme base)))", ParamNames: []string{"ns", "lib-spec"}, Category: "namespace",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeList},
			ReturnType: values.TypeVoid},
	}, registry.PhaseSetRuntime)
	return nil
}
