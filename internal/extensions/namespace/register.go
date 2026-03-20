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
	"github.com/aalpar/wile/registry"
)

// Extension is the namespace extension.
var Extension = registry.NewExtension("namespace", AddToRegistry)

// Builder aggregates all namespace registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all namespace primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "namespace?", ParamCount: 1, Impl: PrimNamespaceQ,
			Doc: "Returns #t if the argument is a namespace.", ParamNames: []string{"obj"}, Category: "namespace"},
		{Name: "namespace-name", ParamCount: 1, Impl: PrimNamespaceName,
			Doc: "Returns the name of a namespace, or #f if unnamed.", ParamNames: []string{"ns"}, Category: "namespace"},
		{Name: "make-namespace", ParamCount: 1, IsVariadic: true, Impl: PrimMakeNamespace,
			Doc: "Creates a namespace, optionally pre-loaded with libraries.", ParamNames: []string{"import-spec"}, Category: "namespace"},
		{Name: "namespace-derive", ParamCount: 1, Impl: PrimNamespaceDerive,
			Doc: "Creates a child namespace with shared interning, isolated bindings.", ParamNames: []string{"ns"}, Category: "namespace"},
		{Name: "namespace-define!", ParamCount: 3, Impl: PrimNamespaceDefine,
			Doc: "Creates or updates a binding in the namespace.", ParamNames: []string{"ns", "sym", "val"}, Category: "namespace"},
		{Name: "namespace-ref", ParamCount: 2, IsVariadic: true, Impl: PrimNamespaceRef,
			Doc: "Looks up a binding by symbol, with optional default.", ParamNames: []string{"ns", "sym"}, Category: "namespace"},
		{Name: "namespace-bound?", ParamCount: 2, Impl: PrimNamespaceBound,
			Doc: "Returns #t if the symbol is bound in the namespace.", ParamNames: []string{"ns", "sym"}, Category: "namespace"},
		{Name: "namespace-undefine!", ParamCount: 2, Impl: PrimNamespaceUndefine,
			Doc: "Removes a binding from the namespace.", ParamNames: []string{"ns", "sym"}, Category: "namespace"},
		{Name: "namespace-bound-names", ParamCount: 1, Impl: PrimNamespaceBoundNames,
			Doc: "Returns a list of all bound symbols in the namespace.", ParamNames: []string{"ns"}, Category: "namespace"},
		{Name: "namespace-require", ParamCount: 2, Impl: PrimNamespaceRequire,
			Doc: "Dynamically loads a library into the namespace.", ParamNames: []string{"ns", "lib-spec"}, Category: "namespace"},
	}, registry.PhaseRuntime)
	return nil
}
