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
			Doc: "Returns #t if obj is a namespace (first-class environment).", ParamNames: []string{"obj"}, Category: "namespace"},
		{Name: "namespace-name", ParamCount: 1, Impl: PrimNamespaceName,
			Doc: "Returns the name of namespace as a string, or #f if unnamed.", ParamNames: []string{"ns"}, Category: "namespace"},
		{Name: "make-namespace", ParamCount: 1, IsVariadic: true, Impl: PrimMakeNamespace,
			Doc: "Creates a new namespace, optionally pre-loaded with libraries specified by import-specs.", ParamNames: []string{"import-spec"}, Category: "namespace"},
		{Name: "namespace-derive", ParamCount: 1, Impl: PrimNamespaceDerive,
			Doc: "Creates a child namespace that shares interning with ns but has isolated bindings.", ParamNames: []string{"ns"}, Category: "namespace"},
		{Name: "namespace-define!", ParamCount: 3, Impl: PrimNamespaceDefine,
			Doc: "Creates or updates a binding for sym to val in namespace ns.", ParamNames: []string{"ns", "sym", "val"}, Category: "namespace"},
		{Name: "namespace-ref", ParamCount: 2, IsVariadic: true, Impl: PrimNamespaceRef,
			Doc: "Returns the value bound to sym in ns. With a third argument, returns it as default if sym is unbound.", ParamNames: []string{"ns", "sym"}, Category: "namespace"},
		{Name: "namespace-bound?", ParamCount: 2, Impl: PrimNamespaceBound,
			Doc: "Returns #t if sym has a binding in namespace ns.", ParamNames: []string{"ns", "sym"}, Category: "namespace"},
		{Name: "namespace-undefine!", ParamCount: 2, Impl: PrimNamespaceUndefine,
			Doc: "Removes the binding for sym from namespace ns.", ParamNames: []string{"ns", "sym"}, Category: "namespace"},
		{Name: "namespace-bound-names", ParamCount: 1, Impl: PrimNamespaceBoundNames,
			Doc: "Returns a list of all symbols that have bindings in namespace ns.", ParamNames: []string{"ns"}, Category: "namespace"},
		{Name: "namespace-require", ParamCount: 2, Impl: PrimNamespaceRequire,
			Doc: "Dynamically imports a library specified by lib-spec into namespace ns.", ParamNames: []string{"ns", "lib-spec"}, Category: "namespace"},
	}, registry.PhaseRuntime)
	return nil
}
