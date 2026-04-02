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

// Package introspection provides read-only environment introspection primitives.
package introspection

import (
	"github.com/aalpar/wile/registry"
)

// Extension is the introspection extension.
var Extension = registry.NewExtension("introspection", AddToRegistry)

// Builder aggregates all introspection registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all introspection primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "interaction-environment", Impl: PrimInteractionEnvironment,
			Doc: "Returns the current top-level interaction environment as an environment object.", Category: "introspection"},
		{Name: "environment?", ParamCount: 1, Impl: PrimEnvironmentQ,
			Doc: "Returns #t if obj is an environment object (created by environment, scheme-report-environment, etc.).", ParamNames: []string{"obj"}, Category: "introspection"},
		{Name: "environment-bound-names", ParamCount: 1, Impl: PrimEnvironmentBoundNames,
			Doc: "Returns a list of all symbols that have bindings in the given environment.", ParamNames: []string{"env"}, Category: "introspection"},
		{Name: "environment-ref", ParamCount: 2, Impl: PrimEnvironmentRef,
			Doc: "Returns the value bound to symbol in the given environment. Raises an error if unbound.", ParamNames: []string{"env", "symbol"}, Category: "introspection"},
		{Name: "environment-bound?", ParamCount: 2, Impl: PrimEnvironmentBoundQ,
			Doc: "Returns #t if symbol has a binding in the given environment.", ParamNames: []string{"env", "symbol"}, Category: "introspection"},
		{Name: "features", Impl: PrimFeatures,
			Doc: "Returns a list of symbols representing implementation features (r7rs, wile, platform, architecture).", Category: "introspection"},
		{Name: "available-libraries", Impl: PrimAvailableLibraries,
			Doc: "Returns a sorted list of all importable library names. Each name is a list of symbols/integers in R7RS library name syntax.", Category: "introspection"},
		{Name: "disassemble", ParamCount: 1, Impl: PrimDisassemble,
			Doc: "Returns structured disassembly of a procedure as a list of alists. " +
				"The first element (car) is always a header alist with metadata " +
				"(type, name, params, variadic, doc). For native closures, the header " +
				"also includes literals and bindings vectors, and remaining elements " +
				"are instruction alists with keys: pc, op, arg, slot, depth, target, " +
				"literal, binding, side-op, source. For case-lambda, the header " +
				"contains a clauses key with per-clause disassemblies. For foreign " +
				"closures, the header is the only element.",
			ParamNames: []string{"proc"}, Category: "introspection"},
	}, registry.PhaseRuntime)
	return nil
}
