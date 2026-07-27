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
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
)

// Extension is the introspection extension.
var Extension = registry.NewDescribedExtension("introspection",
	"Runtime introspection: procedure metadata, disassembly, environment inspection.",
	AddToRegistry)

// Builder aggregates all introspection registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all introspection primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "interaction-environment", Impl: PrimInteractionEnvironment,
			Doc: "Returns the current top-level interaction environment as an environment object.", Category: "introspection",
			ReturnType: values.TypeAny},
		{Name: "environment?", ParamCount: 1, Impl: PrimEnvironmentQ,
			Doc: "Returns #t if OBJ is an environment object (created by environment, scheme-report-environment, etc.).", ParamNames: []string{"obj"}, Category: "introspection",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
		// TODO(contracts): environment-* primitives take *environment.Namespace
		// which is not a ValueType enum. Falling back to TypeAny so enforcement
		// passes; the impl's RequireType still catches mismatches.
		{Name: "environment-bound-names", ParamCount: 1, Impl: PrimEnvironmentBoundNames,
			Doc: "Returns a list of all symbols that have bindings in ENV.", ParamNames: []string{"env"}, Category: "introspection",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeList},
		{Name: "environment-ref", ParamCount: 2, Impl: PrimEnvironmentRef,
			Doc: "Returns the value bound to SYMBOL in ENV. Raises an error if unbound.", ParamNames: []string{"env", "symbol"}, Category: "introspection",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeSymbol}, ReturnType: values.TypeAny},
		{Name: "environment-bound?", ParamCount: 2, Impl: PrimEnvironmentBoundQ,
			Doc: "Returns #t if SYMBOL has a binding in ENV.", ParamNames: []string{"env", "symbol"}, Category: "introspection",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeSymbol},
			ReturnType: values.TypeBoolean},
		{Name: "features", Impl: PrimFeatures,
			Doc: "Returns a list of symbols representing implementation features (r7rs, wile, platform, architecture).", Category: "introspection",
			ReturnType: values.TypeList},
		{Name: "available-libraries", Impl: PrimAvailableLibraries,
			Doc: "Returns a sorted list of all importable library names. Each name is a list of symbols/integers in R7RS library name syntax.", Category: "introspection",
			ReturnType: values.TypeList},
		{Name: "disassemble", ParamCount: 1, Impl: PrimDisassemble,
			Doc: "Returns structured disassembly of a procedure as a list of alists. " +
				"The first element (car) is always a header alist. For native and " +
				"foreign closures it carries type, name, params, variadic, and doc; " +
				"for case-lambda it carries only type and a clauses key holding " +
				"per-clause disassemblies. For native closures, the header also " +
				"includes literals and bindings vectors, and remaining elements " +
				"are instruction alists with keys: pc, op, arg, slot, depth, target, " +
				"literal, binding, side-op, source. For foreign closures, the " +
				"header is the only element.",
			ParamNames: []string{"proc"}, Category: "introspection",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure},
			ReturnType: values.TypeList},
	}, registry.PhaseSetRuntime)
	return nil
}
