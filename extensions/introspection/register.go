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
			Doc: "Returns the current interaction environment.", Category: "introspection"},
		{Name: "environment?", ParamCount: 1, Impl: PrimEnvironmentQ,
			Doc: "Returns #t if the argument is an environment.", ParamNames: []string{"obj"}, Category: "introspection"},
		{Name: "environment-bound-names", ParamCount: 1, Impl: PrimEnvironmentBoundNames,
			Doc: "Returns a list of all symbols bound in the environment.", ParamNames: []string{"env"}, Category: "introspection"},
		{Name: "environment-ref", ParamCount: 2, Impl: PrimEnvironmentRef,
			Doc: "Returns the value bound to a symbol in the environment.", ParamNames: []string{"env", "symbol"}, Category: "introspection"},
		{Name: "environment-bound?", ParamCount: 2, Impl: PrimEnvironmentBoundQ,
			Doc: "Returns #t if the symbol is bound in the environment.", ParamNames: []string{"env", "symbol"}, Category: "introspection"},
		{Name: "features", Impl: PrimFeatures,
			Doc: "Returns a list of implementation feature symbols.", Category: "introspection"},
	}, registry.PhaseRuntime)
	return nil
}
