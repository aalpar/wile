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

package envvars

import (
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/values"
)

// Extension is the envvars extension.
var Extension = registry.NewDescribedExtension("envvars",
	"Environment variable access: get-environment-variable, get-environment-variables.",
	AddToRegistry)

// Builder aggregates all envvars registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all envvars primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "get-environment-variable", ParamCount: 1, Impl: PrimGetEnvironmentVariable,
			Doc: "Returns the value of the named environment variable as a string, or #f if not set.", ParamNames: []string{"name"}, Category: "envvars",
			Keywords:   []string{"getenv", "env var", "environment lookup"},
			ParamTypes: []values.TypeConstraint{values.TypeString}, ReturnType: values.TypeAny},
		{Name: "get-environment-variables", Impl: PrimGetEnvironmentVariables,
			Doc: "Returns all environment variables as an association list of (name . value) pairs.", Category: "envvars",
			Keywords:   []string{"environ", "env vars", "all environment"},
			ReturnType: values.TypeList},
	}, registry.PhaseSetRuntime)
	return nil
}
