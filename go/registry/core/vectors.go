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

//nolint:govet // Using unkeyed struct fields for concise primitive specs
package core

import (
	"wile/registry"
	"wile/runtime/primitives"
)

func addVectors(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"make-vector", 2, true, primitives.PrimMakeVector},
		{"vector", 1, true, primitives.PrimVector},
		{"vector-length", 1, false, primitives.PrimVectorLength},
		{"vector-ref", 2, false, primitives.PrimVectorRef},
		{"vector-set!", 3, false, primitives.PrimVectorSet},
		{"vector->list", 1, false, primitives.PrimVectorToList},
		{"list->vector", 1, false, primitives.PrimListToVector},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
