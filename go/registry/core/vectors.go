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
)

func addVectors(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"make-vector", 2, true, PrimMakeVector},
		{"vector", 1, true, PrimVector},
		{"vector-length", 1, false, PrimVectorLength},
		{"vector-ref", 2, false, PrimVectorRef},
		{"vector-set!", 3, false, PrimVectorSet},
		{"vector->list", 2, true, PrimVectorToList},
		{"list->vector", 1, false, PrimListToVector},
		{"vector-copy", 2, true, PrimVectorCopy},
		{"vector-copy!", 3, true, PrimVectorCopyTo},
		{"vector-fill!", 3, true, PrimVectorFill},
		{"vector-append", 1, true, PrimVectorAppend},
		{"vector-map", 2, true, PrimVectorMap},
		{"vector-for-each", 2, true, PrimVectorForEach},
		{"vector->string", 2, true, PrimVectorToString},
		{"string->vector", 2, true, PrimStringToVector},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
