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

func addCharacters(r *registry.Registry) error {
	// Character conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"char->integer", 1, false, primitives.PrimCharToInteger},
		{"integer->char", 1, false, primitives.PrimIntegerToChar},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// Character comparison
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"char=?", 2, true, primitives.PrimCharEqVariadic},
		{"char<?", 2, true, primitives.PrimCharLtVariadic},
		{"char>?", 2, true, primitives.PrimCharGtVariadic},
		{"char<=?", 2, true, primitives.PrimCharLeVariadic},
		{"char>=?", 2, true, primitives.PrimCharGeVariadic},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
