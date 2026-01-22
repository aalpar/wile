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

func addStrings(r *registry.Registry) error {
	// String construction
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"string", 1, true, primitives.PrimString},
		{"make-string", 2, true, primitives.PrimMakeString},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// String access
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"string-length", 1, false, primitives.PrimStringLength},
		{"string-ref", 2, false, primitives.PrimStringRef},
		{"string-set!", 3, false, primitives.PrimStringSet},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// String conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"string->list", 2, true, primitives.PrimStringToList},
		{"list->string", 1, false, primitives.PrimListToString},
		{"symbol->string", 1, false, primitives.PrimSymbolToString},
		{"string->symbol", 1, false, primitives.PrimStringToSymbol},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// String operations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"string-append", 1, true, primitives.PrimStringAppend},
		{"substring", 3, false, primitives.PrimSubstring},
		{"string-copy", 2, true, primitives.PrimStringCopy},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// String comparison
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"string=?", 2, true, primitives.PrimStringEqVariadic},
		{"string<?", 2, true, primitives.PrimStringLtVariadic},
		{"string>?", 2, true, primitives.PrimStringGtVariadic},
		{"string<=?", 2, true, primitives.PrimStringLeVariadic},
		{"string>=?", 2, true, primitives.PrimStringGeVariadic},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
