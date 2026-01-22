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

func addStrings(r *registry.Registry) error {
	// String construction
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"string", 1, true, PrimString},
		{"make-string", 2, true, PrimMakeString},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// String access
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"string-length", 1, false, PrimStringLength},
		{"string-ref", 2, false, PrimStringRef},
		{"string-set!", 3, false, PrimStringSet},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// String conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"string->list", 2, true, PrimStringToList},
		{"list->string", 1, false, PrimListToString},
		{"symbol->string", 1, false, PrimSymbolToString},
		{"string->symbol", 1, false, PrimStringToSymbol},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// String operations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"string-append", 1, true, PrimStringAppend},
		{"substring", 3, false, PrimSubstring},
		{"string-copy", 2, true, PrimStringCopy},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// String comparison
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"string=?", 2, true, PrimStringEqVariadic},
		{"string<?", 2, true, PrimStringLtVariadic},
		{"string>?", 2, true, PrimStringGtVariadic},
		{"string<=?", 2, true, PrimStringLeVariadic},
		{"string>=?", 2, true, PrimStringGeVariadic},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
