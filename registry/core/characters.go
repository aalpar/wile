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

package core

import (
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/values"
)

func addCharacters(r *registry.Registry) error {
	// Character conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "char->integer", ParamCount: 1, Impl: PrimCharToInteger,
			Doc: "Returns the Unicode code point of char.", ParamNames: []string{"char"}, Category: "characters",
			ParamTypes: []values.ValueType{values.TypeCharacter}, ReturnType: values.TypeExactInteger},
		{Name: "integer->char", ParamCount: 1, Impl: PrimIntegerToChar,
			Doc: "Returns the character with the given Unicode code point.", ParamNames: []string{"n"}, Category: "characters",
			ParamTypes: []values.ValueType{values.TypeExactInteger}, ReturnType: values.TypeCharacter},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// Character comparison (generated from charCompareSpecs table)
	charCmpPrims := make([]registry.PrimitiveSpec, len(charCompareSpecs))
	for i, spec := range charCompareSpecs {
		charCmpPrims[i] = registry.PrimitiveSpec{
			Name: spec.name, ParamCount: 2, IsVariadic: true,
			Impl: makeCharComparePrimitive(spec.name, spec.cmp),
			Doc:  "Compares characters.", Category: "characters",
			ParamTypes: []values.ValueType{values.TypeCharacter, values.TypeCharacter},
			ReturnType: values.TypeBoolean,
		}
	}
	r.AddPrimitives(charCmpPrims, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
