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

func addStrings(r *registry.Registry) error {
	// String construction
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "string", ParamCount: 1, IsVariadic: true, Impl: PrimString,
			Doc: "Creates a string from its character arguments.", ParamNames: []string{"char"}, Category: "strings",
			ParamTypes: []values.ValueType{values.TypeCharacter}, ReturnType: values.TypeString},
		{Name: "make-string", ParamCount: 2, IsVariadic: true, Impl: PrimMakeString,
			Doc: "Creates a string of length k, optionally filled with char.", ParamNames: []string{"k", "char"}, Category: "strings",
			ParamTypes: []values.ValueType{values.TypeExactInteger, values.TypeCharacter}, ReturnType: values.TypeString},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// String access
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "string-length", ParamCount: 1, Impl: PrimStringLength,
			Doc: "Returns the length of string.", ParamNames: []string{"string"}, Category: "strings",
			ParamTypes: []values.ValueType{values.TypeString}, ReturnType: values.TypeExactInteger},
		{Name: "string-ref", ParamCount: 2, Impl: PrimStringRef,
			Doc: "Returns the character at index k.", ParamNames: []string{"string", "k"}, Category: "strings",
			ParamTypes: []values.ValueType{values.TypeString, values.TypeExactInteger}, ReturnType: values.TypeCharacter},
		{Name: "string-set!", ParamCount: 3, Impl: PrimStringSet,
			Doc: "Sets the character at index k.", ParamNames: []string{"string", "k", "char"}, Category: "strings",
			ParamTypes: []values.ValueType{values.TypeString, values.TypeExactInteger, values.TypeCharacter}, ReturnType: values.TypeVoid},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// String conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "string->list", ParamCount: 2, IsVariadic: true, Impl: PrimStringToList,
			Doc: "Converts a string to a list of characters.", ParamNames: []string{"string", "start"}, Category: "strings",
			ParamTypes: []values.ValueType{values.TypeString, values.TypeExactInteger}, ReturnType: values.TypeList},
		{Name: "list->string", ParamCount: 1, Impl: PrimListToString,
			Doc: "Converts a list of characters to a string.", ParamNames: []string{"list"}, Category: "strings",
			ParamTypes: []values.ValueType{values.TypeList}, ReturnType: values.TypeString},
		{Name: "symbol->string", ParamCount: 1, Impl: PrimSymbolToString,
			Doc: "Returns the name of a symbol as a string.", ParamNames: []string{"symbol"}, Category: "strings",
			ParamTypes: []values.ValueType{values.TypeSymbol}, ReturnType: values.TypeString},
		{Name: "string->symbol", ParamCount: 1, Impl: PrimStringToSymbol,
			Doc: "Returns the symbol with the given name.", ParamNames: []string{"string"}, Category: "strings",
			ParamTypes: []values.ValueType{values.TypeString}, ReturnType: values.TypeSymbol},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// String operations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "string-append", ParamCount: 1, IsVariadic: true, Impl: PrimStringAppend,
			Doc: "Appends strings together.", ParamNames: []string{"string"}, Category: "strings",
			ParamTypes: []values.ValueType{values.TypeString}, ReturnType: values.TypeString},
		{Name: "substring", ParamCount: 3, Impl: PrimSubstring,
			Doc: "Returns a substring from start to end.", ParamNames: []string{"string", "start", "end"}, Category: "strings",
			ParamTypes: []values.ValueType{values.TypeString, values.TypeExactInteger, values.TypeExactInteger}, ReturnType: values.TypeString},
		{Name: "string-copy", ParamCount: 2, IsVariadic: true, Impl: PrimStringCopy,
			Doc: "Returns a copy of string, optionally from start to end.", ParamNames: []string{"string", "start"}, Category: "strings",
			ParamTypes: []values.ValueType{values.TypeString, values.TypeExactInteger}, ReturnType: values.TypeString},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// String comparison (generated from stringCompareSpecs table)
	stringCmpPrims := make([]registry.PrimitiveSpec, len(stringCompareSpecs))
	for i, spec := range stringCompareSpecs {
		stringCmpPrims[i] = registry.PrimitiveSpec{
			Name: spec.name, ParamCount: 2, IsVariadic: true,
			Impl: makeStringComparePrimitive(spec.name, spec.cmp),
			Doc:  "Compares strings lexicographically.", Category: "strings",
			ParamTypes: []values.ValueType{values.TypeString, values.TypeString},
			ReturnType: values.TypeBoolean,
		}
	}
	r.AddPrimitives(stringCmpPrims, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
