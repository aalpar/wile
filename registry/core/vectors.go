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

func addVectors(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-vector", ParamCount: 2, IsVariadic: true, Impl: PrimMakeVector,
			Doc: "Creates a vector of length k, optionally filled with fill.", ParamNames: []string{"k", "fill"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeExactInteger, values.TypeAny}, ReturnType: values.TypeVector},
		{Name: "vector", ParamCount: 1, IsVariadic: true, Impl: PrimVector,
			Doc: "Creates a vector from its arguments.", ParamNames: []string{"obj"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeVector},
		{Name: "vector-length", ParamCount: 1, Impl: PrimVectorLength,
			Doc: "Returns the length of vector.", ParamNames: []string{"vector"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeVector}, ReturnType: values.TypeExactInteger},
		{Name: "vector-ref", ParamCount: 2, Impl: PrimVectorRef,
			Doc: "Returns the element at index k.", ParamNames: []string{"vector", "k"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeVector, values.TypeExactInteger}},
		{Name: "vector-set!", ParamCount: 3, Impl: PrimVectorSet,
			Doc: "Sets the element at index k.", ParamNames: []string{"vector", "k", "obj"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeVector, values.TypeExactInteger, values.TypeAny}, ReturnType: values.TypeVoid},
		{Name: "vector->list", ParamCount: 2, IsVariadic: true, Impl: PrimVectorToList,
			Doc: "Converts a vector to a list.", ParamNames: []string{"vector", "start"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeVector, values.TypeExactInteger}, ReturnType: values.TypeList},
		{Name: "list->vector", ParamCount: 1, Impl: PrimListToVector,
			Doc: "Converts a list to a vector.", ParamNames: []string{"list"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeList}, ReturnType: values.TypeVector},
		{Name: "vector-copy", ParamCount: 2, IsVariadic: true, Impl: PrimVectorCopy,
			Doc: "Returns a copy of vector, optionally from start to end.", ParamNames: []string{"vector", "start"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeVector, values.TypeExactInteger}, ReturnType: values.TypeVector},
		{Name: "vector-copy!", ParamCount: 3, IsVariadic: true, Impl: PrimVectorCopyTo,
			Doc: "Copies elements from source to destination vector.", ParamNames: []string{"to", "at", "from"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeVector, values.TypeExactInteger, values.TypeVector}, ReturnType: values.TypeVoid},
		{Name: "vector-fill!", ParamCount: 3, IsVariadic: true, Impl: PrimVectorFill,
			Doc: "Fills vector elements with fill, optionally from start to end.", ParamNames: []string{"vector", "fill", "start"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeVector, values.TypeAny, values.TypeExactInteger}, ReturnType: values.TypeVoid},
		{Name: "vector-append", ParamCount: 1, IsVariadic: true, Impl: PrimVectorAppend,
			Doc: "Appends vectors together.", ParamNames: []string{"vector"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeVector}, ReturnType: values.TypeVector},
		{Name: "vector->string", ParamCount: 2, IsVariadic: true, Impl: PrimVectorToString,
			Doc: "Converts a vector of characters to a string.", ParamNames: []string{"vector", "start"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeVector, values.TypeExactInteger}, ReturnType: values.TypeString},
		{Name: "string->vector", ParamCount: 2, IsVariadic: true, Impl: PrimStringToVector,
			Doc: "Converts a string to a vector of characters.", ParamNames: []string{"string", "start"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeString, values.TypeExactInteger}, ReturnType: values.TypeVector},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
