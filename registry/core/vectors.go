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
			Doc: "Returns a vector of length k. If fill is given, each element is fill; otherwise unspecified.", ParamNames: []string{"k", "fill"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeExactInteger, values.TypeAny}, ReturnType: values.TypeVector},
		{Name: "vector", ParamCount: 1, IsVariadic: true, Impl: PrimVector,
			Doc: "Returns a newly allocated vector whose elements are its arguments.", ParamNames: []string{"obj"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeVector},
		{Name: "vector-length", ParamCount: 1, Impl: PrimVectorLength,
			Doc: "Returns the number of elements in vector.", ParamNames: []string{"vector"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeVector}, ReturnType: values.TypeExactInteger},
		{Name: "vector-ref", ParamCount: 2, Impl: PrimVectorRef,
			Doc: "Returns the element at 0-based index k. Raises an error if k is out of range.", ParamNames: []string{"vector", "k"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeVector, values.TypeExactInteger}},
		{Name: "vector-set!", ParamCount: 3, Impl: PrimVectorSet,
			Doc: "Stores obj at 0-based index k in vector. Raises an error if k is out of range.", ParamNames: []string{"vector", "k", "obj"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeVector, values.TypeExactInteger, values.TypeAny}, ReturnType: values.TypeVoid},
		{Name: "vector->list", ParamCount: 2, IsVariadic: true, Impl: PrimVectorToList,
			Doc: "Returns a list of the vector elements from start to end. Start defaults to 0, end to vector length.", ParamNames: []string{"vector", "start"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeVector, values.TypeExactInteger}, ReturnType: values.TypeList},
		{Name: "list->vector", ParamCount: 1, Impl: PrimListToVector,
			Doc: "Returns a vector whose elements are the elements of list.", ParamNames: []string{"list"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeList}, ReturnType: values.TypeVector},
		{Name: "vector-copy", ParamCount: 2, IsVariadic: true, Impl: PrimVectorCopy,
			Doc: "Returns a fresh copy of vector elements from start to end. Start defaults to 0, end to vector length.", ParamNames: []string{"vector", "start"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeVector, values.TypeExactInteger}, ReturnType: values.TypeVector},
		{Name: "vector-copy!", ParamCount: 3, IsVariadic: true, Impl: PrimVectorCopyTo,
			Doc: "Copies elements from the from vector into the to vector starting at index at. Regions may overlap.", ParamNames: []string{"to", "at", "from"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeVector, values.TypeExactInteger, values.TypeVector}, ReturnType: values.TypeVoid},
		{Name: "vector-fill!", ParamCount: 3, IsVariadic: true, Impl: PrimVectorFill,
			Doc: "Sets all elements of vector from start to end to fill. Start defaults to 0, end to vector length.", ParamNames: []string{"vector", "fill", "start"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeVector, values.TypeAny, values.TypeExactInteger}, ReturnType: values.TypeVoid},
		{Name: "vector-append", ParamCount: 1, IsVariadic: true, Impl: PrimVectorAppend,
			Doc: "Returns a newly allocated vector whose elements are the concatenation of the argument vectors.", ParamNames: []string{"vector"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeVector}, ReturnType: values.TypeVector},
		{Name: "vector->string", ParamCount: 2, IsVariadic: true, Impl: PrimVectorToString,
			Doc: "Returns a string formed from the characters in vector from start to end. Elements must be characters.", ParamNames: []string{"vector", "start"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeVector, values.TypeExactInteger}, ReturnType: values.TypeString},
		{Name: "string->vector", ParamCount: 2, IsVariadic: true, Impl: PrimStringToVector,
			Doc: "Returns a vector of the characters in string from start to end.", ParamNames: []string{"string", "start"}, Category: "vectors",
			ParamTypes: []values.ValueType{values.TypeString, values.TypeExactInteger}, ReturnType: values.TypeVector},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
