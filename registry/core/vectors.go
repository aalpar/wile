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
)

func addVectors(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-vector", ParamCount: 2, IsVariadic: true, Impl: PrimMakeVector,
			Doc: "Creates a vector of length k, optionally filled with fill.", ParamNames: []string{"k", "fill"}, Category: "vectors"},
		{Name: "vector", ParamCount: 1, IsVariadic: true, Impl: PrimVector,
			Doc: "Creates a vector from its arguments.", ParamNames: []string{"obj"}, Category: "vectors"},
		{Name: "vector-length", ParamCount: 1, Impl: PrimVectorLength,
			Doc: "Returns the length of vector.", ParamNames: []string{"vector"}, Category: "vectors"},
		{Name: "vector-ref", ParamCount: 2, Impl: PrimVectorRef,
			Doc: "Returns the element at index k.", ParamNames: []string{"vector", "k"}, Category: "vectors"},
		{Name: "vector-set!", ParamCount: 3, Impl: PrimVectorSet,
			Doc: "Sets the element at index k.", ParamNames: []string{"vector", "k", "obj"}, Category: "vectors"},
		{Name: "vector->list", ParamCount: 2, IsVariadic: true, Impl: PrimVectorToList,
			Doc: "Converts a vector to a list.", ParamNames: []string{"vector", "start"}, Category: "vectors"},
		{Name: "list->vector", ParamCount: 1, Impl: PrimListToVector,
			Doc: "Converts a list to a vector.", ParamNames: []string{"list"}, Category: "vectors"},
		{Name: "vector-copy", ParamCount: 2, IsVariadic: true, Impl: PrimVectorCopy,
			Doc: "Returns a copy of vector, optionally from start to end.", ParamNames: []string{"vector", "start"}, Category: "vectors"},
		{Name: "vector-copy!", ParamCount: 3, IsVariadic: true, Impl: PrimVectorCopyTo,
			Doc: "Copies elements from source to destination vector.", ParamNames: []string{"to", "at", "from"}, Category: "vectors"},
		{Name: "vector-fill!", ParamCount: 3, IsVariadic: true, Impl: PrimVectorFill,
			Doc: "Fills vector elements with fill, optionally from start to end.", ParamNames: []string{"vector", "fill", "start"}, Category: "vectors"},
		{Name: "vector-append", ParamCount: 1, IsVariadic: true, Impl: PrimVectorAppend,
			Doc: "Appends vectors together.", ParamNames: []string{"vector"}, Category: "vectors"},
		{Name: "vector->string", ParamCount: 2, IsVariadic: true, Impl: PrimVectorToString,
			Doc: "Converts a vector of characters to a string.", ParamNames: []string{"vector", "start"}, Category: "vectors"},
		{Name: "string->vector", ParamCount: 2, IsVariadic: true, Impl: PrimStringToVector,
			Doc: "Converts a string to a vector of characters.", ParamNames: []string{"string", "start"}, Category: "vectors"},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
