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

func addLists(r *registry.Registry) error {
	// List construction
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "list", ParamCount: 1, IsVariadic: true, Impl: PrimList,
			Doc: "Returns a newly allocated proper list of its arguments. (list) with no arguments returns '().", ParamNames: []string{"obj"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeList},
		{Name: "make-list", ParamCount: 2, IsVariadic: true, Impl: PrimMakeList,
			Doc: "Returns a list of length k. If fill is given, each element is fill; otherwise elements are unspecified.", ParamNames: []string{"k", "fill"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeExactInteger, values.TypeAny}, ReturnType: values.TypeList},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// List operations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "append", ParamCount: 1, IsVariadic: true, Impl: PrimAppend,
			Doc: "Returns a list formed by concatenating the argument lists. The last argument may be any object and becomes the cdr of the final pair.", ParamNames: []string{"list"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeList}, ReturnType: values.TypeList},
		{Name: "reverse", ParamCount: 1, Impl: PrimReverse,
			Doc: "Returns a newly allocated list with elements in reverse order. The original list is not modified.", ParamNames: []string{"list"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeList}, ReturnType: values.TypeList},
		{Name: "length", ParamCount: 1, Impl: PrimLength,
			Doc: "Returns the number of elements in list. Raises an error if the argument is not a proper list.", ParamNames: []string{"list"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeList}, ReturnType: values.TypeExactInteger},
		{Name: "list-ref", ParamCount: 2, Impl: PrimListRef,
			Doc: "Returns the k-th element of list (0-based). Raises an error if k is out of range.", ParamNames: []string{"list", "k"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeList, values.TypeExactInteger}},
		{Name: "list-set!", ParamCount: 3, Impl: PrimListSet,
			Doc: "Stores obj as the k-th element of list (0-based). The pair at position k must be mutable.", ParamNames: []string{"list", "k", "obj"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypePair, values.TypeExactInteger, values.TypeAny}, ReturnType: values.TypeVoid},
		{Name: "list-tail", ParamCount: 2, Impl: PrimListTail,
			Doc: "Returns the sublist obtained by dropping the first k elements. Equivalent to k applications of cdr.", ParamNames: []string{"list", "k"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeList, values.TypeExactInteger}},
		{Name: "list-copy", ParamCount: 1, Impl: PrimListCopy,
			Doc: "Returns a shallow copy of list. The spine is copied but elements are shared with the original.", ParamNames: []string{"list"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeList}, ReturnType: values.TypeList},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// List search
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "memq", ParamCount: 2, Impl: PrimMemq,
			Doc: "Returns the first sublist of list whose car is eq? to obj, or #f if not found.", ParamNames: []string{"obj", "list"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeList}},
		{Name: "memv", ParamCount: 2, Impl: PrimMemv,
			Doc: "Returns the first sublist of list whose car is eqv? to obj, or #f if not found.", ParamNames: []string{"obj", "list"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeList}},
		{Name: "assq", ParamCount: 2, Impl: PrimAssq,
			Doc: "Returns the first association in alist whose car is eq? to obj, or #f if not found.", ParamNames: []string{"obj", "alist"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeList}},
		{Name: "assv", ParamCount: 2, Impl: PrimAssv,
			Doc: "Returns the first association in alist whose car is eqv? to obj, or #f if not found.", ParamNames: []string{"obj", "alist"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeList}},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
