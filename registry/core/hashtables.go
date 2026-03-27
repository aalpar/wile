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

func addHashtables(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-hashtable", Impl: PrimMakeHashtable,
			Doc: "Creates a new empty hashtable.", Category: "hashtables",
			ReturnType: values.TypeHashtable},
		{Name: "hashtable?", ParamCount: 1, Impl: PrimHashtableQ,
			Doc: "Returns #t if obj is a hashtable.", ParamNames: []string{"obj"}, Category: "hashtables",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "hashtable-ref", ParamCount: 3, IsVariadic: true, Impl: PrimHashtableRef,
			Doc: "Returns the value for key, or default if not found.", ParamNames: []string{"ht", "key", "default"}, Category: "hashtables",
			ParamTypes: []values.ValueType{values.TypeHashtable, values.TypeAny, values.TypeAny}},
		{Name: "hashtable-set!", ParamCount: 3, Impl: PrimHashtableSet,
			Doc: "Associates key with value in the hashtable.", ParamNames: []string{"ht", "key", "value"}, Category: "hashtables",
			ParamTypes: []values.ValueType{values.TypeHashtable, values.TypeAny, values.TypeAny}, ReturnType: values.TypeVoid},
		{Name: "hashtable-delete!", ParamCount: 2, Impl: PrimHashtableDelete,
			Doc: "Removes the entry for key from the hashtable.", ParamNames: []string{"ht", "key"}, Category: "hashtables",
			ParamTypes: []values.ValueType{values.TypeHashtable, values.TypeAny}, ReturnType: values.TypeVoid},
		{Name: "hashtable-keys", ParamCount: 1, Impl: PrimHashtableKeys,
			Doc: "Returns a list of all keys in the hashtable.", ParamNames: []string{"ht"}, Category: "hashtables",
			ParamTypes: []values.ValueType{values.TypeHashtable}, ReturnType: values.TypeList},
		{Name: "hashtable-values", ParamCount: 1, Impl: PrimHashtableValues,
			Doc: "Returns a list of all values in the hashtable.", ParamNames: []string{"ht"}, Category: "hashtables",
			ParamTypes: []values.ValueType{values.TypeHashtable}, ReturnType: values.TypeList},
		{Name: "hashtable-size", ParamCount: 1, Impl: PrimHashtableSize,
			Doc: "Returns the number of entries in the hashtable.", ParamNames: []string{"ht"}, Category: "hashtables",
			ParamTypes: []values.ValueType{values.TypeHashtable}, ReturnType: values.TypeExactInteger},
		{Name: "hashtable-copy", ParamCount: 1, Impl: PrimHashtableCopy,
			Doc: "Returns a copy of the hashtable.", ParamNames: []string{"ht"}, Category: "hashtables",
			ParamTypes: []values.ValueType{values.TypeHashtable}, ReturnType: values.TypeHashtable},
		{Name: "hashtable-clear!", ParamCount: 1, Impl: PrimHashtableClear,
			Doc: "Removes all entries from the hashtable.", ParamNames: []string{"ht"}, Category: "hashtables",
			ParamTypes: []values.ValueType{values.TypeHashtable}, ReturnType: values.TypeVoid},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
