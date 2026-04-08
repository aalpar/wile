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
			Doc: "Returns a new empty hashtable using equal? for key comparison.\n\nExamples:\n  (hashtable-size (make-hashtable))  => 0", Category: "hashtables",
			ReturnType: values.TypeHashtable},
		{Name: "hashtable?", ParamCount: 1, Impl: PrimHashtableQ,
			Doc: "Returns #t if OBJ is a hashtable.\n\nExamples:\n  (hashtable? (make-hashtable))  => #t\n  (hashtable? '())               => #f", ParamNames: []string{"obj"}, Category: "hashtables",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "hashtable-ref", ParamCount: 3, IsVariadic: true, Impl: PrimHashtableRef,
			Doc: "Returns the value associated with KEY in HT. If KEY is not found, returns DEFAULT (or raises an error if no default given).\n\nExamples:\n  (let ((ht (make-hashtable))) (hashtable-set! ht 'a 1) (hashtable-ref ht 'a #f))  => 1\n  (hashtable-ref (make-hashtable) 'x 42)  => 42", ParamNames: []string{"ht", "key", "default"}, Category: "hashtables",
			ParamTypes: []values.ValueType{values.TypeHashtable, values.TypeAny, values.TypeAny}},
		{Name: "hashtable-set!", ParamCount: 3, Impl: PrimHashtableSet,
			Doc: "Associates KEY with VALUE in HT, replacing any existing entry for KEY.\n\nExamples:\n  (let ((ht (make-hashtable))) (hashtable-set! ht 'a 1) (hashtable-ref ht 'a #f))  => 1", ParamNames: []string{"ht", "key", "value"}, Category: "hashtables",
			ParamTypes: []values.ValueType{values.TypeHashtable, values.TypeAny, values.TypeAny}, ReturnType: values.TypeVoid},
		{Name: "hashtable-delete!", ParamCount: 2, Impl: PrimHashtableDelete,
			Doc: "Removes the entry for KEY from HT. Does nothing if KEY is not present.\n\nExamples:\n  (let ((ht (make-hashtable))) (hashtable-set! ht 'a 1) (hashtable-delete! ht 'a) (hashtable-size ht))  => 0", ParamNames: []string{"ht", "key"}, Category: "hashtables",
			ParamTypes: []values.ValueType{values.TypeHashtable, values.TypeAny}, ReturnType: values.TypeVoid},
		{Name: "hashtable-keys", ParamCount: 1, Impl: PrimHashtableKeys,
			Doc: "Returns a list of all keys in HT. The order is unspecified.\n\nExamples:\n  (let ((ht (make-hashtable))) (hashtable-set! ht 'a 1) (hashtable-keys ht))  => (a)", ParamNames: []string{"ht"}, Category: "hashtables",
			ParamTypes: []values.ValueType{values.TypeHashtable}, ReturnType: values.TypeList},
		{Name: "hashtable-values", ParamCount: 1, Impl: PrimHashtableValues,
			Doc: "Returns a list of all values in HT. The order corresponds to hashtable-keys.\n\nExamples:\n  (let ((ht (make-hashtable))) (hashtable-set! ht 'a 1) (hashtable-values ht))  => (1)", ParamNames: []string{"ht"}, Category: "hashtables",
			ParamTypes: []values.ValueType{values.TypeHashtable}, ReturnType: values.TypeList},
		{Name: "hashtable-size", ParamCount: 1, Impl: PrimHashtableSize,
			Doc: "Returns the number of key-value pairs in HT.\n\nExamples:\n  (hashtable-size (make-hashtable))  => 0", ParamNames: []string{"ht"}, Category: "hashtables",
			ParamTypes: []values.ValueType{values.TypeHashtable}, ReturnType: values.TypeExactInteger},
		{Name: "hashtable-copy", ParamCount: 1, Impl: PrimHashtableCopy,
			Doc: "Returns a shallow copy of HT. Keys and values are shared with the original.\n\nExamples:\n  (let ((ht (make-hashtable))) (hashtable-set! ht 'a 1) (hashtable-size (hashtable-copy ht)))  => 1", ParamNames: []string{"ht"}, Category: "hashtables",
			ParamTypes: []values.ValueType{values.TypeHashtable}, ReturnType: values.TypeHashtable},
		{Name: "hashtable-clear!", ParamCount: 1, Impl: PrimHashtableClear,
			Doc: "Removes all entries from HT, leaving it empty.\n\nExamples:\n  (let ((ht (make-hashtable))) (hashtable-set! ht 'a 1) (hashtable-clear! ht) (hashtable-size ht))  => 0", ParamNames: []string{"ht"}, Category: "hashtables",
			ParamTypes: []values.ValueType{values.TypeHashtable}, ReturnType: values.TypeVoid},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
