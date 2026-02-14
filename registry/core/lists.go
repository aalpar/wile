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

func addLists(r *registry.Registry) error {
	// List construction
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "list", ParamCount: 1, IsVariadic: true, Impl: PrimList,
			Doc: "Creates a new list from its arguments.", ParamNames: []string{"obj"}, Category: "lists"},
		{Name: "make-list", ParamCount: 2, IsVariadic: true, Impl: PrimMakeList,
			Doc: "Creates a list of length k, optionally filled with fill.", ParamNames: []string{"k", "fill"}, Category: "lists"},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// List operations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "append", ParamCount: 1, IsVariadic: true, Impl: PrimAppend,
			Doc: "Appends lists together.", ParamNames: []string{"list"}, Category: "lists"},
		{Name: "reverse", ParamCount: 1, Impl: PrimReverse,
			Doc: "Returns a newly allocated reversed list.", ParamNames: []string{"list"}, Category: "lists"},
		{Name: "length", ParamCount: 1, Impl: PrimLength,
			Doc: "Returns the length of list.", ParamNames: []string{"list"}, Category: "lists"},
		{Name: "list-ref", ParamCount: 2, Impl: PrimListRef,
			Doc: "Returns the element at index k.", ParamNames: []string{"list", "k"}, Category: "lists"},
		{Name: "list-set!", ParamCount: 3, Impl: PrimListSet,
			Doc: "Sets the element at index k.", ParamNames: []string{"list", "k", "obj"}, Category: "lists"},
		{Name: "list-tail", ParamCount: 2, Impl: PrimListTail,
			Doc: "Returns the sublist after the first k elements.", ParamNames: []string{"list", "k"}, Category: "lists"},
		{Name: "list-copy", ParamCount: 1, Impl: PrimListCopy,
			Doc: "Returns a shallow copy of list.", ParamNames: []string{"list"}, Category: "lists"},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// List search
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "memq", ParamCount: 2, Impl: PrimMemq,
			Doc: "Returns the first sublist whose car is eq? to obj.", ParamNames: []string{"obj", "list"}, Category: "lists"},
		{Name: "memv", ParamCount: 2, Impl: PrimMemv,
			Doc: "Returns the first sublist whose car is eqv? to obj.", ParamNames: []string{"obj", "list"}, Category: "lists"},
		{Name: "member", ParamCount: 3, IsVariadic: true, Impl: PrimMember,
			Doc: "Returns the first sublist whose car is equal? to obj, using optional compare.", ParamNames: []string{"obj", "list", "compare"}, Category: "lists"},
		{Name: "assq", ParamCount: 2, Impl: PrimAssq,
			Doc: "Returns the first pair whose car is eq? to obj.", ParamNames: []string{"obj", "alist"}, Category: "lists"},
		{Name: "assv", ParamCount: 2, Impl: PrimAssv,
			Doc: "Returns the first pair whose car is eqv? to obj.", ParamNames: []string{"obj", "alist"}, Category: "lists"},
		{Name: "assoc", ParamCount: 3, IsVariadic: true, Impl: PrimAssoc,
			Doc: "Returns the first pair whose car is equal? to obj, using optional compare.", ParamNames: []string{"obj", "alist", "compare"}, Category: "lists"},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
