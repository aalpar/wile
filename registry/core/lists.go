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
			Doc: "Returns a newly allocated proper list of its arguments. (list) with no arguments returns '().\n\nExamples:\n  (list)           => ()\n  (list 1 2 3)     => (1 2 3)\n  (list 'a 'b)     => (a b)", ParamNames: []string{"obj"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeList},
		{Name: "make-list", ParamCount: 2, IsVariadic: true, Impl: PrimMakeList,
			Doc: "Returns a list of length K. If FILL is given, each element is FILL; otherwise elements are unspecified.\n\nExamples:\n  (make-list 3 0)    => (0 0 0)\n  (make-list 3 'a)   => (a a a)", ParamNames: []string{"k", "fill"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeExactInteger, values.TypeAny}, ReturnType: values.TypeList},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// List operations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "append", ParamCount: 1, IsVariadic: true, Impl: PrimAppend,
			Doc: "Returns a list formed by concatenating the argument lists. The last argument may be any object and becomes the cdr of the final pair.\n\nExamples:\n  (append '(1 2) '(3 4))      => (1 2 3 4)\n  (append '(a) '(b) '(c))     => (a b c)\n  (append '(a b) 'c)          => (a b . c)", ParamNames: []string{"list"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeList}, ReturnType: values.TypeList},
		{Name: "reverse", ParamCount: 1, Impl: PrimReverse,
			Doc: "Returns a newly allocated list with the elements of LIST in reverse order. LIST is not modified.\n\nExamples:\n  (reverse '(1 2 3))   => (3 2 1)\n  (reverse '())        => ()", ParamNames: []string{"list"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeList}, ReturnType: values.TypeList},
		{Name: "length", ParamCount: 1, Impl: PrimLength,
			Doc: "Returns the number of elements in LIST. Raises an error if the argument is not a proper list.\n\nExamples:\n  (length '(1 2 3))    => 3\n  (length '())         => 0", ParamNames: []string{"list"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeList}, ReturnType: values.TypeExactInteger},
		{Name: "list-ref", ParamCount: 2, Impl: PrimListRef,
			Doc: "Returns the K-th element of LIST (0-based). Raises an error if K is out of range.\n\nExamples:\n  (list-ref '(a b c) 0)    => a\n  (list-ref '(a b c) 2)    => c", ParamNames: []string{"list", "k"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeList, values.TypeExactInteger},
			Keywords:   []string{"index", "nth", "get element"}},
		{Name: "list-set!", ParamCount: 3, Impl: PrimListSet,
			Doc: "Stores OBJ as the K-th element of LIST (0-based). The pair at position K must be mutable.\n\nExamples:\n  (let ((ls (list 'a 'b 'c))) (list-set! ls 1 'x) ls)  => (a x c)", ParamNames: []string{"list", "k", "obj"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypePair, values.TypeExactInteger, values.TypeAny}, ReturnType: values.TypeVoid},
		{Name: "list-tail", ParamCount: 2, Impl: PrimListTail,
			Doc: "Returns the sublist obtained by dropping the first K elements of LIST. Equivalent to K applications of cdr.\n\nExamples:\n  (list-tail '(a b c d) 2)  => (c d)\n  (list-tail '(a b c) 0)    => (a b c)", ParamNames: []string{"list", "k"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeList, values.TypeExactInteger},
			Keywords:   []string{"drop", "skip", "nthcdr"}},
		{Name: "list-copy", ParamCount: 1, Impl: PrimListCopy,
			Doc: "Returns a shallow copy of LIST. The spine is copied but elements are shared with the original.\n\nExamples:\n  (list-copy '(1 2 3))  => (1 2 3)", ParamNames: []string{"list"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeList}, ReturnType: values.TypeList},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// List search
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "memq", ParamCount: 2, Impl: PrimMemq,
			Doc: "Returns the first sublist of LIST whose car is eq? to OBJ, or #f if not found.\n\nExamples:\n  (memq 'b '(a b c))    => (b c)\n  (memq 'd '(a b c))    => #f", ParamNames: []string{"obj", "list"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeList},
			Keywords:   []string{"find", "search", "contains", "member"}},
		{Name: "memv", ParamCount: 2, Impl: PrimMemv,
			Doc: "Returns the first sublist of LIST whose car is eqv? to OBJ, or #f if not found.\n\nExamples:\n  (memv 2 '(1 2 3))     => (2 3)\n  (memv 4 '(1 2 3))     => #f", ParamNames: []string{"obj", "list"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeList},
			Keywords:   []string{"find", "search", "contains", "member"}},
		{Name: "assq", ParamCount: 2, Impl: PrimAssq,
			Doc: "Returns the first association in ALIST whose car is eq? to OBJ, or #f if not found.\n\nExamples:\n  (assq 'b '((a 1) (b 2) (c 3)))  => (b 2)\n  (assq 'd '((a 1) (b 2)))        => #f", ParamNames: []string{"obj", "alist"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeList},
			Keywords:   []string{"lookup", "dictionary", "association list", "key-value"}},
		{Name: "assv", ParamCount: 2, Impl: PrimAssv,
			Doc: "Returns the first association in ALIST whose car is eqv? to OBJ, or #f if not found.\n\nExamples:\n  (assv 2 '((1 a) (2 b) (3 c)))  => (2 b)\n  (assv 4 '((1 a) (2 b)))        => #f", ParamNames: []string{"obj", "alist"}, Category: "lists",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeList},
			Keywords:   []string{"lookup", "dictionary", "association list", "key-value"}},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
