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
			Doc: "Returns a vector of length K. If FILL is given, each element is FILL; otherwise unspecified.\n\nExamples:\n  (make-vector 3 0)    => #(0 0 0)\n  (make-vector 3 'a)   => #(a a a)", ParamNames: []string{"k", "fill"}, Category: "vectors",
			ParamTypes: []values.TypeConstraint{values.TypeExactInteger, values.TypeAny}, ReturnType: values.TypeVector,
			Keywords: []string{"array", "allocate", "create array"}},
		{Name: "vector", ParamCount: 1, IsVariadic: true, Impl: PrimVector,
			Doc: "Returns a newly allocated vector whose elements are its arguments.\n\nExamples:\n  (vector 1 2 3)       => #(1 2 3)\n  (vector 'a 'b)       => #(a b)", ParamNames: []string{"obj"}, Category: "vectors",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeVector},
		{Name: "vector-length", ParamCount: 1, Impl: PrimVectorLength,
			Doc: "Returns the number of elements in VECTOR.\n\nExamples:\n  (vector-length #(1 2 3))  => 3\n  (vector-length #())       => 0", ParamNames: []string{"vector"}, Category: "vectors",
			ParamTypes: []values.TypeConstraint{values.TypeVector}, ReturnType: values.TypeExactInteger},
		{Name: "vector-ref", ParamCount: 2, Impl: PrimVectorRef,
			Doc: "Returns the element at 0-based index K in VECTOR. Raises an error if K is out of range.\n\nExamples:\n  (vector-ref #(a b c) 0)  => a\n  (vector-ref #(a b c) 2)  => c", ParamNames: []string{"vector", "k"}, Category: "vectors",
			ParamTypes: []values.TypeConstraint{values.TypeVector, values.TypeExactInteger}, ReturnType: values.TypeAny,
			Keywords: []string{"index", "get element", "array access", "nth"}},
		{Name: "vector-set!", ParamCount: 3, Impl: PrimVectorSet,
			Doc: "Stores OBJ at 0-based index K in VECTOR. Raises an error if K is out of range.\n\nExamples:\n  (let ((v (vector 1 2 3))) (vector-set! v 1 'x) v)  => #(1 x 3)", ParamNames: []string{"vector", "k", "obj"}, Category: "vectors",
			ParamTypes: []values.TypeConstraint{values.TypeVector, values.TypeExactInteger, values.TypeAny}, ReturnType: values.TypeVoid},
		{Name: "vector->list", ParamCount: 2, IsVariadic: true, Impl: PrimVectorToList,
			Doc: "Returns a list of the VECTOR elements from START to end. START defaults to 0, end to vector length.\n\nExamples:\n  (vector->list #(a b c))  => (a b c)", ParamNames: []string{"vector", "start"}, Category: "vectors",
			ParamTypes: []values.TypeConstraint{values.TypeVector, values.TypeExactInteger}, ReturnType: values.TypeList},
		{Name: "list->vector", ParamCount: 1, Impl: PrimListToVector,
			Doc: "Returns a vector whose elements are the elements of LIST.\n\nExamples:\n  (list->vector '(a b c))  => #(a b c)", ParamNames: []string{"list"}, Category: "vectors",
			ParamTypes: []values.TypeConstraint{values.TypeList}, ReturnType: values.TypeVector},
		{Name: "vector-copy", ParamCount: 2, IsVariadic: true, Impl: PrimVectorCopy,
			Doc: "Returns a fresh copy of VECTOR elements from START to end. START defaults to 0, end to vector length.\n\nExamples:\n  (vector-copy #(a b c))      => #(a b c)\n  (vector-copy #(a b c) 1 3)  => #(b c)", ParamNames: []string{"vector", "start"}, Category: "vectors",
			ParamTypes: []values.TypeConstraint{values.TypeVector, values.TypeExactInteger}, ReturnType: values.TypeVector},
		{Name: "vector-copy!", ParamCount: 3, IsVariadic: true, Impl: PrimVectorCopyTo,
			Doc: "Copies elements from FROM into TO starting at index AT. Regions may overlap.\n\nExamples:\n  (let ((v (vector 1 2 3 4 5))) (vector-copy! v 0 #(a b c) 0 2) v)  => #(a b 3 4 5)", ParamNames: []string{"to", "at", "from"}, Category: "vectors",
			ParamTypes: []values.TypeConstraint{values.TypeVector, values.TypeExactInteger, values.TypeVector}, ReturnType: values.TypeVoid},
		{Name: "vector-fill!", ParamCount: 3, IsVariadic: true, Impl: PrimVectorFill,
			Doc: "Sets all elements of VECTOR from START to end to FILL. START defaults to 0, end to vector length.\n\nExamples:\n  (let ((v (vector 1 2 3))) (vector-fill! v 0) v)  => #(0 0 0)", ParamNames: []string{"vector", "fill", "start"}, Category: "vectors",
			ParamTypes: []values.TypeConstraint{values.TypeVector, values.TypeAny, values.TypeExactInteger}, ReturnType: values.TypeVoid},
		{Name: "vector-append", ParamCount: 1, IsVariadic: true, Impl: PrimVectorAppend,
			Doc: "Returns a newly allocated vector whose elements are the concatenation of the argument vectors.\n\nExamples:\n  (vector-append #(1 2) #(3 4))  => #(1 2 3 4)", ParamNames: []string{"vector"}, Category: "vectors",
			ParamTypes: []values.TypeConstraint{values.TypeVector}, ReturnType: values.TypeVector},
		{Name: "vector->string", ParamCount: 2, IsVariadic: true, Impl: PrimVectorToString,
			Doc: "Returns a string formed from the characters in VECTOR from START to end. Elements must be characters.\n\nExamples:\n  (vector->string #(#\\a #\\b #\\c))  => \"abc\"", ParamNames: []string{"vector", "start"}, Category: "vectors",
			ParamTypes: []values.TypeConstraint{values.TypeVector, values.TypeExactInteger}, ReturnType: values.TypeString},
		{Name: "string->vector", ParamCount: 2, IsVariadic: true, Impl: PrimStringToVector,
			Doc: "Returns a vector of the characters in STRING from START to end.\n\nExamples:\n  (string->vector \"abc\")  => #(#\\a #\\b #\\c)", ParamNames: []string{"string", "start"}, Category: "vectors",
			ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeExactInteger}, ReturnType: values.TypeVector},
	}, registry.PhaseSetRuntime|registry.PhaseSetExpand)

	return nil
}
