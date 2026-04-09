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

func addEquality(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "eq?", ParamCount: 2, Impl: PrimEqQ,
			Doc: "Returns #t if OBJ1 and OBJ2 are the same object (pointer identity). Reliable for symbols, booleans, and the empty list.\n\nExamples:\n  (eq? 'a 'a)           => #t\n  (eq? '() '())         => #t\n  (eq? (list 1) (list 1))  => #f", ParamNames: []string{"obj1", "obj2"}, Category: "equality",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeAny}, ReturnType: values.TypeBoolean,
			Keywords: []string{"identity", "pointer equality", "same object"}},
		{Name: "eqv?", ParamCount: 2, Impl: PrimEqvQ,
			Doc: "Returns #t if OBJ1 and OBJ2 are operationally equivalent. Extends eq? with numeric and character value comparison.\n\nExamples:\n  (eqv? 1 1)            => #t\n  (eqv? #\\a #\\a)        => #t\n  (eqv? 1 1.0)          => #f", ParamNames: []string{"obj1", "obj2"}, Category: "equality",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeAny}, ReturnType: values.TypeBoolean,
			Keywords: []string{"equivalence", "value equality"}},
		{Name: "equal?", ParamCount: 2, Impl: PrimEqualQ,
			Doc: "Returns #t if OBJ1 and OBJ2 have the same structure and contents. Recursively compares pairs, vectors, strings, and bytevectors.\n\nExamples:\n  (equal? '(1 2 3) '(1 2 3))  => #t\n  (equal? \"abc\" \"abc\")         => #t\n  (equal? '(1 2) '(1 3))      => #f", ParamNames: []string{"obj1", "obj2"}, Category: "equality",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeAny}, ReturnType: values.TypeBoolean,
			Keywords: []string{"structural equality", "deep equality", "recursive compare"}},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
