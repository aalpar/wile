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

func addPairs(r *registry.Registry) error {
	// Basic pair operations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "cons", ParamCount: 2, Impl: PrimCons,
			Doc: "Creates a new pair with OBJ1 as car and OBJ2 as cdr. If OBJ2 is a proper list, the result is a proper list.\n\nExamples:\n  (cons 1 2)        => (1 . 2)\n  (cons 1 '(2 3))   => (1 2 3)\n  (cons 'a '())     => (a)", ParamNames: []string{"obj1", "obj2"}, Category: "pairs",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeAny}, ReturnType: values.TypePair,
			Keywords: []string{"construct pair", "prepend", "cons cell"}},
		{Name: "car", ParamCount: 1, Impl: PrimCar,
			Doc: "Returns the first element (car) of PAIR. Raises an error if the argument is not a pair.\n\nExamples:\n  (car '(1 2 3))    => 1\n  (car '(a . b))    => a", ParamNames: []string{"pair"}, Category: "pairs",
			ParamTypes: []values.ValueType{values.TypePair},
			Keywords:   []string{"first", "head", "left"}},
		{Name: "cdr", ParamCount: 1, Impl: PrimCdr,
			Doc: "Returns the second element (cdr) of PAIR. For a proper list, returns the rest of the list.\n\nExamples:\n  (cdr '(1 2 3))    => (2 3)\n  (cdr '(a . b))    => b", ParamNames: []string{"pair"}, Category: "pairs",
			ParamTypes: []values.ValueType{values.TypePair},
			Keywords:   []string{"rest", "tail", "right"}},
		{Name: "set-car!", ParamCount: 2, Impl: PrimSetCar,
			Doc: "Mutates the car field of PAIR to OBJ. PAIR must be mutable.\n\nExamples:\n  (let ((p (cons 1 2))) (set-car! p 3) p)  => (3 . 2)", ParamNames: []string{"pair", "obj"}, Category: "pairs",
			ParamTypes: []values.ValueType{values.TypePair, values.TypeAny}, ReturnType: values.TypeVoid},
		{Name: "set-cdr!", ParamCount: 2, Impl: PrimSetCdr,
			Doc: "Mutates the cdr field of PAIR to OBJ. PAIR must be mutable.\n\nExamples:\n  (let ((p (cons 1 2))) (set-cdr! p 3) p)  => (1 . 3)", ParamNames: []string{"pair", "obj"}, Category: "pairs",
			ParamTypes: []values.ValueType{values.TypePair, values.TypeAny}, ReturnType: values.TypeVoid},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
