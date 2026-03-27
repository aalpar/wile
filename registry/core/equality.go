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
			Doc: "Returns #t if obj1 and obj2 are the same object (pointer identity). Reliable for symbols, booleans, and the empty list.", ParamNames: []string{"obj1", "obj2"}, Category: "equality",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "eqv?", ParamCount: 2, Impl: PrimEqvQ,
			Doc: "Returns #t if obj1 and obj2 are operationally equivalent. Extends eq? with numeric and character value comparison.", ParamNames: []string{"obj1", "obj2"}, Category: "equality",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "equal?", ParamCount: 2, Impl: PrimEqualQ,
			Doc: "Returns #t if obj1 and obj2 have the same structure and contents. Recursively compares pairs, vectors, strings, and bytevectors.", ParamNames: []string{"obj1", "obj2"}, Category: "equality",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeAny}, ReturnType: values.TypeBoolean},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
