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

func addPredicates(r *registry.Registry) error {
	// Type predicates available at both runtime and expand time
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "void?", ParamCount: 1, Impl: PrimVoidQ,
			Doc: "Returns #t if obj is void.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "null?", ParamCount: 1, Impl: PrimNullQ,
			Doc: "Returns #t if obj is the empty list.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "pair?", ParamCount: 1, Impl: PrimPairQ,
			Doc: "Returns #t if obj is a pair.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "boolean?", ParamCount: 1, Impl: PrimBooleanQ,
			Doc: "Returns #t if obj is a boolean.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "number?", ParamCount: 1, Impl: PrimNumberQ,
			Doc: "Returns #t if obj is a number.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "integer?", ParamCount: 1, Impl: PrimIntegerQ,
			Doc: "Returns #t if obj is an integer.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "real?", ParamCount: 1, Impl: PrimRealQ,
			Doc: "Returns #t if obj is a real number.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "rational?", ParamCount: 1, Impl: PrimRationalQ,
			Doc: "Returns #t if obj is a rational number.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "complex?", ParamCount: 1, Impl: PrimComplexQ,
			Doc: "Returns #t if obj is a complex number.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "exact?", ParamCount: 1, Impl: PrimExactQ,
			Doc: "Returns #t if obj is exact.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "inexact?", ParamCount: 1, Impl: PrimInexactQ,
			Doc: "Returns #t if obj is inexact.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "exact-integer?", ParamCount: 1, Impl: PrimExactIntegerQ,
			Doc: "Returns #t if obj is an exact integer.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "symbol?", ParamCount: 1, Impl: PrimSymbolQ,
			Doc: "Returns #t if obj is a symbol.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "string?", ParamCount: 1, Impl: PrimStringQ,
			Doc: "Returns #t if obj is a string.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "char?", ParamCount: 1, Impl: PrimCharQ,
			Doc: "Returns #t if obj is a character.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "vector?", ParamCount: 1, Impl: PrimVectorQ,
			Doc: "Returns #t if obj is a vector.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "bytevector?", ParamCount: 1, Impl: PrimBytevectorQ,
			Doc: "Returns #t if obj is a bytevector.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "procedure?", ParamCount: 1, Impl: PrimProcedureQ,
			Doc: "Returns #t if obj is a procedure.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "list?", ParamCount: 1, Impl: PrimListQ,
			Doc: "Returns #t if obj is a proper list.", ParamNames: []string{"obj"}, Category: "predicates"},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// Numeric predicates
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "zero?", ParamCount: 1, Impl: PrimZeroQ,
			Doc: "Returns #t if z is zero.", ParamNames: []string{"z"}, Category: "predicates"},
		{Name: "positive?", ParamCount: 1, Impl: PrimPositiveQ,
			Doc: "Returns #t if x is positive.", ParamNames: []string{"x"}, Category: "predicates"},
		{Name: "negative?", ParamCount: 1, Impl: PrimNegativeQ,
			Doc: "Returns #t if x is negative.", ParamNames: []string{"x"}, Category: "predicates"},
		{Name: "odd?", ParamCount: 1, Impl: PrimOddQ,
			Doc: "Returns #t if n is odd.", ParamNames: []string{"n"}, Category: "predicates"},
		{Name: "even?", ParamCount: 1, Impl: PrimEvenQ,
			Doc: "Returns #t if n is even.", ParamNames: []string{"n"}, Category: "predicates"},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
