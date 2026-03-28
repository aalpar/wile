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

func addPredicates(r *registry.Registry) error {
	// Type predicates available at both runtime and expand time
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "void?", ParamCount: 1, Impl: PrimVoidQ,
			Doc: "Returns #t if obj is the void value. Void is returned by side-effecting operations like set! and vector-set!.", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "null?", ParamCount: 1, Impl: PrimNullQ,
			Doc: "Returns #t if obj is the empty list '(). Does not return #t for other falsy values.", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "pair?", ParamCount: 1, Impl: PrimPairQ,
			Doc: "Returns #t if obj is a pair (cons cell). Note: proper and improper lists both start with a pair; the empty list is not a pair.", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "boolean?", ParamCount: 1, Impl: PrimBooleanQ,
			Doc: "Returns #t if obj is #t or #f. No other values are booleans.", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "number?", ParamCount: 1, Impl: PrimNumberQ,
			Doc: "Returns #t if obj is a number. Includes integers, rationals, reals, and complex numbers.", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "integer?", ParamCount: 1, Impl: PrimIntegerQ,
			Doc: "Returns #t if obj is an integer. Returns #t for both exact integers and inexact integers like 3.0.", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "real?", ParamCount: 1, Impl: PrimRealQ,
			Doc: "Returns #t if obj is a real number. All rationals and integers are also real. Complex numbers with zero imaginary part are real.", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "rational?", ParamCount: 1, Impl: PrimRationalQ,
			Doc: "Returns #t if obj is a rational number. All integers are rational. Inexact reals like +inf.0 and +nan.0 are not rational.", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "complex?", ParamCount: 1, Impl: PrimComplexQ,
			Doc: "Returns #t if obj is a complex number. All real numbers are also complex per R7RS §6.2.", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "exact?", ParamCount: 1, Impl: PrimExactQ,
			Doc: "Returns #t if obj is an exact number. Exact numbers have unlimited precision. Raises an error if obj is not a number.", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "inexact?", ParamCount: 1, Impl: PrimInexactQ,
			Doc: "Returns #t if obj is an inexact number. Inexact numbers use floating-point representation. Raises an error if obj is not a number.", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "symbol?", ParamCount: 1, Impl: PrimSymbolQ,
			Doc: "Returns #t if obj is a symbol. Symbols are interned names used as identifiers and keys.", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "string?", ParamCount: 1, Impl: PrimStringQ,
			Doc: "Returns #t if obj is a string. Strings are sequences of characters.", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "char?", ParamCount: 1, Impl: PrimCharQ,
			Doc: "Returns #t if obj is a character. Characters represent Unicode code points.", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "vector?", ParamCount: 1, Impl: PrimVectorQ,
			Doc: "Returns #t if obj is a vector. Vectors are fixed-length arrays with O(1) element access.", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "bytevector?", ParamCount: 1, Impl: PrimBytevectorQ,
			Doc: "Returns #t if obj is a bytevector. Bytevectors are fixed-length sequences of bytes (0-255).", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "procedure?", ParamCount: 1, Impl: PrimProcedureQ,
			Doc: "Returns #t if obj is a procedure. Includes lambdas, primitives, continuations, and parameters.", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// Numeric predicates
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "odd?", ParamCount: 1, Impl: PrimOddQ,
			Doc: "Returns #t if n is odd. The argument must be an integer.", ParamNames: []string{"n"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeInteger}, ReturnType: values.TypeBoolean},
		{Name: "even?", ParamCount: 1, Impl: PrimEvenQ,
			Doc: "Returns #t if n is even. The argument must be an integer.", ParamNames: []string{"n"}, Category: "predicates",
			ParamTypes: []values.ValueType{values.TypeInteger}, ReturnType: values.TypeBoolean},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
