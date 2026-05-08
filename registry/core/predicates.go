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
			Doc: "Returns #t if OBJ is the void value. Void is returned by side-effecting operations like set! and vector-set!.\n\nExamples:\n  (void? (if #f 1))     => #t\n  (void? 42)            => #f", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "null?", ParamCount: 1, Impl: PrimNullQ,
			Doc: "Returns #t if OBJ is the empty list '(). Does not return #t for other falsy values.\n\nExamples:\n  (null? '())           => #t\n  (null? '(1))          => #f\n  (null? #f)            => #f", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "pair?", ParamCount: 1, Impl: PrimPairQ,
			Doc: "Returns #t if OBJ is a pair (cons cell). Note: proper and improper lists both start with a pair; the empty list is not a pair.\n\nExamples:\n  (pair? '(1 2))        => #t\n  (pair? '(1 . 2))      => #t\n  (pair? '())           => #f", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "boolean?", ParamCount: 1, Impl: PrimBooleanQ,
			Doc: "Returns #t if OBJ is #t or #f. No other values are booleans.\n\nExamples:\n  (boolean? #t)         => #t\n  (boolean? #f)         => #t\n  (boolean? 0)          => #f", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "number?", ParamCount: 1, Impl: PrimNumberQ,
			Doc: "Returns #t if OBJ is a number. Includes integers, rationals, reals, and complex numbers.\n\nExamples:\n  (number? 42)          => #t\n  (number? 3.14)        => #t\n  (number? \"hello\")     => #f", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "integer?", ParamCount: 1, Impl: PrimIntegerQ,
			Doc: "Returns #t if OBJ is an integer. Returns #t for both exact integers and inexact integers like 3.0.\n\nExamples:\n  (integer? 42)         => #t\n  (integer? 3.0)        => #t\n  (integer? 3.5)        => #f", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "real?", ParamCount: 1, Impl: PrimRealQ,
			Doc: "Returns #t if OBJ is a real number. All rationals and integers are also real. Complex numbers with zero imaginary part are real.\n\nExamples:\n  (real? 3)             => #t\n  (real? 3.14)          => #t\n  (real? 'a)            => #f", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "rational?", ParamCount: 1, Impl: PrimRationalQ,
			Doc: "Returns #t if OBJ is a rational number. All integers are rational. Inexact reals like +inf.0 and +nan.0 are not rational.\n\nExamples:\n  (rational? 1/3)       => #t\n  (rational? 42)        => #t\n  (rational? +inf.0)    => #f", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "complex?", ParamCount: 1, Impl: PrimComplexQ,
			Doc: "Returns #t if OBJ is a complex number. All real numbers are also complex per R7RS §6.2.\n\nExamples:\n  (complex? 3+4i)       => #t\n  (complex? 42)         => #t\n  (complex? \"hello\")    => #f", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "exact?", ParamCount: 1, Impl: PrimExactQ,
			Doc: "Returns #t if OBJ is an exact number. Exact numbers have unlimited precision. Raises an error if OBJ is not a number.\n\nExamples:\n  (exact? 42)           => #t\n  (exact? 1/3)          => #t\n  (exact? 3.14)         => #f", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "inexact?", ParamCount: 1, Impl: PrimInexactQ,
			Doc: "Returns #t if OBJ is an inexact number. Inexact numbers use floating-point representation. Raises an error if OBJ is not a number.\n\nExamples:\n  (inexact? 3.14)       => #t\n  (inexact? 42)         => #f", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "symbol?", ParamCount: 1, Impl: PrimSymbolQ,
			Doc: "Returns #t if OBJ is a symbol. Symbols are interned names used as identifiers and keys.\n\nExamples:\n  (symbol? 'foo)        => #t\n  (symbol? \"foo\")       => #f", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "string?", ParamCount: 1, Impl: PrimStringQ,
			Doc: "Returns #t if OBJ is a string. Strings are sequences of characters.\n\nExamples:\n  (string? \"hello\")     => #t\n  (string? 'hello)      => #f", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "char?", ParamCount: 1, Impl: PrimCharQ,
			Doc: "Returns #t if OBJ is a character. Characters represent Unicode code points.\n\nExamples:\n  (char? #\\a)           => #t\n  (char? \"a\")           => #f", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "vector?", ParamCount: 1, Impl: PrimVectorQ,
			Doc: "Returns #t if OBJ is a vector. Vectors are fixed-length arrays with O(1) element access.\n\nExamples:\n  (vector? #(1 2 3))    => #t\n  (vector? '(1 2 3))    => #f", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "bytevector?", ParamCount: 1, Impl: PrimBytevectorQ,
			Doc: "Returns #t if OBJ is a bytevector. Bytevectors are fixed-length sequences of bytes (0-255).\n\nExamples:\n  (bytevector? #u8(1 2))  => #t\n  (bytevector? #(1 2))    => #f", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "procedure?", ParamCount: 1, Impl: PrimProcedureQ,
			Doc: "Returns #t if OBJ is a procedure. Includes lambdas, primitives, continuations, and parameters.\n\nExamples:\n  (procedure? car)       => #t\n  (procedure? 42)        => #f", ParamNames: []string{"obj"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean,
			Keywords: []string{"callable", "function", "lambda"}},
	}, registry.PhaseSetRuntime|registry.PhaseSetExpand)

	// Numeric predicates
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "odd?", ParamCount: 1, Impl: PrimOddQ,
			Doc: "Returns #t if N is odd. The argument must be an integer.\n\nExamples:\n  (odd? 3)              => #t\n  (odd? 4)              => #f", ParamNames: []string{"n"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeInteger}, ReturnType: values.TypeBoolean},
		{Name: "even?", ParamCount: 1, Impl: PrimEvenQ,
			Doc: "Returns #t if N is even. The argument must be an integer.\n\nExamples:\n  (even? 4)             => #t\n  (even? 3)             => #f", ParamNames: []string{"n"}, Category: "predicates",
			ParamTypes: []values.TypeConstraint{values.TypeInteger}, ReturnType: values.TypeBoolean},
	}, registry.PhaseSetRuntime|registry.PhaseSetExpand)

	return nil
}
