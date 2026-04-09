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

func addArithmetic(r *registry.Registry) error {
	// Basic arithmetic
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "+", ParamCount: 1, IsVariadic: true, Impl: PrimAdd,
			Doc: "Returns the sum of its arguments. With no arguments, returns 0 (additive identity).\n\nExamples:\n  (+)          => 0\n  (+ 1 2)      => 3\n  (+ 1 2 3 4)  => 10", ParamNames: []string{"z"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeNumber}, ReturnType: values.TypeNumber,
			Keywords: []string{"add", "plus", "sum"}},
		{Name: "-", ParamCount: 2, IsVariadic: true, Impl: PrimSub,
			Doc: "With one argument, returns its negation. With two or more, subtracts all subsequent arguments from the first.\n\nExamples:\n  (- 5)        => -5\n  (- 5 3)      => 2\n  (- 10 3 2)   => 5", ParamNames: []string{"z1", "z2"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeNumber, values.TypeNumber}, ReturnType: values.TypeNumber,
			Keywords: []string{"subtract", "minus", "negate"}},
		{Name: "*", ParamCount: 1, IsVariadic: true, Impl: PrimMul,
			Doc: "Returns the product of its arguments. With no arguments, returns 1 (multiplicative identity).\n\nExamples:\n  (*)          => 1\n  (* 2 3)      => 6\n  (* 2 3 4)    => 24", ParamNames: []string{"z"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeNumber}, ReturnType: values.TypeNumber,
			Keywords: []string{"multiply", "times", "product"}},
		{Name: "/", ParamCount: 2, IsVariadic: true, Impl: PrimDiv,
			Doc: "With one argument, returns its reciprocal. With two or more, divides the first by each subsequent argument. Raises an error on division by zero.\n\nExamples:\n  (/ 2)        => 1/2\n  (/ 6 3)      => 2\n  (/ 12 3 2)   => 2", ParamNames: []string{"z1", "z2"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeNumber, values.TypeNumber}, ReturnType: values.TypeNumber,
			Keywords: []string{"divide", "division", "reciprocal"}},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// Comparisons
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "=", ParamCount: 2, IsVariadic: true, Impl: PrimNumEq,
			Doc: "Returns #t if all arguments are numerically equal. Compares across exactness: (= 1 1.0) is #t.\n\nExamples:\n  (= 1 1)        => #t\n  (= 1 1.0)      => #t\n  (= 1 2)        => #f", ParamNames: []string{"z1", "z2", "zs"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeNumber, values.TypeNumber}, ReturnType: values.TypeBoolean},
		{Name: "<", ParamCount: 2, IsVariadic: true, Impl: PrimNumLt,
			Doc: "Returns #t if each argument is strictly less than the next. Arguments must be real numbers.\n\nExamples:\n  (< 1 2)        => #t\n  (< 1 2 3)      => #t\n  (< 1 2 2)      => #f", ParamNames: []string{"x1", "x2", "xs"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeReal, values.TypeReal}, ReturnType: values.TypeBoolean},
		{Name: ">", ParamCount: 2, IsVariadic: true, Impl: PrimNumGt,
			Doc: "Returns #t if each argument is strictly greater than the next. Arguments must be real numbers.\n\nExamples:\n  (> 3 2)        => #t\n  (> 3 2 1)      => #t\n  (> 3 2 2)      => #f", ParamNames: []string{"x1", "x2", "xs"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeReal, values.TypeReal}, ReturnType: values.TypeBoolean},
		{Name: "<=", ParamCount: 2, IsVariadic: true, Impl: PrimNumLe,
			Doc: "Returns #t if each argument is less than or equal to the next. Arguments must be real numbers.\n\nExamples:\n  (<= 1 2)       => #t\n  (<= 1 1)       => #t\n  (<= 2 1)       => #f", ParamNames: []string{"x1", "x2", "xs"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeReal, values.TypeReal}, ReturnType: values.TypeBoolean},
		{Name: ">=", ParamCount: 2, IsVariadic: true, Impl: PrimNumGe,
			Doc: "Returns #t if each argument is greater than or equal to the next. Arguments must be real numbers.\n\nExamples:\n  (>= 3 2)       => #t\n  (>= 3 3)       => #t\n  (>= 2 3)       => #f", ParamNames: []string{"x1", "x2", "xs"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeReal, values.TypeReal}, ReturnType: values.TypeBoolean},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// Basic numeric operations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "abs", ParamCount: 1, Impl: PrimAbs,
			Doc: "Returns the absolute value of X. The result has the same exactness as the argument.\n\nExamples:\n  (abs -7)       => 7\n  (abs 7)        => 7\n  (abs -3.5)     => 3.5", ParamNames: []string{"x"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeReal}, ReturnType: values.TypeReal},
		{Name: "min", ParamCount: 2, IsVariadic: true, Impl: PrimMin,
			Doc: "Returns the smallest of its arguments. If any argument is inexact, the result is inexact.\n\nExamples:\n  (min 3 1 2)    => 1\n  (min 1 2.0)    => 1.0", ParamNames: []string{"x1", "x2"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeReal, values.TypeReal}, ReturnType: values.TypeReal},
		{Name: "max", ParamCount: 2, IsVariadic: true, Impl: PrimMax,
			Doc: "Returns the largest of its arguments. If any argument is inexact, the result is inexact.\n\nExamples:\n  (max 3 1 2)    => 3\n  (max 1 2.0)    => 2.0", ParamNames: []string{"x1", "x2"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeReal, values.TypeReal}, ReturnType: values.TypeReal},
		// TODO(Phase 4): quotient/remainder/modulo/gcd/lcm contracts declare TypeInteger,
		// but implementations accept inexact integers (e.g., 7.0) via helpers.ExtractInteger.
		// Before enabling runtime enforcement, widen to TypeNumber or introduce TypeIntegerValue.
		{Name: "quotient", ParamCount: 2, Impl: PrimQuotient,
			Doc: "Returns the integer quotient of N1 divided by N2, truncated toward zero. Both arguments must be integers.\n\nExamples:\n  (quotient 7 3)    => 2\n  (quotient -7 3)   => -2\n  (quotient 7 -3)   => -2", ParamNames: []string{"n1", "n2"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeInteger, values.TypeInteger}, ReturnType: values.TypeInteger,
			Keywords: []string{"integer division", "truncate division", "div"}},
		{Name: "remainder", ParamCount: 2, Impl: PrimRemainder,
			Doc: "Returns the remainder of N1 divided by N2. The sign of the result matches the sign of N1.\n\nExamples:\n  (remainder 7 3)    => 1\n  (remainder -7 3)   => -1\n  (remainder 7 -3)   => 1", ParamNames: []string{"n1", "n2"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeInteger, values.TypeInteger}, ReturnType: values.TypeInteger,
			Keywords: []string{"mod", "modular"}},
		{Name: "modulo", ParamCount: 2, Impl: PrimModulo,
			Doc: "Returns N1 modulo N2. The sign of the result matches the sign of N2.\n\nExamples:\n  (modulo 7 3)     => 1\n  (modulo -7 3)    => 2\n  (modulo 7 -3)    => -2", ParamNames: []string{"n1", "n2"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeInteger, values.TypeInteger}, ReturnType: values.TypeInteger,
			Keywords: []string{"mod", "modular arithmetic", "euclidean"}},
		{Name: "gcd", ParamCount: 1, IsVariadic: true, Impl: PrimGcd,
			Doc: "Returns the greatest common divisor of its arguments. With no arguments, returns 0.\n\nExamples:\n  (gcd)            => 0\n  (gcd 12 8)       => 4\n  (gcd 12 8 6)     => 2", ParamNames: []string{"n"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeInteger}, ReturnType: values.TypeInteger,
			Keywords: []string{"greatest common divisor", "common factor", "coprime"}},
		{Name: "lcm", ParamCount: 1, IsVariadic: true, Impl: PrimLcm,
			Doc: "Returns the least common multiple of its arguments. With no arguments, returns 1.\n\nExamples:\n  (lcm)            => 1\n  (lcm 4 6)        => 12\n  (lcm 4 6 10)     => 60", ParamNames: []string{"n"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeInteger}, ReturnType: values.TypeInteger,
			Keywords: []string{"least common multiple"}},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// Exactness conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "exact", ParamCount: 1, Impl: PrimExact,
			Doc: "Converts Z to exact representation. Returns the closest exact number that is numerically equal.\n\nExamples:\n  (exact 1.0)      => 1\n  (exact 1/3)      => 1/3\n  (exact 1.5)      => 3/2", ParamNames: []string{"z"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeNumber}, ReturnType: values.TypeNumber,
			Keywords: []string{"rational", "fraction", "arbitrary precision", "convert"}},
		{Name: "inexact", ParamCount: 1, Impl: PrimInexact,
			Doc: "Converts Z to inexact (floating-point) representation. May lose precision for large exact integers.\n\nExamples:\n  (inexact 1)      => 1.0\n  (inexact 1/3)    => 0.3333333333333333", ParamNames: []string{"z"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeNumber}, ReturnType: values.TypeNumber,
			Keywords: []string{"float", "double", "floating-point", "convert"}},
		{Name: "exact->inexact", ParamCount: 1, Impl: PrimInexact,
			Doc: "R5RS alias for inexact. Converts Z to inexact representation.\n\nExamples:\n  (exact->inexact 1)  => 1.0", ParamNames: []string{"z"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeNumber}, ReturnType: values.TypeNumber,
			Keywords: []string{"float", "double", "floating-point", "convert"}},
		{Name: "inexact->exact", ParamCount: 1, Impl: PrimExact,
			Doc: "R5RS alias for exact. Converts Z to exact representation.\n\nExamples:\n  (inexact->exact 1.5)  => 3/2", ParamNames: []string{"z"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeNumber}, ReturnType: values.TypeNumber,
			Keywords: []string{"rational", "fraction", "convert"}},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
