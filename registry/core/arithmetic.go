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
			Doc: "Returns the sum of its arguments.", ParamNames: []string{"z"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeNumber}, ReturnType: values.TypeNumber},
		{Name: "-", ParamCount: 2, IsVariadic: true, Impl: PrimSub,
			Doc: "Subtracts subsequent arguments from the first, or negates.", ParamNames: []string{"z1", "z2"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeNumber, values.TypeNumber}, ReturnType: values.TypeNumber},
		{Name: "*", ParamCount: 1, IsVariadic: true, Impl: PrimMul,
			Doc: "Returns the product of its arguments.", ParamNames: []string{"z"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeNumber}, ReturnType: values.TypeNumber},
		{Name: "/", ParamCount: 2, IsVariadic: true, Impl: PrimDiv,
			Doc: "Divides the first argument by subsequent arguments.", ParamNames: []string{"z1", "z2"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeNumber, values.TypeNumber}, ReturnType: values.TypeNumber},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// Comparisons
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "=", ParamCount: 2, IsVariadic: true, Impl: PrimNumEq,
			Doc: "Returns #t if all arguments are numerically equal.", ParamNames: []string{"z1", "z2", "zs"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeNumber, values.TypeNumber}, ReturnType: values.TypeBoolean},
		{Name: "<", ParamCount: 2, IsVariadic: true, Impl: PrimNumLt,
			Doc: "Returns #t if arguments are monotonically increasing.", ParamNames: []string{"x1", "x2", "xs"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeReal, values.TypeReal}, ReturnType: values.TypeBoolean},
		{Name: ">", ParamCount: 2, IsVariadic: true, Impl: PrimNumGt,
			Doc: "Returns #t if arguments are monotonically decreasing.", ParamNames: []string{"x1", "x2", "xs"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeReal, values.TypeReal}, ReturnType: values.TypeBoolean},
		{Name: "<=", ParamCount: 2, IsVariadic: true, Impl: PrimNumLe,
			Doc: "Returns #t if arguments are monotonically non-decreasing.", ParamNames: []string{"x1", "x2", "xs"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeReal, values.TypeReal}, ReturnType: values.TypeBoolean},
		{Name: ">=", ParamCount: 2, IsVariadic: true, Impl: PrimNumGe,
			Doc: "Returns #t if arguments are monotonically non-increasing.", ParamNames: []string{"x1", "x2", "xs"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeReal, values.TypeReal}, ReturnType: values.TypeBoolean},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// Basic numeric operations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "abs", ParamCount: 1, Impl: PrimAbs,
			Doc: "Returns the absolute value of x.", ParamNames: []string{"x"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeReal}, ReturnType: values.TypeReal},
		{Name: "min", ParamCount: 2, IsVariadic: true, Impl: PrimMin,
			Doc: "Returns the smallest of its arguments.", ParamNames: []string{"x1", "x2"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeReal, values.TypeReal}, ReturnType: values.TypeReal},
		{Name: "max", ParamCount: 2, IsVariadic: true, Impl: PrimMax,
			Doc: "Returns the largest of its arguments.", ParamNames: []string{"x1", "x2"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeReal, values.TypeReal}, ReturnType: values.TypeReal},
		// TODO(Phase 4): quotient/remainder/modulo/gcd/lcm contracts declare TypeInteger,
		// but implementations accept inexact integers (e.g., 7.0) via helpers.ExtractInteger.
		// Before enabling runtime enforcement, widen to TypeNumber or introduce TypeIntegerValue.
		{Name: "quotient", ParamCount: 2, Impl: PrimQuotient,
			Doc: "Returns the integer quotient of n1 and n2.", ParamNames: []string{"n1", "n2"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeInteger, values.TypeInteger}, ReturnType: values.TypeInteger},
		{Name: "remainder", ParamCount: 2, Impl: PrimRemainder,
			Doc: "Returns the integer remainder of n1 and n2.", ParamNames: []string{"n1", "n2"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeInteger, values.TypeInteger}, ReturnType: values.TypeInteger},
		{Name: "modulo", ParamCount: 2, Impl: PrimModulo,
			Doc: "Returns n1 modulo n2.", ParamNames: []string{"n1", "n2"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeInteger, values.TypeInteger}, ReturnType: values.TypeInteger},
		{Name: "gcd", ParamCount: 1, IsVariadic: true, Impl: PrimGcd,
			Doc: "Returns the greatest common divisor.", ParamNames: []string{"n"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeInteger}, ReturnType: values.TypeInteger},
		{Name: "lcm", ParamCount: 1, IsVariadic: true, Impl: PrimLcm,
			Doc: "Returns the least common multiple.", ParamNames: []string{"n"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeInteger}, ReturnType: values.TypeInteger},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// Exactness conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "exact", ParamCount: 1, Impl: PrimExact,
			Doc: "Converts z to an exact number.", ParamNames: []string{"z"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeNumber}, ReturnType: values.TypeNumber},
		{Name: "inexact", ParamCount: 1, Impl: PrimInexact,
			Doc: "Converts z to an inexact number.", ParamNames: []string{"z"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeNumber}, ReturnType: values.TypeNumber},
		{Name: "exact->inexact", ParamCount: 1, Impl: PrimInexact,
			Doc: "Converts z to an inexact number.", ParamNames: []string{"z"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeNumber}, ReturnType: values.TypeNumber},
		{Name: "inexact->exact", ParamCount: 1, Impl: PrimExact,
			Doc: "Converts z to an exact number.", ParamNames: []string{"z"}, Category: "arithmetic",
			ParamTypes: []values.ValueType{values.TypeNumber}, ReturnType: values.TypeNumber},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
