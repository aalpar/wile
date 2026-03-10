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

// Package math provides transcendental math functions.
//
//nolint:revive // package name conflicts with stdlib
package math

import (
	"github.com/aalpar/wile/registry"
)

// Extension is the math extension.
var Extension = registry.NewExtension("math", AddToRegistry)

// Builder aggregates all math registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all math primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	// Transcendental functions
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "exp", ParamCount: 1, Impl: PrimExp,
			Doc: "Returns e raised to the power z.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "log", ParamCount: 2, IsVariadic: true, Impl: PrimLog,
			Doc: "Returns the natural logarithm, or log base b.", ParamNames: []string{"z", "base"}, Category: "math"},
		{Name: "sin", ParamCount: 1, Impl: PrimSin,
			Doc: "Returns the sine of z.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "cos", ParamCount: 1, Impl: PrimCos,
			Doc: "Returns the cosine of z.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "tan", ParamCount: 1, Impl: PrimTan,
			Doc: "Returns the tangent of z.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "asin", ParamCount: 1, Impl: PrimAsin,
			Doc: "Returns the arc sine of z.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "acos", ParamCount: 1, Impl: PrimAcos,
			Doc: "Returns the arc cosine of z.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "atan", ParamCount: 2, IsVariadic: true, Impl: PrimAtan,
			Doc: "Returns the arc tangent of y/x.", ParamNames: []string{"y", "x"}, Category: "math"},
		{Name: "sqrt", ParamCount: 1, Impl: PrimSqrt,
			Doc: "Returns the square root of z.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "expt", ParamCount: 2, Impl: PrimExpt,
			Doc: "Returns z1 raised to the power z2.", ParamNames: []string{"z1", "z2"}, Category: "math"},
	}, registry.PhaseRuntime)

	// Rounding and division
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "floor", ParamCount: 1, Impl: PrimFloor,
			Doc: "Returns the largest integer not greater than x.", ParamNames: []string{"x"}, Category: "math"},
		{Name: "ceiling", ParamCount: 1, Impl: PrimCeiling,
			Doc: "Returns the smallest integer not less than x.", ParamNames: []string{"x"}, Category: "math"},
		{Name: "truncate", ParamCount: 1, Impl: PrimTruncate,
			Doc: "Returns the integer closest to x toward zero.", ParamNames: []string{"x"}, Category: "math"},
		{Name: "round", ParamCount: 1, Impl: PrimRound,
			Doc: "Returns the closest integer, rounding to even.", ParamNames: []string{"x"}, Category: "math"},
		{Name: "floor/", ParamCount: 2, Impl: PrimFloorDiv,
			Doc: "Returns floor quotient and remainder as two values.", ParamNames: []string{"n1", "n2"}, Category: "math"},
		{Name: "floor-quotient", ParamCount: 2, Impl: PrimFloorQuotient,
			Doc: "Returns the floor quotient of n1 and n2.", ParamNames: []string{"n1", "n2"}, Category: "math"},
		{Name: "floor-remainder", ParamCount: 2, Impl: PrimFloorRemainder,
			Doc: "Returns the floor remainder of n1 and n2.", ParamNames: []string{"n1", "n2"}, Category: "math"},
		{Name: "truncate/", ParamCount: 2, Impl: PrimTruncateDiv,
			Doc: "Returns truncate quotient and remainder as two values.", ParamNames: []string{"n1", "n2"}, Category: "math"},
		{Name: "truncate-quotient", ParamCount: 2, Impl: PrimTruncateQuotient,
			Doc: "Returns the truncate quotient of n1 and n2.", ParamNames: []string{"n1", "n2"}, Category: "math"},
		{Name: "truncate-remainder", ParamCount: 2, Impl: PrimTruncateRemainder,
			Doc: "Returns the truncate remainder of n1 and n2.", ParamNames: []string{"n1", "n2"}, Category: "math"},
	}, registry.PhaseRuntime)

	// Numeric predicates
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "finite?", ParamCount: 1, Impl: PrimFiniteQ,
			Doc: "Returns #t if z is finite.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "infinite?", ParamCount: 1, Impl: PrimInfiniteQ,
			Doc: "Returns #t if z is infinite.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "nan?", ParamCount: 1, Impl: PrimNanQ,
			Doc: "Returns #t if z is NaN.", ParamNames: []string{"z"}, Category: "math"},
	}, registry.PhaseRuntime)

	// Rationals and exact
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "numerator", ParamCount: 1, Impl: PrimNumerator,
			Doc: "Returns the numerator of a rational number.", ParamNames: []string{"q"}, Category: "math"},
		{Name: "denominator", ParamCount: 1, Impl: PrimDenominator,
			Doc: "Returns the denominator of a rational number.", ParamNames: []string{"q"}, Category: "math"},
		{Name: "rationalize", ParamCount: 2, Impl: PrimRationalize,
			Doc: "Returns the simplest rational within tolerance of x.", ParamNames: []string{"x", "y"}, Category: "math"},
		{Name: "exact-integer-sqrt", ParamCount: 1, Impl: PrimExactIntegerSqrt,
			Doc: "Returns s and r where n = s^2 + r.", ParamNames: []string{"n"}, Category: "math"},
	}, registry.PhaseRuntime)

	// Complex
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-rectangular", ParamCount: 2, Impl: PrimMakeRectangular,
			Doc: "Creates a complex number from real and imaginary parts.", ParamNames: []string{"x1", "x2"}, Category: "math"},
		{Name: "make-polar", ParamCount: 2, Impl: PrimMakePolar,
			Doc: "Creates a complex number from magnitude and angle.", ParamNames: []string{"magnitude", "angle"}, Category: "math"},
		{Name: "real-part", ParamCount: 1, Impl: PrimRealPart,
			Doc: "Returns the real part of a complex number.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "imag-part", ParamCount: 1, Impl: PrimImagPart,
			Doc: "Returns the imaginary part of a complex number.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "magnitude", ParamCount: 1, Impl: PrimMagnitude,
			Doc: "Returns the magnitude of a complex number.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "angle", ParamCount: 1, Impl: PrimAngle,
			Doc: "Returns the angle of a complex number.", ParamNames: []string{"z"}, Category: "math"},
	}, registry.PhaseRuntime)

	// Number/string conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "number->string", ParamCount: 2, IsVariadic: true, Impl: PrimNumberToString,
			Doc: "Converts a number to a string with optional radix.", ParamNames: []string{"z", "radix"}, Category: "math"},
		{Name: "string->number", ParamCount: 2, IsVariadic: true, Impl: PrimStringToNumber,
			Doc: "Parses a string as a number with optional radix.", ParamNames: []string{"string", "radix"}, Category: "math"},
	}, registry.PhaseRuntime)

	return nil
}
