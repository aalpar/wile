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
var Extension = registry.NewDescribedExtension("math",
	"Extended math: trigonometry, logarithms, bitwise operations.",
	AddToRegistry)

// Builder aggregates all math registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all math primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	// Transcendental functions
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "exp", ParamCount: 1, Impl: PrimExp,
			Doc: "Returns e raised to the power z. Accepts complex numbers.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "log", ParamCount: 2, IsVariadic: true, Impl: PrimLog,
			Doc: "Returns the natural logarithm of z. With two arguments, returns log base b of z.", ParamNames: []string{"z", "base"}, Category: "math"},
		{Name: "sin", ParamCount: 1, Impl: PrimSin,
			Doc: "Returns the sine of z in radians. Accepts complex numbers.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "cos", ParamCount: 1, Impl: PrimCos,
			Doc: "Returns the cosine of z in radians. Accepts complex numbers.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "tan", ParamCount: 1, Impl: PrimTan,
			Doc: "Returns the tangent of z in radians. Accepts complex numbers.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "asin", ParamCount: 1, Impl: PrimAsin,
			Doc: "Returns the arc sine of z in radians. May return complex for arguments outside [-1, 1].", ParamNames: []string{"z"}, Category: "math"},
		{Name: "acos", ParamCount: 1, Impl: PrimAcos,
			Doc: "Returns the arc cosine of z in radians. May return complex for arguments outside [-1, 1].", ParamNames: []string{"z"}, Category: "math"},
		{Name: "atan", ParamCount: 2, IsVariadic: true, Impl: PrimAtan,
			Doc: "With one argument, returns the arc tangent of y. With two, returns the angle of the point (x, y) via atan2.", ParamNames: []string{"y", "x"}, Category: "math"},
		{Name: "sqrt", ParamCount: 1, Impl: PrimSqrt,
			Doc: "Returns the square root of z. Returns a complex number for negative real arguments.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "expt", ParamCount: 2, Impl: PrimExpt,
			Doc: "Returns z1 raised to the power z2. Uses exact arithmetic when both arguments are exact integers.", ParamNames: []string{"z1", "z2"}, Category: "math"},
	}, registry.PhaseRuntime)

	// Rounding and division
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "floor", ParamCount: 1, Impl: PrimFloor,
			Doc: "Returns the largest integer not greater than x (rounds toward -infinity). Preserves exactness.", ParamNames: []string{"x"}, Category: "math"},
		{Name: "ceiling", ParamCount: 1, Impl: PrimCeiling,
			Doc: "Returns the smallest integer not less than x (rounds toward +infinity). Preserves exactness.", ParamNames: []string{"x"}, Category: "math"},
		{Name: "truncate", ParamCount: 1, Impl: PrimTruncate,
			Doc: "Returns the integer closest to x toward zero. Preserves exactness.", ParamNames: []string{"x"}, Category: "math"},
		{Name: "round", ParamCount: 1, Impl: PrimRound,
			Doc: "Returns the closest integer to x, rounding ties to even (banker's rounding). Preserves exactness.", ParamNames: []string{"x"}, Category: "math"},
		{Name: "floor/", ParamCount: 2, Impl: PrimFloorDiv,
			Doc: "Returns two values: the floor quotient and floor remainder of n1 divided by n2.", ParamNames: []string{"n1", "n2"}, Category: "math"},
		{Name: "floor-quotient", ParamCount: 2, Impl: PrimFloorQuotient,
			Doc: "Returns the quotient of n1/n2 rounded toward -infinity.", ParamNames: []string{"n1", "n2"}, Category: "math"},
		{Name: "floor-remainder", ParamCount: 2, Impl: PrimFloorRemainder,
			Doc: "Returns the remainder after floor division. The sign matches n2.", ParamNames: []string{"n1", "n2"}, Category: "math"},
		{Name: "truncate/", ParamCount: 2, Impl: PrimTruncateDiv,
			Doc: "Returns two values: the truncate quotient and truncate remainder of n1 divided by n2.", ParamNames: []string{"n1", "n2"}, Category: "math"},
		{Name: "truncate-quotient", ParamCount: 2, Impl: PrimTruncateQuotient,
			Doc: "Returns the quotient of n1/n2 rounded toward zero.", ParamNames: []string{"n1", "n2"}, Category: "math"},
		{Name: "truncate-remainder", ParamCount: 2, Impl: PrimTruncateRemainder,
			Doc: "Returns the remainder after truncate division. The sign matches n1.", ParamNames: []string{"n1", "n2"}, Category: "math"},
	}, registry.PhaseRuntime)

	// Numeric predicates
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "finite?", ParamCount: 1, Impl: PrimFiniteQ,
			Doc: "Returns #t if z is a finite number (not infinite and not NaN).", ParamNames: []string{"z"}, Category: "math"},
		{Name: "infinite?", ParamCount: 1, Impl: PrimInfiniteQ,
			Doc: "Returns #t if z is positive or negative infinity.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "nan?", ParamCount: 1, Impl: PrimNanQ,
			Doc: "Returns #t if z is NaN (not a number). Returns #f for all exact numbers.", ParamNames: []string{"z"}, Category: "math"},
	}, registry.PhaseRuntime)

	// Rationals and exact
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "numerator", ParamCount: 1, Impl: PrimNumerator,
			Doc: "Returns the numerator of q in lowest terms. For inexact input, returns an inexact result.", ParamNames: []string{"q"}, Category: "math"},
		{Name: "denominator", ParamCount: 1, Impl: PrimDenominator,
			Doc: "Returns the denominator of q in lowest terms. For inexact input, returns an inexact result.", ParamNames: []string{"q"}, Category: "math"},
		{Name: "rationalize", ParamCount: 2, Impl: PrimRationalize,
			Doc: "Returns the simplest rational number within distance y of x, using the Stern-Brocot algorithm.", ParamNames: []string{"x", "y"}, Category: "math"},
		{Name: "exact-integer-sqrt", ParamCount: 1, Impl: PrimExactIntegerSqrt,
			Doc: "Returns two values s and r such that n = s*s + r and s*s <= n < (s+1)*(s+1). n must be a non-negative exact integer.", ParamNames: []string{"n"}, Category: "math"},
	}, registry.PhaseRuntime)

	// Complex
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-rectangular", ParamCount: 2, Impl: PrimMakeRectangular,
			Doc: "Creates a complex number from real part x1 and imaginary part x2. Uses exact arithmetic when both parts are exact.", ParamNames: []string{"x1", "x2"}, Category: "math"},
		{Name: "make-polar", ParamCount: 2, Impl: PrimMakePolar,
			Doc: "Creates a complex number from magnitude and angle (in radians).", ParamNames: []string{"magnitude", "angle"}, Category: "math"},
		{Name: "real-part", ParamCount: 1, Impl: PrimRealPart,
			Doc: "Returns the real part of z. For real numbers, returns z itself.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "imag-part", ParamCount: 1, Impl: PrimImagPart,
			Doc: "Returns the imaginary part of z. For real numbers, returns exact 0.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "magnitude", ParamCount: 1, Impl: PrimMagnitude,
			Doc: "Returns the magnitude (absolute value) of z. For real numbers, equivalent to abs.", ParamNames: []string{"z"}, Category: "math"},
		{Name: "angle", ParamCount: 1, Impl: PrimAngle,
			Doc: "Returns the angle (argument) of z in radians. For positive reals, returns 0; for negative reals, returns pi.", ParamNames: []string{"z"}, Category: "math"},
	}, registry.PhaseRuntime)

	// Number/string conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "number->string", ParamCount: 2, IsVariadic: true, Impl: PrimNumberToString,
			Doc: "Converts z to its string representation. Optional radix (2, 8, 10, or 16) controls the base for integers.", ParamNames: []string{"z", "radix"}, Category: "math"},
		{Name: "string->number", ParamCount: 2, IsVariadic: true, Impl: PrimStringToNumber,
			Doc: "Parses string as a number. Returns #f if the string is not a valid number. Optional radix overrides the default base 10.", ParamNames: []string{"string", "radix"}, Category: "math"},
	}, registry.PhaseRuntime)

	return nil
}
