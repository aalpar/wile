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
	"github.com/aalpar/wile/values"
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
	// Transcendental functions — all accept any Number (complex or real).
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "exp", ParamCount: 1, Impl: PrimExp,
			Doc: "Returns e raised to the power Z. Accepts complex numbers.", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber}},
		{Name: "log", ParamCount: 2, IsVariadic: true, Impl: PrimLog,
			Doc: "Returns the natural logarithm of Z. With two arguments, returns log BASE of Z.", ParamNames: []string{"z", "base"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber, values.TypeNumber}},
		{Name: "sin", ParamCount: 1, Impl: PrimSin,
			Doc: "Returns the sine of Z in radians. Accepts complex numbers.", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber}},
		{Name: "cos", ParamCount: 1, Impl: PrimCos,
			Doc: "Returns the cosine of Z in radians. Accepts complex numbers.", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber}},
		{Name: "tan", ParamCount: 1, Impl: PrimTan,
			Doc: "Returns the tangent of Z in radians. Accepts complex numbers.", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber}},
		{Name: "asin", ParamCount: 1, Impl: PrimAsin,
			Doc: "Returns the arc sine of Z in radians. May return complex for arguments outside [-1, 1].", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber}},
		{Name: "acos", ParamCount: 1, Impl: PrimAcos,
			Doc: "Returns the arc cosine of Z in radians. May return complex for arguments outside [-1, 1].", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber}},
		{Name: "atan", ParamCount: 2, IsVariadic: true, Impl: PrimAtan,
			Doc: "With one argument, returns the arc tangent of Y. With two, returns the angle of the point (X, Y) via atan2.", ParamNames: []string{"y", "x"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber, values.TypeNumber}},
		{Name: "sqrt", ParamCount: 1, Impl: PrimSqrt,
			Doc: "Returns the square root of Z. Returns a complex number for negative real arguments.", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber}},
		{Name: "expt", ParamCount: 2, Impl: PrimExpt,
			Doc: "Returns Z1 raised to the power Z2. Uses exact arithmetic when both arguments are exact integers.", ParamNames: []string{"z1", "z2"}, Category: "math",
			Keywords:   []string{"power", "exponentiation", "raise", "pow"},
			ParamTypes: []values.TypeConstraint{values.TypeNumber, values.TypeNumber}},
	}, registry.PhaseRuntime)

	// Rounding and division — real arguments (complex rejected by impl).
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "floor", ParamCount: 1, Impl: PrimFloor,
			Doc: "Returns the largest integer not greater than X (rounds toward -infinity). Preserves exactness.", ParamNames: []string{"x"}, Category: "math",
			Keywords:   []string{"round down", "toward negative infinity"},
			ParamTypes: []values.TypeConstraint{values.TypeReal}},
		{Name: "ceiling", ParamCount: 1, Impl: PrimCeiling,
			Doc: "Returns the smallest integer not less than X (rounds toward +infinity). Preserves exactness.", ParamNames: []string{"x"}, Category: "math",
			Keywords:   []string{"round up", "ceil", "toward positive infinity"},
			ParamTypes: []values.TypeConstraint{values.TypeReal}},
		{Name: "truncate", ParamCount: 1, Impl: PrimTruncate,
			Doc: "Returns the integer closest to X toward zero. Preserves exactness.", ParamNames: []string{"x"}, Category: "math",
			Keywords:   []string{"round toward zero", "trunc", "int"},
			ParamTypes: []values.TypeConstraint{values.TypeReal}},
		{Name: "round", ParamCount: 1, Impl: PrimRound,
			Doc: "Returns the closest integer to X, rounding ties to even (banker's rounding). Preserves exactness.", ParamNames: []string{"x"}, Category: "math",
			Keywords:   []string{"round to nearest", "banker's rounding", "round half even"},
			ParamTypes: []values.TypeConstraint{values.TypeReal}},
		{Name: "floor/", ParamCount: 2, Impl: PrimFloorDiv,
			Doc: "Returns two values: the floor quotient and floor remainder of N1 divided by N2.", ParamNames: []string{"n1", "n2"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeReal, values.TypeReal}},
		{Name: "floor-quotient", ParamCount: 2, Impl: PrimFloorQuotient,
			Doc: "Returns the quotient of N1/N2 rounded toward -infinity.", ParamNames: []string{"n1", "n2"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeReal, values.TypeReal}},
		{Name: "floor-remainder", ParamCount: 2, Impl: PrimFloorRemainder,
			Doc: "Returns the remainder after floor division. The sign matches N2.", ParamNames: []string{"n1", "n2"}, Category: "math",
			Keywords:   []string{"modulo", "mod"},
			ParamTypes: []values.TypeConstraint{values.TypeReal, values.TypeReal}},
		{Name: "truncate/", ParamCount: 2, Impl: PrimTruncateDiv,
			Doc: "Returns two values: the truncate quotient and truncate remainder of N1 divided by N2.", ParamNames: []string{"n1", "n2"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeReal, values.TypeReal}},
		{Name: "truncate-quotient", ParamCount: 2, Impl: PrimTruncateQuotient,
			Doc: "Returns the quotient of N1/N2 rounded toward zero.", ParamNames: []string{"n1", "n2"}, Category: "math",
			Keywords:   []string{"quotient"},
			ParamTypes: []values.TypeConstraint{values.TypeReal, values.TypeReal}},
		{Name: "truncate-remainder", ParamCount: 2, Impl: PrimTruncateRemainder,
			Doc: "Returns the remainder after truncate division. The sign matches N1.", ParamNames: []string{"n1", "n2"}, Category: "math",
			Keywords:   []string{"remainder"},
			ParamTypes: []values.TypeConstraint{values.TypeReal, values.TypeReal}},
	}, registry.PhaseRuntime)

	// Numeric predicates
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "finite?", ParamCount: 1, Impl: PrimFiniteQ,
			Doc: "Returns #t if Z is a finite number (not infinite and not NaN).", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber},
			ReturnType: values.TypeBoolean},
		{Name: "infinite?", ParamCount: 1, Impl: PrimInfiniteQ,
			Doc: "Returns #t if Z is positive or negative infinity.", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber},
			ReturnType: values.TypeBoolean},
		{Name: "nan?", ParamCount: 1, Impl: PrimNanQ,
			Doc: "Returns #t if Z is NaN (not a number). Returns #f for all exact numbers.", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber},
			ReturnType: values.TypeBoolean},
	}, registry.PhaseRuntime)

	// Rationals and exact
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "numerator", ParamCount: 1, Impl: PrimNumerator,
			Doc: "Returns the numerator of Q in lowest terms. For inexact input, returns an inexact result.", ParamNames: []string{"q"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeReal}},
		{Name: "denominator", ParamCount: 1, Impl: PrimDenominator,
			Doc: "Returns the denominator of Q in lowest terms. For inexact input, returns an inexact result.", ParamNames: []string{"q"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeReal}},
		{Name: "rationalize", ParamCount: 2, Impl: PrimRationalize,
			Doc: "Returns the simplest rational number within distance Y of X, using the Stern-Brocot algorithm.", ParamNames: []string{"x", "y"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeReal, values.TypeReal}},
		{Name: "exact-integer-sqrt", ParamCount: 1, Impl: PrimExactIntegerSqrt,
			Doc: "Returns two values s and r such that N = s*s + r and s*s <= N < (s+1)*(s+1). N must be a non-negative exact integer.", ParamNames: []string{"n"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeExactInteger}},
	}, registry.PhaseRuntime)

	// Complex
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-rectangular", ParamCount: 2, Impl: PrimMakeRectangular,
			Doc: "Creates a complex number from real part X1 and imaginary part X2. Uses exact arithmetic when both parts are exact.", ParamNames: []string{"x1", "x2"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeReal, values.TypeReal},
			ReturnType: values.TypeNumber},
		{Name: "make-polar", ParamCount: 2, Impl: PrimMakePolar,
			Doc: "Creates a complex number from MAGNITUDE and ANGLE (in radians).", ParamNames: []string{"magnitude", "angle"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeReal, values.TypeReal},
			ReturnType: values.TypeNumber},
		{Name: "real-part", ParamCount: 1, Impl: PrimRealPart,
			Doc: "Returns the real part of Z. For real numbers, returns Z itself.", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber},
			ReturnType: values.TypeReal},
		{Name: "imag-part", ParamCount: 1, Impl: PrimImagPart,
			Doc: "Returns the imaginary part of Z. For real numbers, returns exact 0.", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber},
			ReturnType: values.TypeReal},
		{Name: "magnitude", ParamCount: 1, Impl: PrimMagnitude,
			Doc: "Returns the magnitude (absolute value) of Z. For real numbers, equivalent to abs.", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber},
			ReturnType: values.TypeReal},
		{Name: "angle", ParamCount: 1, Impl: PrimAngle,
			Doc: "Returns the angle (argument) of Z in radians. For positive reals, returns 0; for negative reals, returns pi.", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber},
			ReturnType: values.TypeReal},
	}, registry.PhaseRuntime)

	// Number/string conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "number->string", ParamCount: 2, IsVariadic: true, Impl: PrimNumberToString,
			Doc: "Converts Z to its string representation. Optional RADIX (2, 8, 10, or 16) controls the base for integers.", ParamNames: []string{"z", "radix"}, Category: "math",
			Keywords:   []string{"format number", "serialize", "to string", "radix conversion"},
			ParamTypes: []values.TypeConstraint{values.TypeNumber, values.TypeInteger},
			ReturnType: values.TypeString},
		{Name: "string->number", ParamCount: 2, IsVariadic: true, Impl: PrimStringToNumber,
			Doc: "Parses STRING as a number. Returns #f if STRING is not a valid number. Optional RADIX overrides the default base 10.", ParamNames: []string{"string", "radix"}, Category: "math",
			Keywords:   []string{"parse number", "deserialize", "from string", "atoi"},
			ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeInteger}},
	}, registry.PhaseRuntime)

	return nil
}
