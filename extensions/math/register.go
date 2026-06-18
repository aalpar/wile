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
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
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
			ParamTypes: []values.TypeConstraint{values.TypeNumber},
			ReturnType: values.TypeNumber},
		{Name: "log", ParamCount: 2, IsVariadic: true, Impl: PrimLog,
			Doc: "Returns the natural logarithm of Z. With two arguments, returns log BASE of Z.", ParamNames: []string{"z", "base"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber, values.TypeNumber},
			ReturnType: values.TypeNumber},
		{Name: "sin", ParamCount: 1, Impl: PrimSin,
			Doc: "Returns the sine of Z in radians. Accepts complex numbers.", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber},
			ReturnType: values.TypeNumber},
		{Name: "cos", ParamCount: 1, Impl: PrimCos,
			Doc: "Returns the cosine of Z in radians. Accepts complex numbers.", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber},
			ReturnType: values.TypeNumber},
		{Name: "tan", ParamCount: 1, Impl: PrimTan,
			Doc: "Returns the tangent of Z in radians. Accepts complex numbers.", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber},
			ReturnType: values.TypeNumber},
		{Name: "asin", ParamCount: 1, Impl: PrimAsin,
			Doc: "Returns the arc sine of Z in radians. May return complex for arguments outside [-1, 1].", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber},
			ReturnType: values.TypeNumber},
		{Name: "acos", ParamCount: 1, Impl: PrimAcos,
			Doc: "Returns the arc cosine of Z in radians. May return complex for arguments outside [-1, 1].", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber},
			ReturnType: values.TypeNumber},
		{Name: "atan", ParamCount: 2, IsVariadic: true, Impl: PrimAtan,
			Doc: "With one argument, returns the arc tangent of Y. With two, returns the angle of the point (X, Y) via atan2.", ParamNames: []string{"y", "x"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber, values.TypeNumber},
			ReturnType: values.TypeNumber},
		{Name: "sqrt", ParamCount: 1, Impl: PrimSqrt,
			Doc: "Returns the square root of Z. Returns a complex number for negative real arguments.", ParamNames: []string{"z"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeNumber},
			ReturnType: values.TypeNumber},
		{Name: "expt", ParamCount: 2, Impl: PrimExpt,
			Doc: "Returns Z1 raised to the power Z2. Uses exact arithmetic when both arguments are exact integers.", ParamNames: []string{"z1", "z2"}, Category: "math",
			Keywords:   []string{"power", "exponentiation", "raise", "pow"},
			ParamTypes: []values.TypeConstraint{values.TypeNumber, values.TypeNumber},
			ReturnType: values.TypeNumber},
	}, registry.PhaseSetRuntime)

	// Rounding and division — real arguments (complex rejected by impl).
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "floor", ParamCount: 1, Impl: PrimFloor,
			Doc: "Returns the largest integer not greater than X (rounds toward -infinity). Preserves exactness.", ParamNames: []string{"x"}, Category: "math",
			Keywords:   []string{"round down", "toward negative infinity"},
			ParamTypes: []values.TypeConstraint{values.TypeReal},
			ReturnType: values.TypeReal},
		{Name: "ceiling", ParamCount: 1, Impl: PrimCeiling,
			Doc: "Returns the smallest integer not less than X (rounds toward +infinity). Preserves exactness.", ParamNames: []string{"x"}, Category: "math",
			Keywords:   []string{"round up", "ceil", "toward positive infinity"},
			ParamTypes: []values.TypeConstraint{values.TypeReal},
			ReturnType: values.TypeReal},
		{Name: "truncate", ParamCount: 1, Impl: PrimTruncate,
			Doc: "Returns the integer closest to X toward zero. Preserves exactness.", ParamNames: []string{"x"}, Category: "math",
			Keywords:   []string{"round toward zero", "trunc", "int"},
			ParamTypes: []values.TypeConstraint{values.TypeReal},
			ReturnType: values.TypeReal},
		{Name: "round", ParamCount: 1, Impl: PrimRound,
			Doc: "Returns the closest integer to X, rounding ties to even (banker's rounding). Preserves exactness.", ParamNames: []string{"x"}, Category: "math",
			Keywords:   []string{"round to nearest", "banker's rounding", "round half even"},
			ParamTypes: []values.TypeConstraint{values.TypeReal},
			ReturnType: values.TypeReal},
		// floor/ returns two values (quotient, remainder) — ReturnType left unset.
		{Name: "floor/", ParamCount: 2, Impl: PrimFloorDiv,
			Doc: "Returns two values: the floor quotient and floor remainder of N1 divided by N2.", ParamNames: []string{"n1", "n2"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeReal, values.TypeReal}},
		{Name: "floor-quotient", ParamCount: 2, Impl: PrimFloorQuotient,
			Doc: "Returns the quotient of N1/N2 rounded toward -infinity.", ParamNames: []string{"n1", "n2"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeReal, values.TypeReal},
			ReturnType: values.TypeReal},
		{Name: "floor-remainder", ParamCount: 2, Impl: PrimFloorRemainder,
			Doc: "Returns the remainder after floor division. The sign matches N2.", ParamNames: []string{"n1", "n2"}, Category: "math",
			Keywords:   []string{"modulo", "mod"},
			ParamTypes: []values.TypeConstraint{values.TypeReal, values.TypeReal},
			ReturnType: values.TypeReal},
		// truncate/ returns two values (quotient, remainder) — ReturnType left unset.
		{Name: "truncate/", ParamCount: 2, Impl: PrimTruncateDiv,
			Doc: "Returns two values: the truncate quotient and truncate remainder of N1 divided by N2.", ParamNames: []string{"n1", "n2"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeReal, values.TypeReal}},
		{Name: "truncate-quotient", ParamCount: 2, Impl: PrimTruncateQuotient,
			Doc: "Returns the quotient of N1/N2 rounded toward zero.", ParamNames: []string{"n1", "n2"}, Category: "math",
			Keywords:   []string{"quotient"},
			ParamTypes: []values.TypeConstraint{values.TypeReal, values.TypeReal},
			ReturnType: values.TypeReal},
		{Name: "truncate-remainder", ParamCount: 2, Impl: PrimTruncateRemainder,
			Doc: "Returns the remainder after truncate division. The sign matches N1.", ParamNames: []string{"n1", "n2"}, Category: "math",
			Keywords:   []string{"remainder"},
			ParamTypes: []values.TypeConstraint{values.TypeReal, values.TypeReal},
			ReturnType: values.TypeReal},
	}, registry.PhaseSetRuntime)

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
	}, registry.PhaseSetRuntime)

	// Rationals and exact
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "numerator", ParamCount: 1, Impl: PrimNumerator,
			Doc: "Returns the numerator of Q in lowest terms. For inexact input, returns an inexact result.", ParamNames: []string{"q"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeReal},
			ReturnType: values.TypeReal},
		{Name: "denominator", ParamCount: 1, Impl: PrimDenominator,
			Doc: "Returns the denominator of Q in lowest terms. For inexact input, returns an inexact result.", ParamNames: []string{"q"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeReal},
			ReturnType: values.TypeReal},
		{Name: "rationalize", ParamCount: 2, Impl: PrimRationalize,
			Doc: "Returns the simplest rational number within distance Y of X, using the Stern-Brocot algorithm.", ParamNames: []string{"x", "y"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeReal, values.TypeReal},
			ReturnType: values.TypeReal},
		// exact-integer-sqrt returns two values (s, r) — ReturnType left unset.
		{Name: "exact-integer-sqrt", ParamCount: 1, Impl: PrimExactIntegerSqrt,
			Doc: "Returns two values s and r such that N = s*s + r and s*s <= N < (s+1)*(s+1). N must be a non-negative exact integer.", ParamNames: []string{"n"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeInteger}},
	}, registry.PhaseSetRuntime)

	// Complex
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-rectangular", ParamCount: 2, Impl: PrimMakeRectangular,
			Doc: "Creates a complex number from real part X1 and imaginary part X2. Uses exact arithmetic when both parts are exact.", ParamNames: []string{"x1", "x2"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeReal, values.TypeReal},
			ReturnType: values.TypeNumber},
		{Name: "make-polar", ParamCount: 2, Impl: PrimMakePolar,
			Doc: "Creates a complex number from MAGNITUDE and ANGLE (in radians).", ParamNames: []string{"magnitude", "angle"}, Category: "math",
			ParamTypes: []values.TypeConstraint{values.TypeReal, values.TypeReal},
			ReturnType: values.TypeComplex},
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
			ReturnType: values.TypeFlonum},
	}, registry.PhaseSetRuntime)

	// Number/string conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "number->string", ParamCount: 2, IsVariadic: true, Impl: PrimNumberToString,
			Doc: "Converts Z to its string representation. Optional RADIX (2, 8, 10, or 16) controls the base for integers.", ParamNames: []string{"z", "radix"}, Category: "math",
			Keywords:   []string{"format number", "serialize", "to string", "radix conversion"},
			ParamTypes: []values.TypeConstraint{values.TypeNumber, values.TypeInteger},
			ReturnType: values.TypeString},
		// string->number returns a Number on success or #f on parse failure —
		// the union is not representable by a single ValueType, so ReturnType
		// is TypeAny.
		{Name: "string->number", ParamCount: 2, IsVariadic: true, Impl: PrimStringToNumber,
			Doc: "Parses STRING as a number. Returns #f if STRING is not a valid number. Optional RADIX overrides the default base 10.", ParamNames: []string{"string", "radix"}, Category: "math",
			Keywords:   []string{"parse number", "deserialize", "from string", "atoi"},
			ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeInteger},
			ReturnType: values.TypeAny},
	}, registry.PhaseSetRuntime)

	// Loss-signal primitives — surface Go big.Accuracy to Scheme.
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "inexact-lossless?", ParamCount: 1, Impl: PrimInexactLosslessQ,
			Doc: "Returns #t if (exact->inexact N) would be lossless (every component exactly representable in float64/complex128). " +
				"For complex N, both real and imaginary parts must be lossless.",
			ParamNames: []string{"n"}, Category: "math",
			Keywords:   []string{"precision", "lossless", "exact", "accuracy", "round-trip"},
			ParamTypes: []values.TypeConstraint{values.TypeNumber},
			ReturnType: values.TypeBoolean},
		// inexact-accuracy returns 1 symbol (real) or 2 (complex) — ReturnType omitted (matches floor/ precedent).
		{Name: "inexact-accuracy", ParamCount: 1, Impl: PrimInexactAccuracy,
			Doc: "Predicts the accuracy of (exact->inexact N) without performing the conversion. " +
				"For real N, returns one of 'below, 'exact, or 'above. " +
				"For complex N, returns two values: (values real-acc imag-acc).",
			ParamNames: []string{"n"}, Category: "math",
			Keywords:   []string{"precision", "accuracy", "below", "exact", "above"},
			ParamTypes: []values.TypeConstraint{values.TypeNumber}},
		// inexact-with-accuracy returns 2 values (real) or 3 (complex).
		{Name: "inexact-with-accuracy", ParamCount: 1, Impl: PrimInexactWithAccuracy,
			Doc: "Returns (exact->inexact N) along with its accuracy. " +
				"For real N, returns two values: (values inexact-n accuracy-sym). " +
				"For complex N, returns three values: (values inexact-c real-acc imag-acc). " +
				"Accuracy symbols are 'below, 'exact, or 'above.",
			ParamNames: []string{"n"}, Category: "math",
			Keywords:   []string{"precision", "convert", "inexact", "accuracy"},
			ParamTypes: []values.TypeConstraint{values.TypeNumber}},
		// complex-inexact-with-accuracy always returns 3 values regardless of input domain.
		{Name: "complex-inexact-with-accuracy", ParamCount: 1, Impl: PrimComplexInexactWithAccuracy,
			Doc: "Returns the complex-domain inexact conversion of N with per-component accuracy. " +
				"Always returns three values: (values inexact-c real-acc imag-acc), " +
				"where real-acc and imag-acc are each one of 'below, 'exact, or 'above. " +
				"For real input N, imag-acc is trivially 'exact.",
			ParamNames: []string{"n"}, Category: "math",
			Keywords:   []string{"complex", "precision", "convert", "inexact", "accuracy"},
			ParamTypes: []values.TypeConstraint{values.TypeNumber}},
	}, registry.PhaseSetRuntime)

	return nil
}
