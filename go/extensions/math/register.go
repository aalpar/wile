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
//nolint:govet,revive // Using unkeyed struct fields, package name conflicts with stdlib
package math

import (
	"github.com/aalpar/wile/go/registry"
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
		{"exp", 1, false, PrimExp},
		{"log", 2, true, PrimLog},
		{"sin", 1, false, PrimSin},
		{"cos", 1, false, PrimCos},
		{"tan", 1, false, PrimTan},
		{"asin", 1, false, PrimAsin},
		{"acos", 1, false, PrimAcos},
		{"atan", 2, true, PrimAtan},
		{"sqrt", 1, false, PrimSqrt},
		{"expt", 2, false, PrimExpt},
		{"square", 1, false, PrimSquare},
	}, registry.PhaseRuntime)

	// Rounding and division
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"floor", 1, false, PrimFloor},
		{"ceiling", 1, false, PrimCeiling},
		{"truncate", 1, false, PrimTruncate},
		{"round", 1, false, PrimRound},
		{"floor/", 2, false, PrimFloorDiv},
		{"floor-quotient", 2, false, PrimFloorQuotient},
		{"floor-remainder", 2, false, PrimFloorRemainder},
		{"truncate/", 2, false, PrimTruncateDiv},
		{"truncate-quotient", 2, false, PrimTruncateQuotient},
		{"truncate-remainder", 2, false, PrimTruncateRemainder},
	}, registry.PhaseRuntime)

	// Numeric predicates
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"finite?", 1, false, PrimFiniteQ},
		{"infinite?", 1, false, PrimInfiniteQ},
		{"nan?", 1, false, PrimNanQ},
	}, registry.PhaseRuntime)

	// Rationals and exact
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"numerator", 1, false, PrimNumerator},
		{"denominator", 1, false, PrimDenominator},
		{"rationalize", 2, false, PrimRationalize},
		{"exact-integer-sqrt", 1, false, PrimExactIntegerSqrt},
	}, registry.PhaseRuntime)

	// Complex
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"make-rectangular", 2, false, PrimMakeRectangular},
		{"make-polar", 2, false, PrimMakePolar},
		{"real-part", 1, false, PrimRealPart},
		{"imag-part", 1, false, PrimImagPart},
		{"magnitude", 1, false, PrimMagnitude},
		{"angle", 1, false, PrimAngle},
	}, registry.PhaseRuntime)

	// Number/string conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"number->string", 2, true, PrimNumberToString},
		{"string->number", 2, true, PrimStringToNumber},
	}, registry.PhaseRuntime)

	return nil
}
