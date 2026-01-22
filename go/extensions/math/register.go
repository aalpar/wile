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
	"wile/registry"
	"wile/runtime/primitives"
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
		{"exp", 1, false, primitives.PrimExp},
		{"log", 2, true, primitives.PrimLog},
		{"sin", 1, false, primitives.PrimSin},
		{"cos", 1, false, primitives.PrimCos},
		{"tan", 1, false, primitives.PrimTan},
		{"asin", 1, false, primitives.PrimAsin},
		{"acos", 1, false, primitives.PrimAcos},
		{"atan", 2, true, primitives.PrimAtan},
		{"sqrt", 1, false, primitives.PrimSqrt},
		{"expt", 2, false, primitives.PrimExpt},
		{"square", 1, false, primitives.PrimSquare},
	}, registry.PhaseRuntime)

	// Rounding and division
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"floor", 1, false, primitives.PrimFloor},
		{"ceiling", 1, false, primitives.PrimCeiling},
		{"truncate", 1, false, primitives.PrimTruncate},
		{"round", 1, false, primitives.PrimRound},
		{"floor/", 2, false, primitives.PrimFloorDiv},
		{"floor-quotient", 2, false, primitives.PrimFloorQuotient},
		{"floor-remainder", 2, false, primitives.PrimFloorRemainder},
		{"truncate/", 2, false, primitives.PrimTruncateDiv},
		{"truncate-quotient", 2, false, primitives.PrimTruncateQuotient},
		{"truncate-remainder", 2, false, primitives.PrimTruncateRemainder},
	}, registry.PhaseRuntime)

	// Numeric predicates
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"finite?", 1, false, primitives.PrimFiniteQ},
		{"infinite?", 1, false, primitives.PrimInfiniteQ},
		{"nan?", 1, false, primitives.PrimNanQ},
	}, registry.PhaseRuntime)

	// Rationals and exact
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"numerator", 1, false, primitives.PrimNumerator},
		{"denominator", 1, false, primitives.PrimDenominator},
		{"rationalize", 2, false, primitives.PrimRationalize},
		{"exact-integer-sqrt", 1, false, primitives.PrimExactIntegerSqrt},
	}, registry.PhaseRuntime)

	// Complex
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"make-rectangular", 2, false, primitives.PrimMakeRectangular},
		{"make-polar", 2, false, primitives.PrimMakePolar},
		{"real-part", 1, false, primitives.PrimRealPart},
		{"imag-part", 1, false, primitives.PrimImagPart},
		{"magnitude", 1, false, primitives.PrimMagnitude},
		{"angle", 1, false, primitives.PrimAngle},
	}, registry.PhaseRuntime)

	// Number/string conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"number->string", 2, true, primitives.PrimNumberToString},
		{"string->number", 2, true, primitives.PrimStringToNumber},
	}, registry.PhaseRuntime)

	return nil
}
