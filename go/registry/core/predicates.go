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

//nolint:govet // Using unkeyed struct fields for concise primitive specs
package core

import (
	"wile/registry"
	"wile/runtime/primitives"
)

func addPredicates(r *registry.Registry) error {
	// Type predicates available at both runtime and expand time
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"void?", 1, false, primitives.PrimVoidQ},
		{"null?", 1, false, primitives.PrimNullQ},
		{"pair?", 1, false, primitives.PrimPairQ},
		{"boolean?", 1, false, primitives.PrimBooleanQ},
		{"number?", 1, false, primitives.PrimNumberQ},
		{"integer?", 1, false, primitives.PrimIntegerQ},
		{"real?", 1, false, primitives.PrimRealQ},
		{"rational?", 1, false, primitives.PrimRationalQ},
		{"complex?", 1, false, primitives.PrimComplexQ},
		{"exact?", 1, false, primitives.PrimExactQ},
		{"inexact?", 1, false, primitives.PrimInexactQ},
		{"exact-integer?", 1, false, primitives.PrimExactIntegerQ},
		{"symbol?", 1, false, primitives.PrimSymbolQ},
		{"string?", 1, false, primitives.PrimStringQ},
		{"char?", 1, false, primitives.PrimCharQ},
		{"vector?", 1, false, primitives.PrimVectorQ},
		{"bytevector?", 1, false, primitives.PrimBytevectorQ},
		{"procedure?", 1, false, primitives.PrimProcedureQ},
		{"list?", 1, false, primitives.PrimListQ},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// Numeric predicates
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"zero?", 1, false, primitives.PrimZeroQ},
		{"positive?", 1, false, primitives.PrimPositiveQ},
		{"negative?", 1, false, primitives.PrimNegativeQ},
		{"odd?", 1, false, primitives.PrimOddQ},
		{"even?", 1, false, primitives.PrimEvenQ},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
