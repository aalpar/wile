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
	"github.com/aalpar/wile/registry"
)

func addPredicates(r *registry.Registry) error {
	// Type predicates available at both runtime and expand time
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"void?", 1, false, PrimVoidQ},
		{"null?", 1, false, PrimNullQ},
		{"pair?", 1, false, PrimPairQ},
		{"boolean?", 1, false, PrimBooleanQ},
		{"number?", 1, false, PrimNumberQ},
		{"integer?", 1, false, PrimIntegerQ},
		{"real?", 1, false, PrimRealQ},
		{"rational?", 1, false, PrimRationalQ},
		{"complex?", 1, false, PrimComplexQ},
		{"exact?", 1, false, PrimExactQ},
		{"inexact?", 1, false, PrimInexactQ},
		{"exact-integer?", 1, false, PrimExactIntegerQ},
		{"symbol?", 1, false, PrimSymbolQ},
		{"string?", 1, false, PrimStringQ},
		{"char?", 1, false, PrimCharQ},
		{"vector?", 1, false, PrimVectorQ},
		{"bytevector?", 1, false, PrimBytevectorQ},
		{"procedure?", 1, false, PrimProcedureQ},
		{"list?", 1, false, PrimListQ},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// Numeric predicates
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"zero?", 1, false, PrimZeroQ},
		{"positive?", 1, false, PrimPositiveQ},
		{"negative?", 1, false, PrimNegativeQ},
		{"odd?", 1, false, PrimOddQ},
		{"even?", 1, false, PrimEvenQ},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
