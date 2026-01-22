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
)

func addArithmetic(r *registry.Registry) error {
	// Basic arithmetic
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"+", 1, true, PrimAdd},
		{"-", 2, true, PrimSub},
		{"*", 1, true, PrimMul},
		{"/", 2, true, PrimDiv},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// Comparisons
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"=", 2, true, PrimNumEq},
		{"<", 2, true, PrimNumLt},
		{">", 2, true, PrimNumGt},
		{"<=", 2, true, PrimNumLe},
		{">=", 2, true, PrimNumGe},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// Basic numeric operations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"abs", 1, false, PrimAbs},
		{"min", 2, true, PrimMin},
		{"max", 2, true, PrimMax},
		{"quotient", 2, false, PrimQuotient},
		{"remainder", 2, false, PrimRemainder},
		{"modulo", 2, false, PrimModulo},
		{"gcd", 1, true, PrimGcd},
		{"lcm", 1, true, PrimLcm},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// Exactness conversion
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"exact", 1, false, PrimExact},
		{"inexact", 1, false, PrimInexact},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
