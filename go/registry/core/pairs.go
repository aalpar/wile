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

func addPairs(r *registry.Registry) error {
	// Basic pair operations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"cons", 2, false, primitives.PrimCons},
		{"car", 1, false, primitives.PrimCar},
		{"cdr", 1, false, primitives.PrimCdr},
		{"set-car!", 2, false, primitives.PrimSetCar},
		{"set-cdr!", 2, false, primitives.PrimSetCdr},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// CxR accessors (2-level)
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"caar", 1, false, primitives.PrimCaar},
		{"cadr", 1, false, primitives.PrimCadr},
		{"cdar", 1, false, primitives.PrimCdar},
		{"cddr", 1, false, primitives.PrimCddr},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// CxR accessors (3-level)
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"caaar", 1, false, primitives.PrimCaaar},
		{"caadr", 1, false, primitives.PrimCaadr},
		{"cadar", 1, false, primitives.PrimCadar},
		{"caddr", 1, false, primitives.PrimCaddr},
		{"cdaar", 1, false, primitives.PrimCdaar},
		{"cdadr", 1, false, primitives.PrimCdadr},
		{"cddar", 1, false, primitives.PrimCddar},
		{"cdddr", 1, false, primitives.PrimCdddr},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// CxR accessors (4-level)
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"caaaar", 1, false, primitives.PrimCaaaar},
		{"caaadr", 1, false, primitives.PrimCaaadr},
		{"caadar", 1, false, primitives.PrimCaadar},
		{"caaddr", 1, false, primitives.PrimCaaddr},
		{"cadaar", 1, false, primitives.PrimCadaar},
		{"cadadr", 1, false, primitives.PrimCadadr},
		{"caddar", 1, false, primitives.PrimCaddar},
		{"cadddr", 1, false, primitives.PrimCadddr},
		{"cdaaar", 1, false, primitives.PrimCdaaar},
		{"cdaadr", 1, false, primitives.PrimCdaadr},
		{"cdadar", 1, false, primitives.PrimCdadar},
		{"cdaddr", 1, false, primitives.PrimCdaddr},
		{"cddaar", 1, false, primitives.PrimCddaar},
		{"cddadr", 1, false, primitives.PrimCddadr},
		{"cdddar", 1, false, primitives.PrimCdddar},
		{"cddddr", 1, false, primitives.PrimCddddr},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
