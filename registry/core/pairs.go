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

func addPairs(r *registry.Registry) error {
	// Basic pair operations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"cons", 2, false, PrimCons},
		{"car", 1, false, PrimCar},
		{"cdr", 1, false, PrimCdr},
		{"set-car!", 2, false, PrimSetCar},
		{"set-cdr!", 2, false, PrimSetCdr},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// CxR accessors (2-level)
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"caar", 1, false, PrimCaar},
		{"cadr", 1, false, PrimCadr},
		{"cdar", 1, false, PrimCdar},
		{"cddr", 1, false, PrimCddr},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// CxR accessors (3-level)
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"caaar", 1, false, PrimCaaar},
		{"caadr", 1, false, PrimCaadr},
		{"cadar", 1, false, PrimCadar},
		{"caddr", 1, false, PrimCaddr},
		{"cdaar", 1, false, PrimCdaar},
		{"cdadr", 1, false, PrimCdadr},
		{"cddar", 1, false, PrimCddar},
		{"cdddr", 1, false, PrimCdddr},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// CxR accessors (4-level)
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"caaaar", 1, false, PrimCaaaar},
		{"caaadr", 1, false, PrimCaaadr},
		{"caadar", 1, false, PrimCaadar},
		{"caaddr", 1, false, PrimCaaddr},
		{"cadaar", 1, false, PrimCadaar},
		{"cadadr", 1, false, PrimCadadr},
		{"caddar", 1, false, PrimCaddar},
		{"cadddr", 1, false, PrimCadddr},
		{"cdaaar", 1, false, PrimCdaaar},
		{"cdaadr", 1, false, PrimCdaadr},
		{"cdadar", 1, false, PrimCdadar},
		{"cdaddr", 1, false, PrimCdaddr},
		{"cddaar", 1, false, PrimCddaar},
		{"cddadr", 1, false, PrimCddadr},
		{"cdddar", 1, false, PrimCdddar},
		{"cddddr", 1, false, PrimCddddr},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
