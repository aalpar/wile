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

	// CxR accessors (2/3/4-level) — generated from cxrSpecs table
	cxrPrims := make([]registry.PrimitiveSpec, len(cxrSpecs))
	for i, spec := range cxrSpecs {
		cxrPrims[i] = registry.PrimitiveSpec{spec.name, 1, false, makeCxrPrimitive(spec.name, spec.ops)}
	}
	r.AddPrimitives(cxrPrims, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
