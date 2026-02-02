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
	"github.com/aalpar/wile/go/registry"
)

func addControl(r *registry.Registry) error {
	// Higher-order functions
	// Note: map and for-each are implemented in Scheme (see bootstrap.go)
	// so their iteration becomes capturable Scheme frames for call/cc.
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"apply", 2, true, PrimApply},
	}, registry.PhaseRuntime)

	// Continuations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"call-with-current-continuation", 1, false, PrimCallCC},
		{"call/cc", 1, false, PrimCallCC},
		// dynamic-wind is now a compiled form, not a primitive (see machine/compile_validated.go)
	}, registry.PhaseRuntime)

	// Multiple values
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"values", 1, true, PrimValues},
		{"call-with-values", 2, false, PrimCallWithValues},
	}, registry.PhaseRuntime)

	return nil
}
