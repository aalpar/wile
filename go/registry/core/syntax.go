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

func addSyntax(r *registry.Registry) error {
	// Syntax objects (R6RS syntax-case support)
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"identifier?", 1, false, PrimIdentifierQ},
		{"syntax->datum", 1, false, PrimSyntaxToDatum},
		{"datum->syntax", 2, false, PrimDatumToSyntax},
		{"generate-temporaries", 1, false, PrimGenerateTemporaries},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// Identifier comparison
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"bound-identifier=?", 2, false, PrimBoundIdentifierEqualQ},
		{"free-identifier=?", 2, false, PrimFreeIdentifierEqualQ},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
