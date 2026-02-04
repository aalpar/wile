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

func addLists(r *registry.Registry) error {
	// List construction
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"list", 1, true, PrimList},
		{"make-list", 2, true, PrimMakeList},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// List operations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"append", 1, true, PrimAppend},
		{"reverse", 1, false, PrimReverse},
		{"length", 1, false, PrimLength},
		{"list-ref", 2, false, PrimListRef},
		{"list-set!", 3, false, PrimListSet},
		{"list-tail", 2, false, PrimListTail},
		{"list-copy", 1, false, PrimListCopy},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	// List search
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"memq", 2, false, PrimMemq},
		{"memv", 2, false, PrimMemv},
		{"member", 3, true, PrimMember},
		{"assq", 2, false, PrimAssq},
		{"assv", 2, false, PrimAssv},
		{"assoc", 3, true, PrimAssoc},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
