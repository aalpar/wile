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

package sat

import (
	"github.com/aalpar/wile/registry"
)

// Extension is the SAT solver FFI extension.
var Extension = registry.NewDescribedExtension("sat",
	"CDCL SAT solver kernel backing (wile algebra sat). Accepts CNF as a flat vector of int literals with 0-terminated clauses; returns SAT/UNSAT plus a model on SAT, or 'unknown on conflict-budget exhaustion or ctx cancellation.",
	AddToRegistry)

// Builder aggregates all sat registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all sat primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	// Primitives added in later phases. This stub exists now so the
	// extension compiles and can be wired into AllExtensions.
	return nil
}
