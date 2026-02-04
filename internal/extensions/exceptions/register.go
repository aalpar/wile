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

// Package exceptions provides exception handling primitives.
//
//nolint:govet // Using unkeyed struct fields for concise primitive specs
package exceptions

import (
	"github.com/aalpar/wile/registry"
)

// Extension is the exceptions extension.
var Extension = registry.NewExtension("exceptions", AddToRegistry)

// Builder aggregates all exception registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all exception primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"with-exception-handler", 2, false, PrimWithExceptionHandler},
		{"raise", 1, false, PrimRaise},
		{"raise-continuable", 1, false, PrimRaiseContinuable},
		{"error", 2, true, PrimError},
		{"error-object?", 1, false, PrimErrorObjectQ},
		{"error-object-message", 1, false, PrimErrorObjectMessage},
		{"error-object-irritants", 1, false, PrimErrorObjectIrritants},
		{"read-error?", 1, false, PrimReadErrorQ},
		{"file-error?", 1, false, PrimFileErrorQ},
	}, registry.PhaseRuntime)
	return nil
}
