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

// Package system provides system-level primitives.
//
//nolint:govet // Using unkeyed struct fields for concise primitive specs
package system

import (
	"wile/registry"
	"wile/runtime/primitives"
)

// Extension is the system extension.
var Extension = registry.NewExtension("system", AddToRegistry)

// Builder aggregates all system registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all system primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"command-line", 0, false, primitives.PrimCommandLine},
		{"exit", 1, true, primitives.PrimExit},
		{"emergency-exit", 1, true, primitives.PrimEmergencyExit},
		{"get-environment-variable", 1, false, primitives.PrimGetEnvironmentVariable},
		{"get-environment-variables", 0, false, primitives.PrimGetEnvironmentVariables},
		{"current-second", 0, false, primitives.PrimCurrentSecond},
		{"current-jiffy", 0, false, primitives.PrimCurrentJiffy},
		{"jiffies-per-second", 0, false, primitives.PrimJiffiesPerSecond},
		{"features", 0, false, primitives.PrimFeatures},
	}, registry.PhaseRuntime)
	return nil
}
