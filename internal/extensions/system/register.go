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
package system

import (
	"github.com/aalpar/wile/registry"
)

// Extension is the system extension.
var Extension = registry.NewExtension("system", AddToRegistry)

// Builder aggregates all system registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all system primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "command-line", Impl: PrimCommandLine,
			Doc: "Returns the command-line arguments as a list.", Category: "system"},
		{Name: "exit", ParamCount: 1, IsVariadic: true, Impl: PrimExit,
			Doc: "Exits the program with the given status.", ParamNames: []string{"status"}, Category: "system"},
		{Name: "emergency-exit", ParamCount: 1, IsVariadic: true, Impl: PrimEmergencyExit,
			Doc: "Exits the program immediately.", ParamNames: []string{"status"}, Category: "system"},
		{Name: "get-environment-variable", ParamCount: 1, Impl: PrimGetEnvironmentVariable,
			Doc: "Returns the value of an environment variable, or #f.", ParamNames: []string{"name"}, Category: "system"},
		{Name: "get-environment-variables", Impl: PrimGetEnvironmentVariables,
			Doc: "Returns all environment variables as an alist.", Category: "system"},
		{Name: "current-second", Impl: PrimCurrentSecond,
			Doc: "Returns the current time as seconds since epoch.", Category: "system"},
		{Name: "current-jiffy", Impl: PrimCurrentJiffy,
			Doc: "Returns monotonic nanoseconds since program start.", Category: "system"},
		{Name: "jiffies-per-second", Impl: PrimJiffiesPerSecond,
			Doc: "Returns the number of jiffies per second.", Category: "system"},
		{Name: "features", Impl: PrimFeatures,
			Doc: "Returns a list of implementation feature symbols.", Category: "system"},
	}, registry.PhaseRuntime)
	return nil
}
