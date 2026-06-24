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
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
)

// Extension is the system extension.
var Extension = registry.NewDescribedExtension("system",
	"System primitives: command-line, exit/emergency-exit, and time (current-second, current-jiffy, jiffies-per-second).",
	AddToRegistry)

// Builder aggregates all system registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all system primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "command-line", Impl: PrimCommandLine,
			Doc: "Returns the command-line arguments as a list of strings, including the program name.", Category: "system",
			Keywords:   []string{"argv", "args", "command-line arguments"},
			ReturnType: values.TypeList},
		// exit and emergency-exit never return — ReturnType is TypeAny as a
		// nominal annotation.
		{Name: "exit", ParamCount: 1, IsVariadic: true, Impl: PrimExit,
			Doc: "Exits the process. With no argument or #t, exits with code 0. With #f, exits with code 1. With an integer, uses that code.", ParamNames: []string{"status"}, Category: "system",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "emergency-exit", ParamCount: 1, IsVariadic: true, Impl: PrimEmergencyExit,
			Doc: "Exits the process immediately without running cleanup. Same argument handling as exit.", ParamNames: []string{"status"}, Category: "system",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "current-second", Impl: PrimCurrentSecond,
			Doc: "Returns the current time as seconds since the Unix epoch as an inexact real number.", Category: "system",
			Keywords:   []string{"time now", "Unix timestamp", "epoch", "wall clock"},
			ReturnType: values.TypeFlonum},
		{Name: "current-jiffy", Impl: PrimCurrentJiffy,
			Doc: "Returns the number of nanoseconds since program start, using the monotonic clock.", Category: "system",
			Keywords:   []string{"monotonic time", "nanoseconds", "timer", "benchmark"},
			ReturnType: values.TypeInteger},
		{Name: "jiffies-per-second", Impl: PrimJiffiesPerSecond,
			Doc: "Returns 1000000000, the number of jiffies (nanoseconds) per second.", Category: "system",
			ReturnType: values.TypeInteger},
	}, registry.PhaseSetRuntime)
	return nil
}
