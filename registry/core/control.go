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

package core

import (
	"github.com/aalpar/wile/registry"
)

func addControl(r *registry.Registry) error {
	// Higher-order functions
	// Note: map and for-each are implemented in Scheme (see bootstrap.go)
	// so their iteration becomes capturable Scheme frames for call/cc.
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "apply", ParamCount: 2, IsVariadic: true, Impl: PrimApply,
			Doc: "Applies a procedure to a list of arguments.", ParamNames: []string{"proc", "arg1", "args"}, Category: "control"},
	}, registry.PhaseRuntime)

	// Continuations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "call-with-current-continuation", ParamCount: 1, Impl: PrimCallCC,
			Doc: "Captures the current continuation and passes it to proc.", ParamNames: []string{"proc"}, Category: "control"},
		{Name: "call/cc", ParamCount: 1, Impl: PrimCallCC,
			Doc: "Shorthand for call-with-current-continuation.", ParamNames: []string{"proc"}, Category: "control"},
		// dynamic-wind is now a compiled form, not a primitive (see machine/compile_validated.go)
		{Name: "call-with-exit", ParamCount: 1, Impl: PrimCallWithExit,
			Doc: "Calls proc with a lightweight one-shot escape procedure valid only during the call.", ParamNames: []string{"proc"}, Category: "control"},
		{Name: "call-with-continuation-barrier", ParamCount: 1, Impl: PrimCallWithContinuationBarrier,
			Doc: "Calls thunk, preventing continuations from crossing the barrier boundary.", ParamNames: []string{"thunk"}, Category: "control"},
	}, registry.PhaseRuntime)

	// Multiple values
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "values", ParamCount: 1, IsVariadic: true, Impl: PrimValues,
			Doc: "Returns multiple values.", ParamNames: []string{"obj", "objs"}, Category: "control"},
		{Name: "call-with-values", ParamCount: 2, Impl: PrimCallWithValues,
			Doc: "Calls consumer with the values produced by producer.", ParamNames: []string{"producer", "consumer"}, Category: "control"},
	}, registry.PhaseRuntime)

	return nil
}
