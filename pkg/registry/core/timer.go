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
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
)

func addTimer(r *registry.Registry) error { //nolint:govet
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "with-timeout", InvokesProcedure: true, ParamCount: 3, Impl: PrimWithTimeout,
			Doc: "Runs THUNK with a wall-clock timeout of MS milliseconds. " +
				"If the thunk completes before the deadline, returns its result. " +
				"If the timer expires, suspends the thunk and calls HANDLER with a " +
				"composable continuation that, when invoked, resumes the suspended computation.\n\n" +
				"Parameters:\n  ms: integer — exact non-negative (milliseconds)\n" +
				"  handler: procedure — (lambda (resumable-continuation) ...)\n" +
				"  thunk: procedure — (lambda () ...)\n\n" +
				"Examples:\n" +
				"  (with-timeout 5000 (lambda (k) 'timeout) (lambda () 42))  => 42\n" +
				"  (with-timeout 1 (lambda (k) 'expired) (lambda () (let loop () (loop))))  => expired",
			ParamNames: []string{"ms", "handler", "thunk"},
			ParamTypes: []values.TypeConstraint{values.TypeInteger, values.TypeProcedure, values.TypeProcedure},
			ReturnType: values.TypeAny,
			Category:   "control", Keywords: []string{"timeout", "timer", "engine", "fuel", "bounded-eval", "preemption"}},
	}, registry.PhaseSetRuntime)

	return nil
}
