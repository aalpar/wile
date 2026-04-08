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
	"github.com/aalpar/wile/values"
)

func addControl(r *registry.Registry) error {
	// Higher-order functions
	// Note: map and for-each are implemented in Scheme (see bootstrap.go)
	// so their iteration becomes capturable Scheme frames for call/cc.
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "apply", ParamCount: 2, IsVariadic: true, Impl: PrimApply,
			Doc: "Calls PROC with the elements of the final list argument as individual arguments. Intermediate arguments are prepended.\n\nExamples:\n  (apply + '(1 2 3))      => 6\n  (apply + 1 '(2 3))      => 6\n  (apply list 'a 'b '(c))  => (a b c)", ParamNames: []string{"proc", "arg1", "args"}, Category: "control"},
	}, registry.PhaseRuntime)

	// Continuations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "call-with-current-continuation", ParamCount: 1, Impl: PrimCallCC,
			Doc: "Captures the current continuation as an escape procedure and passes it to PROC. The escape procedure can be called to return to this point.\n\nExamples:\n  (call-with-current-continuation (lambda (k) 42))  => 42\n  (call-with-current-continuation (lambda (k) (k 7) 99))  => 7", ParamNames: []string{"proc"}, Category: "control",
			ParamTypes: []values.ValueType{values.TypeProcedure}},
		{Name: "call/cc", ParamCount: 1, Impl: PrimCallCC,
			Doc: "Abbreviation for call-with-current-continuation. Captures the current continuation and passes it to PROC.\n\nExamples:\n  (call/cc (lambda (k) (k 42)))  => 42", ParamNames: []string{"proc"}, Category: "control",
			ParamTypes: []values.ValueType{values.TypeProcedure}},
		// dynamic-wind is now a compiled form, not a primitive (see machine/compile_validated.go)
		{Name: "call-with-exit", ParamCount: 1, Impl: PrimCallWithExit,
			Doc: "Calls PROC with a lightweight one-shot escape procedure. The escape is only valid during the dynamic extent of the call.\n\nExamples:\n  (call-with-exit (lambda (exit) (exit 42) 99))  => 42", ParamNames: []string{"proc"}, Category: "control",
			ParamTypes: []values.ValueType{values.TypeProcedure}},
		{Name: "call-with-continuation-barrier", ParamCount: 1, Impl: PrimCallWithContinuationBarrier,
			Doc: "Calls THUNK with a continuation barrier. Continuations cannot cross the barrier boundary in either direction.\n\nExamples:\n  (call-with-continuation-barrier (lambda () 42))  => 42", ParamNames: []string{"thunk"}, Category: "control",
			ParamTypes: []values.ValueType{values.TypeProcedure}},
	}, registry.PhaseRuntime)

	// Multiple values
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "values", ParamCount: 1, IsVariadic: true, Impl: PrimValues,
			Doc: "Returns zero or more values. With one argument, equivalent to returning that value directly.\n\nExamples:\n  (call-with-values (lambda () (values 1 2)) +)  => 3", ParamNames: []string{"obj", "objs"}, Category: "control",
			ParamTypes: []values.ValueType{values.TypeAny}},
		{Name: "call-with-values", ParamCount: 2, Impl: PrimCallWithValues,
			Doc: "Calls PRODUCER with no arguments, then passes its return values as arguments to CONSUMER.\n\nExamples:\n  (call-with-values (lambda () (values 1 2)) +)  => 3\n  (call-with-values (lambda () (values 'a 'b)) cons)  => (a . b)", ParamNames: []string{"producer", "consumer"}, Category: "control",
			ParamTypes: []values.ValueType{values.TypeProcedure, values.TypeProcedure}},
	}, registry.PhaseRuntime)

	return nil
}
