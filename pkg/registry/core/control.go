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

func addControl(r *registry.PrimitiveRegistry) error {
	// Higher-order functions
	// Note: map and for-each are implemented in Scheme (see bootstrap.go)
	// so their iteration becomes capturable Scheme frames for call/cc.
	//
	// Control-flow primitives (apply, call/cc, call-with-exit, etc.) return
	// whatever the invoked procedure returns — TypeAny is the honest answer.
	// values/call-with-values may produce zero or multiple values; ReturnType
	// still describes the first value (or the consumer's result) as TypeAny.
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "apply", InvokesProcedure: true, ParamCount: 2, IsVariadic: true, Impl: PrimApply,
			Doc: "Calls PROC with the elements of the final list argument as individual arguments. Intermediate arguments are prepended.\n\nExamples:\n  (apply + '(1 2 3))      => 6\n  (apply + 1 '(2 3))      => 6\n  (apply list 'a 'b '(c))  => (a b c)", ParamNames: []string{"proc", "arg1", "args"}, Category: "control",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure, values.TypeAny}, ReturnType: values.TypeAny,
			Keywords: []string{"invoke", "call with list", "spread", "splat"}},
	}, registry.PhaseSetRuntime)

	// Continuations
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "call-with-current-continuation", InvokesProcedure: true, ParamCount: 1, Impl: PrimCallCC,
			Doc: "Captures the current continuation as an escape procedure and passes it to PROC. The escape procedure can be called to return to this point.\n\nExamples:\n  (call-with-current-continuation (lambda (k) 42))  => 42\n  (call-with-current-continuation (lambda (k) (k 7) 99))  => 7", ParamNames: []string{"proc"}, Category: "control",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure}, ReturnType: values.TypeAny,
			Keywords: []string{"continuation", "escape", "non-local exit", "setjmp", "longjmp"}},
		{Name: "call/cc", InvokesProcedure: true, ParamCount: 1, Impl: PrimCallCC,
			Doc: "Abbreviation for call-with-current-continuation. Captures the current continuation and passes it to PROC.\n\nExamples:\n  (call/cc (lambda (k) (k 42)))  => 42", ParamNames: []string{"proc"}, Category: "control",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure}, ReturnType: values.TypeAny,
			Keywords: []string{"continuation", "escape", "non-local exit"}},
		// dynamic-wind is now a compiled form, not a primitive (see machine/compile_validated.go)
		{Name: "call-with-exit", InvokesProcedure: true, ParamCount: 1, Impl: PrimCallWithExit,
			Doc: "Calls PROC with a lightweight one-shot escape procedure. The escape is only valid during the dynamic extent of the call.\n\nExamples:\n  (call-with-exit (lambda (exit) (exit 42) 99))  => 42", ParamNames: []string{"proc"}, Category: "control",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure}, ReturnType: values.TypeAny},
		{Name: "call-with-continuation-barrier", InvokesProcedure: true, ParamCount: 1, Impl: PrimCallWithContinuationBarrier,
			Doc: "Calls THUNK with a continuation barrier. Continuations cannot cross the barrier boundary in either direction.\n\nExamples:\n  (call-with-continuation-barrier (lambda () 42))  => 42", ParamNames: []string{"thunk"}, Category: "control",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure}, ReturnType: values.TypeAny},
	}, registry.PhaseSetRuntime)

	// Multiple values
	r.AddPrimitives([]registry.PrimitiveSpec{
		// values/call-with-values can produce zero or many outputs — a single
		// ReturnType cannot express that, so TypeAny is the closest honest
		// annotation.
		{Name: "values", ParamCount: 1, IsVariadic: true, Impl: PrimValues,
			Doc: "Returns zero or more values. With one argument, equivalent to returning that value directly.\n\nExamples:\n  (call-with-values (lambda () (values 1 2)) +)  => 3", ParamNames: []string{"obj", "objs"}, Category: "control",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny,
			Keywords: []string{"multiple return values", "multi-value", "tuple return"}},
		{Name: "call-with-values", InvokesProcedure: true, ParamCount: 2, Impl: PrimCallWithValues,
			Doc: "Calls PRODUCER with no arguments, then passes its return values as arguments to CONSUMER.\n\nExamples:\n  (call-with-values (lambda () (values 1 2)) +)  => 3\n  (call-with-values (lambda () (values 'a 'b)) cons)  => (a . b)", ParamNames: []string{"producer", "consumer"}, Category: "control",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure, values.TypeProcedure}, ReturnType: values.TypeAny,
			Keywords: []string{"multiple value dispatch", "receive values"}},
	}, registry.PhaseSetRuntime)

	return nil
}
