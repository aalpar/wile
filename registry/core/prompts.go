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

func addPrompts(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		// Prompt tags are opaque values without a dedicated ValueType enum —
		// annotate as TypeAny.
		{Name: "make-continuation-prompt-tag", ParamCount: 1, IsVariadic: true, Impl: PrimMakeContinuationPromptTag,
			Doc: "Creates a new prompt tag with an optional symbolic name for debugging.\n\nExamples:\n  (continuation-prompt-tag? (make-continuation-prompt-tag 'my-tag))  => #t", ParamNames: []string{"name"}, Category: "continuations",
			ParamTypes: []values.TypeConstraint{values.TypeSymbol}, ReturnType: values.TypeAny},
		{Name: "default-continuation-prompt-tag", Impl: PrimDefaultContinuationPromptTag,
			Doc: "Returns the default prompt tag used by call/cc and the top-level evaluator.\n\nExamples:\n  (continuation-prompt-tag? (default-continuation-prompt-tag))  => #t", Category: "continuations",
			ReturnType: values.TypeAny},
		{Name: "continuation-prompt-tag?", ParamCount: 1, Impl: PrimContinuationPromptTagQ,
			Doc: "Returns #t if OBJ is a continuation prompt tag.\n\nExamples:\n  (continuation-prompt-tag? (make-continuation-prompt-tag))  => #t\n  (continuation-prompt-tag? 42)  => #f", ParamNames: []string{"obj"}, Category: "continuations",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		// Prompt/abort/composable-continuation invocations return whatever
		// the body/handler produces.
		{Name: "call-with-continuation-prompt", ParamCount: 3, Impl: PrimCallWithContinuationPrompt,
			Doc: "Calls THUNK with a delimited continuation prompt tagged with TAG. If THUNK aborts to TAG, HANDLER is called with the abort values.\n\nExamples:\n  (let ((tag (make-continuation-prompt-tag)))\n    (call-with-continuation-prompt\n      (lambda () (abort-current-continuation tag 42))\n      tag\n      (lambda (v) v)))  => 42", ParamNames: []string{"thunk", "tag", "handler"}, Category: "continuations",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure, values.TypeAny, values.TypeProcedure}, ReturnType: values.TypeAny},
		{Name: "abort-current-continuation", ParamCount: 2, IsVariadic: true, Impl: PrimAbortCurrentContinuation,
			Doc: "Aborts to the nearest prompt matching TAG, passing VAL and VALS to the prompt handler.\n\nExamples:\n  ;; See call-with-continuation-prompt for usage.", ParamNames: []string{"tag", "val", "vals"}, Category: "continuations",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "call-with-composable-continuation", ParamCount: 2, Impl: PrimCallWithComposableContinuation,
			Doc: "Captures the continuation up to the nearest prompt matching TAG as a composable procedure, and passes it to PROC.\n\nExamples:\n  ;; See (wile control) for shift/reset built on this primitive.", ParamNames: []string{"proc", "tag"}, Category: "continuations",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure, values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "continuation-prompt-available?", ParamCount: 1, Impl: PrimContinuationPromptAvailableQ,
			Doc: "Returns #t if a prompt matching TAG exists on the current continuation chain.\n\nExamples:\n  (continuation-prompt-available? (default-continuation-prompt-tag))  => #t", ParamNames: []string{"tag"}, Category: "continuations",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
	}, registry.PhaseSetRuntime)

	return nil
}
