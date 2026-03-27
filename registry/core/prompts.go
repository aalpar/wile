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
		{Name: "make-continuation-prompt-tag", ParamCount: 1, IsVariadic: true, Impl: PrimMakeContinuationPromptTag,
			Doc: "Creates a new prompt tag with an optional symbolic name for debugging.", ParamNames: []string{"name"}, Category: "continuations",
			ParamTypes: []values.ValueType{values.TypeSymbol}},
		{Name: "default-continuation-prompt-tag", Impl: PrimDefaultContinuationPromptTag,
			Doc: "Returns the default prompt tag used by call/cc and the top-level evaluator.", Category: "continuations"},
		{Name: "continuation-prompt-tag?", ParamCount: 1, Impl: PrimContinuationPromptTagQ,
			Doc: "Returns #t if obj is a continuation prompt tag.", ParamNames: []string{"obj"}, Category: "continuations",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "call-with-continuation-prompt", ParamCount: 3, Impl: PrimCallWithContinuationPrompt,
			Doc: "Calls thunk with a delimited continuation prompt tagged with tag. If thunk aborts to tag, handler is called with the abort values.", ParamNames: []string{"thunk", "tag", "handler"}, Category: "continuations",
			ParamTypes: []values.ValueType{values.TypeProcedure, values.TypeAny, values.TypeProcedure}},
		{Name: "abort-current-continuation", ParamCount: 2, IsVariadic: true, Impl: PrimAbortCurrentContinuation,
			Doc: "Aborts to the nearest prompt matching tag, passing val and vals to the prompt handler.", ParamNames: []string{"tag", "val", "vals"}, Category: "continuations",
			ParamTypes: []values.ValueType{values.TypeAny, values.TypeAny}},
		{Name: "call-with-composable-continuation", ParamCount: 2, Impl: PrimCallWithComposableContinuation,
			Doc: "Captures the continuation up to the nearest prompt matching tag as a composable procedure, and passes it to proc.", ParamNames: []string{"proc", "tag"}, Category: "continuations",
			ParamTypes: []values.ValueType{values.TypeProcedure, values.TypeAny}},
		{Name: "continuation-prompt-available?", ParamCount: 1, Impl: PrimContinuationPromptAvailableQ,
			Doc: "Returns #t if a prompt matching tag exists on the current continuation chain.", ParamNames: []string{"tag"}, Category: "continuations",
			ParamTypes: []values.ValueType{values.TypeAny}, ReturnType: values.TypeBoolean},
	}, registry.PhaseRuntime)

	return nil
}
