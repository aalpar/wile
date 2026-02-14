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

import "github.com/aalpar/wile/registry"

func addPrompts(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-continuation-prompt-tag", ParamCount: 1, IsVariadic: true, Impl: PrimMakeContinuationPromptTag,
			Doc: "Creates a new continuation prompt tag.", ParamNames: []string{"name"}, Category: "continuations"},
		{Name: "default-continuation-prompt-tag", Impl: PrimDefaultContinuationPromptTag,
			Doc: "Returns the default continuation prompt tag.", Category: "continuations"},
		{Name: "continuation-prompt-tag?", ParamCount: 1, Impl: PrimContinuationPromptTagQ,
			Doc: "Returns #t if obj is a continuation prompt tag.", ParamNames: []string{"obj"}, Category: "continuations"},
		{Name: "call-with-continuation-prompt", ParamCount: 3, Impl: PrimCallWithContinuationPrompt,
			Doc: "Calls thunk with a continuation prompt.", ParamNames: []string{"thunk", "tag", "handler"}, Category: "continuations"},
		{Name: "abort-current-continuation", ParamCount: 2, IsVariadic: true, Impl: PrimAbortCurrentContinuation,
			Doc: "Aborts to the nearest prompt with the given tag.", ParamNames: []string{"tag", "val", "vals"}, Category: "continuations"},
		{Name: "call-with-composable-continuation", ParamCount: 2, Impl: PrimCallWithComposableContinuation,
			Doc: "Captures a composable continuation up to the nearest prompt.", ParamNames: []string{"proc", "tag"}, Category: "continuations"},
	}, registry.PhaseRuntime)

	return nil
}
