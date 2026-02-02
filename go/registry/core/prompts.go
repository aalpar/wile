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

import "github.com/aalpar/wile/go/registry"

//nolint:govet
func addPrompts(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"make-continuation-prompt-tag", 1, true, PrimMakeContinuationPromptTag},
		{"default-continuation-prompt-tag", 0, false, PrimDefaultContinuationPromptTag},
		{"continuation-prompt-tag?", 1, false, PrimContinuationPromptTagQ},
		{"call-with-continuation-prompt", 3, false, PrimCallWithContinuationPrompt},
		{"abort-current-continuation", 2, true, PrimAbortCurrentContinuation},
		{"call-with-composable-continuation", 2, false, PrimCallWithComposableContinuation},
	}, registry.PhaseRuntime)

	return nil
}
