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

package machine

import (
	"fmt"

	"wile/values"
)

// ErrPromptAbort signals an abort to the nearest continuation prompt matching
// the given tag. This parallels ErrContinuationEscape: it propagates up through
// Run() and is caught by RunWithEscapeHandling().
//
// When caught, the handler finds the matching prompt frame, unwinds dynamic-wind
// extents, and invokes the prompt's handler with the abort values.
type ErrPromptAbort struct {
	Tag    *PromptTag
	Values []values.Value
}

func (e *ErrPromptAbort) Error() string {
	return fmt.Sprintf("abort to prompt %s", e.Tag.SchemeString())
}
