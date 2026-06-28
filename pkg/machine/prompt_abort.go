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

	"github.com/aalpar/wile/pkg/values"
)

// ErrPromptAbort signals an abort to the nearest continuation prompt matching
// the given tag. It propagates up through Run() and is caught by
// RunWithEscapeHandling() or by call-with-continuation-prompt sub-contexts.
//
// When caught, the handler finds the matching prompt frame, unwinds dynamic-wind
// extents, and invokes the prompt's handler with the abort values.
type ErrPromptAbort struct {
	Tag    *PromptTag
	Values []values.Value
}

func (p *ErrPromptAbort) Error() string {
	return fmt.Sprintf("abort to prompt %s", p.Tag.SchemeString())
}

// ErrResumeContinuation is the control signal that a call/cc-captured continuation
// is being invoked. It is NOT a value-delivery abort (that is ErrPromptAbort): it
// asks the nearest DefaultPromptTag driver loop to REINSTALL the carried segment
// into the live continuation and keep looping — the trampoline that replaces the
// per-resume nested sub.Run(). It implements error so it rides the existing
// return-err plumbing and errors.As matching across nested sub.Run() frames
// unchanged.
type ErrResumeContinuation struct {
	Tag           *PromptTag
	Segment       *ComposableContinuation // carried UNRUN
	Values        []values.Value          // resume args, already copied off the eval stack
	SourceWinding WindingStack            // .Copy() of the winding live at the (k v) site
	Isolate       bool                    // captured call/cc => true; composable resume => false
}

func (p *ErrResumeContinuation) Error() string {
	return "continuation resume escaped without a matching prompt"
}
