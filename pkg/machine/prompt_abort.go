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
// RunResumable or RunWithinBoundary, whichever driver's chain holds the prompt frame.
//
// When caught, the handler finds the matching prompt frame, unwinds dynamic-wind
// extents, and invokes the prompt's handler with the abort values.
type ErrPromptAbort struct {
	Tag    *PromptTag
	Values []values.Value
	// SourceWinding is a .Copy() of the winding stack live at the abort ORIGIN — the
	// escape point, which may be a deeper sub-context than the driver that catches the
	// abort. When set, the driver reconciles dynamic-wind from it (not from its own
	// p.windingStack) so the after-thunks between the escape point and the target
	// prompt fire exactly once: removing exitFn's own UnwindTo fixes the exit-path
	// double-fire (C2), and reconciling from the escape-point winding fixes the
	// after-thunk silently skipped when escaping through a deeper sub (C3). nil means
	// "reconcile from the driver's own winding" (the value-delivery aborts, e.g.
	// composable continuation, where the abort site and driver share a winding).
	SourceWinding WindingStack
}

func (p *ErrPromptAbort) Error() string {
	return fmt.Sprintf("abort to prompt %s", p.Tag.SchemeString())
}

// ErrResumeContinuation is the control signal that a call/cc-captured continuation
// is being invoked. It is NOT a value-delivery abort (that is ErrPromptAbort): it
// asks the driver that owns where it was delimited (resumePrompt) to REINSTALL the carried segment
// into the live continuation and keep looping — the trampoline that replaces the
// per-resume nested sub.Run(). It implements error so it rides the existing
// return-err plumbing and errors.As matching across nested sub.Run() frames
// unchanged.
type ErrResumeContinuation struct {
	Tag     *PromptTag
	Segment *ComposableContinuation // carried UNRUN
	Values  []values.Value          // resume args, already copied off the eval stack
	// SourceWinding is the winding the reconcile starts from: a .Copy() of the
	// winding live at the (k v) site, until an expansion-time root passes the
	// resume up, when it is nil and rebaseWinding is set (passUpResume), and the
	// next driver replaces it with its own (rebaseResumeWinding).
	SourceWinding WindingStack
	// escalatorRevivals is the non-continuable escalator arms this resume re-enters
	// from OUTSIDE their finalizer frame's extent, decided at the (k v) site — the
	// only place the answer exists. The signal trampolines to the owning driver, whose
	// own chain never held the frames of the sub-context a raise may have been
	// handled in (a dynamic-wind thunk, a parameter converter), so asking the
	// driver's chain there reads "absent" for an arm that is in fact still live.
	// State of the invocation site, not of the driver; unlike SourceWinding it is
	// not rebased when an expansion-time root passes the resume up (TODO.md).
	//
	// Unexported unlike its neighbours: escalatorArm is package-private, so an
	// exported field would be surface an embedder can neither set nor read through.
	escalatorRevivals []*escalatorArm
	// rebaseWinding is set by an expansion-time root that passed the resume up
	// after leaving its own extents: SourceWinding was that root's winding, not
	// the next driver's, so that driver replaces it with its own before resolving
	// or re-raising (rebaseResumeWinding, in both RunWithinBoundary and
	// RunResumable).
	rebaseWinding bool
}

func (p *ErrResumeContinuation) Error() string {
	return fmt.Sprintf("continuation resume escaped without a matching prompt %s", p.Tag.SchemeString())
}
