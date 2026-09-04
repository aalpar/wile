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

// ErrBreakInterrupt signals that the debugger wants execution suspended, either
// because a breakpoint matched or because a step completed. It propagates through
// the Go error return path and is resolved by the nearest driver (RunResumable at
// the top level, RunWithinBoundary in a surviving sub-context), which locates the
// break prompt frame via FindPrompt(Tag).
//
// This is a signal, not an exception. It is NOT caught by Scheme exception
// handlers — only by the VM infrastructure that installed the break prompt.
type ErrBreakInterrupt struct {
	Handler values.Callable
	// Tag identifies the break boundary (the prompt frame InstallBreakPrompt
	// pushed) on the continuation chain. A driver does FindPrompt(Tag) to locate
	// that frame and resolves the break there.
	Tag *PromptTag
	// BP is the breakpoint that matched, or nil when the stop is a step
	// completion rather than a breakpoint.
	BP *Breakpoint
}

func (p *ErrBreakInterrupt) Error() string {
	return "break interrupt"
}

// breakState clusters the MachineContext fields describing an armed break
// boundary: the suspension handler and the prompt tag of its frame, which are
// written together and read together at the emit site.
//
// It deliberately does NOT reuse timerState. That record's
// (handler == nil) ⇔ (cancel == nil) invariant names RunBodyUnderTimer as its
// sole production writer, and a break has neither a cancel function nor a saved
// context to restore: nothing about it is a timeout. It also carries no parent
// pointer, because a boundary is installed once per run and never torn down —
// there is nothing to nest and nothing to pop.
type breakState struct {
	handler values.Callable
	tag     *PromptTag
}

// breakTraceDepth is how many frames a break snapshot CAPTURES. It bounds what
// a later render can show, so it sits comfortably above the 20 the REPL's
// ,backtrace asks for; the render itself honours whatever budget it is given.
const breakTraceDepth = 32

var _ values.DebugState = (*BreakSnapshot)(nil)

// BreakSnapshot is the frozen view of the VM at a break: the location, the
// rendered stack trace, and the call depth, all read from the LIVE continuation
// chain before the suspension restores past it.
//
// A snapshot rather than the *MachineContext itself, for two reasons. The
// context is pool-recycled and zeroed by ReleaseTopLevelContext the moment the
// evaluation ends, so a stored pointer reads as a blank context afterwards; and
// during the suspension p.cont has already been moved to the break boundary, so
// the live context no longer describes the break point either.
type BreakSnapshot struct {
	loc *values.DebugLocation
	// frames is the trace as captured, at most breakTraceDepth real frames plus
	// CaptureStackTrace's own trailing ellipsis when the chain ran deeper.
	frames StackTrace
	// capped records that the ellipsis is present, so a truncating render can
	// say "or more" rather than assert an exact remainder it does not know.
	capped bool
	depth  int
}

// newBreakSnapshot freezes mc's current location, stack trace and call depth.
// It must be called while mc still holds the break point's chain.
func newBreakSnapshot(mc *MachineContext) *BreakSnapshot {
	frames := mc.CaptureStackTrace(breakTraceDepth)
	return &BreakSnapshot{
		loc: mc.CurrentLocation(),
		// CaptureStackTrace appends at most one ellipsis frame and only after
		// filling its budget, so exceeding the budget is exactly "it truncated".
		frames: frames,
		capped: len(frames) > breakTraceDepth,
		depth:  mc.CallDepth(),
	}
}

// CurrentLocation returns the source location of the break point.
// Implements values.DebugState.
func (p *BreakSnapshot) CurrentLocation() *values.DebugLocation {
	return p.loc
}

// FormatStackTrace renders at most maxDepth frames of the trace frozen at the
// break, per values.DebugState. maxDepth <= 0 means "no bound", matching the
// interface's other implementation, whose walk a non-positive budget does not
// enter.
//
// The frames are the ones captured at the break, not a fresh walk: the live
// chain has moved to the break boundary (or been recycled) by then, so a request
// deeper than breakTraceDepth can only be answered with what was captured.
// Implements values.DebugState.
func (p *BreakSnapshot) FormatStackTrace(maxDepth int) string {
	if maxDepth <= 0 || maxDepth >= len(p.frames) {
		return p.frames.String()
	}
	hidden := len(p.frames) - maxDepth
	more := "more frames"
	if p.capped {
		// The deepest captured entry is capture's own ellipsis, which is not a
		// frame — and the chain it stood for is longer than anything counted here.
		hidden--
		more = "or more frames"
	}
	q := make(StackTrace, maxDepth, maxDepth+1)
	copy(q, p.frames)
	q = append(q, StackFrame{FunctionName: fmt.Sprintf("... %d %s ...", hidden, more)})
	return q.String()
}

// CallDepth returns the continuation depth at the break point. It is the key
// step-over and step-out are re-armed on: by the time a handler runs, the live
// context sits at the break boundary and its own depth is no longer the break
// point's.
func (p *BreakSnapshot) CallDepth() int {
	return p.depth
}

// InstallBreakPrompt arms a break boundary on this context and returns its tag.
// It pushes a transparent prompt frame (returnTemplate) carrying that tag, so a
// break emitted anywhere below it routes here via a driver's FindPrompt, and
// records the boundary in p.brk so the emit site knows a suspension is possible.
//
// handler may be nil at install time: the closure that suspends generally needs
// the tag this call mints, so callers install first and supply the handler with
// SetBreakHandler.
//
// Unlike RunBodyUnderFrame this applies no body — runCompiled has already loaded
// the template to run — so it copies only the frame-push half: thread identity,
// a winding copy, a marks clone, and the current barrier.
//
// A nil parent (p.cont == nil, the top-level case) is correct. The frame is
// transparent, so the body's value(s) flow through it untouched; on normal
// completion the VM runs off the end of the template and never restores the
// frame at all, and when it is restored — by a top-level tail call taking
// applyForeign's rooted arm instead of its rootless one — returnTemplate's
// OpRestoreContinuation returns on the nil parent with the value register intact.
func (p *MachineContext) InstallBreakPrompt(handler values.Callable) *PromptTag {
	tag := NewPromptTag("debugger-break")
	p.brk = &breakState{handler: handler, tag: tag}

	frame := NewMachineContinuationWithPrompt(p.cont, returnTemplate, p.env, tag, nil)
	frame.threadID = p.threadID
	if len(p.windingStack) > 0 {
		frame.windingStack = p.windingStack.Copy()
	}
	frame.marks = cloneMarks(p.marks)
	frame.barrierValid = p.barrierValid
	p.cont = frame
	// Pushing a frame lengthens the chain, and p.callDepth caches that length —
	// SaveContinuation increments it for exactly this reason. Without the
	// increment, the synthetic frame CaptureInterruptContinuationAt builds (depth
	// = p.cont.callDepth + 1) is one deeper than p.callDepth, so every resume
	// inflates the depth by one and the depth-keyed step modes drift.
	p.callDepth++
	return tag
}

// SetBreakHandler installs the callable a break suspension applies, on the
// innermost armed boundary. It is a no-op when no boundary is armed.
func (p *MachineContext) SetBreakHandler(handler values.Callable) {
	if p.brk == nil {
		return
	}
	p.brk.handler = handler
}

// BreakResumeValues returns the arguments a suspension handler must apply the
// resumable continuation to in order to resume unchanged: the value register
// frozen at the break point. Resuming with no arguments instead clears the
// register, because ReinstallSegment ends with SetValues(args...) and Restore
// does not carry a register of its own.
func (p *MachineContext) BreakResumeValues() []values.Value {
	return p.breakResumeValues
}
