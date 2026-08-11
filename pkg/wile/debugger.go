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

package wile

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"
)

// BreakpointInfo holds read-only breakpoint state for display.
type BreakpointInfo struct {
	ID       int
	File     string
	Line     int
	Column   int
	Enabled  bool
	HitCount int
}

// BreakAction is a suspension handler's verdict: what the VM should do with the
// computation it stopped. It is returned by the callback registered with
// [Debugger.OnBreakSuspend], which runs while the VM is suspended.
type BreakAction int

const (
	// BreakContinue resumes the suspended computation with stepping off.
	BreakContinue BreakAction = iota
	// BreakStep resumes and stops at the next source line, entering calls.
	BreakStep
	// BreakNext resumes and stops at the next source line in the same or a
	// shallower frame, running nested calls to completion.
	BreakNext
	// BreakFinish resumes and stops once execution reaches a strictly
	// shallower frame than the one that was suspended.
	BreakFinish
	// BreakAbandon does not resume: the computation is discarded and the
	// evaluation returns void. dynamic-wind after-thunks between the break
	// point and the top level still run.
	BreakAbandon
)

// Debugger controls breakpoints and stepping for an Engine.
// It wraps the internal machine.Debugger to avoid exposing VM types.
type Debugger struct {
	inner          *machine.Debugger
	onBreak        func(state values.DebugState, bp *BreakpointInfo)
	onBreakSuspend func(state values.DebugState, bp *BreakpointInfo) BreakAction
}

// NewDebugger creates a new Debugger.
func NewDebugger() *Debugger {
	q := &Debugger{
		inner: machine.NewDebugger(),
	}
	q.inner.OnBreak(func(mc *machine.MachineContext, bp *machine.Breakpoint) {
		if q.onBreak != nil {
			q.onBreak(mc, machineBreakpointToInfo(bp))
		}
	})
	return q
}

// SetBreakpoint adds a breakpoint at the given source location.
// Returns the breakpoint ID.
func (p *Debugger) SetBreakpoint(file string, line, col int) int {
	return int(p.inner.SetBreakpoint(file, line, col))
}

// RemoveBreakpoint removes a breakpoint by ID.
func (p *Debugger) RemoveBreakpoint(id int) bool {
	return p.inner.RemoveBreakpoint(machine.BreakpointID(id))
}

// EnableBreakpoint enables a breakpoint by ID.
func (p *Debugger) EnableBreakpoint(id int) bool {
	return p.inner.EnableBreakpoint(machine.BreakpointID(id))
}

// DisableBreakpoint disables a breakpoint by ID.
func (p *Debugger) DisableBreakpoint(id int) bool {
	return p.inner.DisableBreakpoint(machine.BreakpointID(id))
}

// Breakpoints returns all breakpoints.
func (p *Debugger) Breakpoints() []BreakpointInfo {
	bps := p.inner.Breakpoints()
	q := make([]BreakpointInfo, len(bps))
	for i, bp := range bps {
		q[i] = BreakpointInfo{
			ID:       int(bp.ID),
			File:     bp.File,
			Line:     bp.Line,
			Column:   bp.Column,
			Enabled:  bp.Enabled,
			HitCount: bp.HitCount,
		}
	}
	return q
}

// StepInto enables step-into mode.
func (p *Debugger) StepInto() {
	p.inner.StepInto()
}

// StepOver enables step-over mode relative to the most recent break. It is a
// no-op when no break has happened, because there is no depth to step over.
func (p *Debugger) StepOver() {
	depth, ok := p.breakDepth()
	if !ok {
		return
	}
	p.inner.StepOver(depth)
}

// StepOut enables step-out mode relative to the most recent break. It is a
// no-op when no break has happened.
func (p *Debugger) StepOut() {
	depth, ok := p.breakDepth()
	if !ok {
		return
	}
	p.inner.StepOut(depth)
}

// breakDepth returns the call depth recorded at the most recent break.
func (p *Debugger) breakDepth() (int, bool) {
	state, _ := p.inner.BreakState()
	return breakDepthOf(state)
}

// breakDepthOf reads the call depth out of a break snapshot. That depth is the
// break POINT's; the context applying a verdict is suspended at the break
// boundary and its own depth is a different number.
func breakDepthOf(state values.DebugState) (int, bool) {
	snap, ok := state.(*machine.BreakSnapshot)
	if !ok {
		return 0, false
	}
	return snap.CallDepth(), true
}

// Continue resumes execution.
func (p *Debugger) Continue() {
	p.inner.Continue()
}

// IsStepping returns true if the debugger is in step mode.
func (p *Debugger) IsStepping() bool {
	return p.inner.IsStepping()
}

// OnBreak sets the render-only callback for a breakpoint hit or a completed
// step. The DebugState provides source location and stack trace access without
// exposing VM internals.
//
// It is the fallback, not the general notification: a stop that SUSPENDS invokes
// [Debugger.OnBreakSuspend] instead and does not invoke this. With a suspension
// handler registered, the stops that still reach here are the ones that cannot
// suspend — breaks inside load and eval, and any run the boundary was not armed
// for.
func (p *Debugger) OnBreak(fn func(state values.DebugState, bp *BreakpointInfo)) {
	p.onBreak = fn
}

// OnBreakSuspend installs the handler that runs while the VM is SUSPENDED at a
// breakpoint or step stop, and whose verdict decides what happens next.
//
// nil means NONE: with no suspend handler the VM does not suspend at all and the
// render-only [Debugger.OnBreak] callback is invoked inline instead, which is the
// behaviour an embedder with a Go-only debugger has always had.
//
// Breaks inside load and eval do not suspend. Their freshly compiled template
// runs on a sub-context whose own continuation chain does not carry the break
// prompt, so those stops fall back to OnBreak.
func (p *Debugger) OnBreakSuspend(fn func(state values.DebugState, bp *BreakpointInfo) BreakAction) {
	p.onBreakSuspend = fn
}

// CurrentState returns the DebugState from the most recent break, or nil if no
// break has occurred.
//
// It is a snapshot taken at the break, not the live VM context. The context is
// pool-recycled and zeroed when the evaluation ends, so handing it back was how
// ,where and ,backtrace came to report "No source location available" and
// "Empty stack trace" on a breakpoint that had just printed its location.
func (p *Debugger) CurrentState() values.DebugState {
	state, _ := p.inner.BreakState()
	return state
}

// breakHandler builds the callable the VM applies while suspended at a break.
// It receives the resumable composable continuation as its single argument,
// asks the registered verdict function what to do, and either resumes that
// continuation (every verdict but BreakAbandon) or aborts to tag.
//
// It lives here, not in pkg/repl, because the bridge needs machine types and
// pkg/repl deliberately does not import pkg/machine.
func (p *Debugger) breakHandler(env *environment.EnvironmentFrame, tag *machine.PromptTag) *machine.ForeignClosure {
	fn := func(cc machine.CallContext) error {
		mc, err := machine.RequireMachineContext(cc, "debugger-break")
		if err != nil {
			return err
		}
		state, bp := p.inner.BreakState()
		action := BreakContinue
		if p.onBreakSuspend != nil {
			action = p.onBreakSuspend(state, machineBreakpointToInfo(bp))
		}
		if action == BreakAbandon {
			// Disarm first: the after-thunks resolveAbort is about to run below
			// this prompt are the user's code, and stopping in them while
			// abandoning would suspend on a computation already discarded.
			mc.SetDebugger(nil)
			return &machine.ErrPromptAbort{
				Tag:           tag,
				Values:        []values.Value{values.Void},
				SourceWinding: mc.WindingStack().Copy(),
			}
		}
		depth, known := breakDepthOf(state)
		if !known {
			// No recorded break means no depth to step relative to, and a step
			// mode armed against a meaningless depth would never stop again.
			action = BreakContinue
		}
		p.armStepMode(action, depth)
		// Resume, re-delivering the value register frozen at the break — resuming
		// with no arguments would clear it. applyForeign returns early rather than
		// restoring, because applying a composable continuation repoints the
		// template.
		_, aerr := mc.ApplyCallable(mc.Arg(0), mc.BreakResumeValues()...)
		return aerr
	}
	return machine.NewForeignClosure(env, 1, false, fn)
}

// armStepMode translates a non-abandon verdict into the debugger's step mode,
// relative to the call depth at the break point.
func (p *Debugger) armStepMode(action BreakAction, depth int) {
	switch action {
	case BreakStep:
		p.inner.StepInto()
	case BreakNext:
		p.inner.StepOver(depth)
	case BreakFinish:
		p.inner.StepOut(depth)
	default:
		p.inner.Continue()
	}
}

// machineDebugger returns the wrapped machine.Debugger for Engine use.
func (p *Debugger) machineDebugger() *machine.Debugger {
	return p.inner
}

// canStop reports whether an enabled breakpoint or an armed step mode could stop
// the next run. See [machine.Debugger.CanStop] for why arming on nothing costs.
func (p *Debugger) canStop() bool {
	return p.inner.CanStop()
}

func machineBreakpointToInfo(bp *machine.Breakpoint) *BreakpointInfo {
	if bp == nil {
		return nil
	}
	return &BreakpointInfo{
		ID:       int(bp.ID),
		File:     bp.File,
		Line:     bp.Line,
		Column:   bp.Column,
		Enabled:  bp.Enabled,
		HitCount: bp.HitCount,
	}
}
