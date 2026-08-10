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
	// BreakStep resumes and stops at the next expression, entering calls.
	BreakStep
	// BreakNext resumes and stops at the next expression in the same or a
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
	currentMC      *machine.MachineContext
	onBreak        func(state values.DebugState, bp *BreakpointInfo)
	onBreakSuspend func(state values.DebugState, bp *BreakpointInfo) BreakAction
}

// NewDebugger creates a new Debugger.
func NewDebugger() *Debugger {
	q := &Debugger{
		inner: machine.NewDebugger(),
	}
	q.inner.OnBreak(func(mc *machine.MachineContext, bp *machine.Breakpoint) {
		q.currentMC = mc
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

// StepOver enables step-over mode using the stored break context.
func (p *Debugger) StepOver() {
	if p.currentMC != nil {
		p.inner.StepOver(p.currentMC)
	}
}

// StepOut enables step-out mode using the stored break context.
func (p *Debugger) StepOut() {
	if p.currentMC != nil {
		p.inner.StepOut(p.currentMC)
	}
}

// Continue resumes execution.
func (p *Debugger) Continue() {
	p.inner.Continue()
}

// IsStepping returns true if the debugger is in step mode.
func (p *Debugger) IsStepping() bool {
	return p.inner.IsStepping()
}

// OnBreak sets the callback invoked when a breakpoint is hit or a
// step completes. The DebugState provides source location and stack
// trace access without exposing VM internals.
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

// CurrentState returns the DebugState from the most recent break, or
// nil if no break has occurred.
func (p *Debugger) CurrentState() values.DebugState {
	if p.currentMC == nil {
		return nil
	}
	return p.currentMC
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
		action := BreakContinue
		if p.onBreakSuspend != nil {
			state, bp := p.inner.BreakState()
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
		p.armStepMode(action, mc)
		// Resume. applyForeign returns early rather than restoring, because
		// applying a composable continuation repoints the template.
		_, aerr := mc.ApplyCallable(mc.Arg(0))
		return aerr
	}
	return machine.NewForeignClosure(env, 1, false, fn)
}

// armStepMode translates a non-abandon verdict into the debugger's step mode.
func (p *Debugger) armStepMode(action BreakAction, mc *machine.MachineContext) {
	switch action {
	case BreakStep:
		p.inner.StepInto()
	case BreakNext:
		p.inner.StepOver(mc)
	case BreakFinish:
		p.inner.StepOut(mc)
	default:
		p.inner.Continue()
	}
}

// machineDebugger returns the wrapped machine.Debugger for Engine use.
func (p *Debugger) machineDebugger() *machine.Debugger {
	return p.inner
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
