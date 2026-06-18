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

// Debugger controls breakpoints and stepping for an Engine.
// It wraps the internal machine.Debugger to avoid exposing VM types.
type Debugger struct {
	inner     *machine.Debugger
	currentMC *machine.MachineContext
	onBreak   func(state values.DebugState, bp *BreakpointInfo)
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

// CurrentState returns the DebugState from the most recent break, or
// nil if no break has occurred.
func (p *Debugger) CurrentState() values.DebugState {
	if p.currentMC == nil {
		return nil
	}
	return p.currentMC
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
