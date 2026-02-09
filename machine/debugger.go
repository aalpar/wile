// Copyright 2025 Aaron Alpar
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
	"sync"
)

// BreakpointID uniquely identifies a breakpoint.
type BreakpointID int

// Breakpoint represents a source-level breakpoint.
type Breakpoint struct {
	ID       BreakpointID
	File     string
	Line     int
	Column   int // 0 = any column on line
	Enabled  bool
	HitCount int
}

// StepMode represents the stepping mode for the debugger.
type StepMode int

const (
	StepNone StepMode = iota
	StepInto          // Step to next source location
	StepOver          // Step to next source location in same or parent frame
	StepOut           // Step until current frame returns
)

// Debugger manages breakpoints and stepping.
type Debugger struct {
	mu          sync.RWMutex
	breakpoints map[BreakpointID]*Breakpoint
	nextID      BreakpointID

	// Stepping state
	stepping       bool
	stepMode       StepMode
	stepFrameDepth int                  // For step-over
	stepFrame      *MachineContinuation // For step-out

	// Callback when breakpoint hit
	onBreak func(mc *MachineContext, bp *Breakpoint)
}

// NewDebugger creates a new debugger.
func NewDebugger() *Debugger {
	return &Debugger{
		breakpoints: make(map[BreakpointID]*Breakpoint),
	}
}

// SetBreakpoint adds a breakpoint at the given source location.
func (p *Debugger) SetBreakpoint(file string, line, column int) BreakpointID {
	p.mu.Lock()
	defer p.mu.Unlock()

	id := p.nextID
	p.nextID++

	p.breakpoints[id] = &Breakpoint{
		ID:      id,
		File:    file,
		Line:    line,
		Column:  column,
		Enabled: true,
	}
	return id
}

// RemoveBreakpoint removes a breakpoint.
func (p *Debugger) RemoveBreakpoint(id BreakpointID) bool {
	p.mu.Lock()
	defer p.mu.Unlock()

	if _, ok := p.breakpoints[id]; ok { //nolint:gocritic
		delete(p.breakpoints, id)
		return true
	}
	return false
}

// EnableBreakpoint enables a breakpoint.
func (p *Debugger) EnableBreakpoint(id BreakpointID) bool {
	p.mu.Lock()
	defer p.mu.Unlock()

	if bp, ok := p.breakpoints[id]; ok { //nolint:gocritic
		bp.Enabled = true
		return true
	}
	return false
}

// DisableBreakpoint disables a breakpoint.
func (p *Debugger) DisableBreakpoint(id BreakpointID) bool {
	p.mu.Lock()
	defer p.mu.Unlock()

	if bp, ok := p.breakpoints[id]; ok { //nolint:gocritic
		bp.Enabled = false
		return true
	}
	return false
}

// Breakpoints returns all breakpoints.
func (p *Debugger) Breakpoints() []*Breakpoint {
	p.mu.RLock()
	defer p.mu.RUnlock()

	result := make([]*Breakpoint, 0, len(p.breakpoints))
	for _, bp := range p.breakpoints {
		result = append(result, bp)
	}
	return result
}

// CheckBreakpoint checks if execution should break at current location.
func (p *Debugger) CheckBreakpoint(mc *MachineContext) *Breakpoint {
	source := mc.CurrentSource()
	if source == nil {
		return nil
	}

	p.mu.RLock()
	defer p.mu.RUnlock()

	for _, bp := range p.breakpoints {
		if !bp.Enabled {
			continue
		}
		if bp.File == source.File && bp.Line == source.Start.Line() {
			if bp.Column == 0 || bp.Column == source.Start.Column() {
				bp.HitCount++
				return bp
			}
		}
	}
	return nil
}

// ShouldStep checks if we should break due to stepping.
func (p *Debugger) ShouldStep(mc *MachineContext) bool {
	if !p.stepping {
		return false
	}

	switch p.stepMode {
	case StepInto:
		return mc.CurrentSource() != nil
	case StepOver:
		// Only break if we're in the same or shallower frame
		return mc.CurrentSource() != nil && mc.CallDepth() <= p.stepFrameDepth
	case StepOut:
		// Only break if we've returned from the target frame
		return mc.cont != p.stepFrame
	}
	return false
}

// Continue resumes execution.
func (p *Debugger) Continue() {
	p.stepping = false
}

// StepInto enables step-into mode.
func (p *Debugger) StepInto() {
	p.stepping = true
	p.stepMode = StepInto
}

// StepOver enables step-over mode.
func (p *Debugger) StepOver(mc *MachineContext) {
	p.stepping = true
	p.stepMode = StepOver
	p.stepFrameDepth = mc.CallDepth()
}

// StepOut enables step-out mode.
func (p *Debugger) StepOut(mc *MachineContext) {
	p.stepping = true
	p.stepMode = StepOut
	p.stepFrame = mc.cont
}

// OnBreak sets the callback for when a breakpoint is hit.
func (p *Debugger) OnBreak(fn func(mc *MachineContext, bp *Breakpoint)) {
	p.onBreak = fn
}

// TriggerBreak calls the break callback if set.
func (p *Debugger) TriggerBreak(mc *MachineContext, bp *Breakpoint) {
	if p.onBreak != nil {
		p.onBreak(mc, bp)
	}
}

// IsStepping returns whether the debugger is in stepping mode.
func (p *Debugger) IsStepping() bool {
	return p.stepping
}
