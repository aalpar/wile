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
func (d *Debugger) SetBreakpoint(file string, line, column int) BreakpointID {
	d.mu.Lock()
	defer d.mu.Unlock()

	id := d.nextID
	d.nextID++

	d.breakpoints[id] = &Breakpoint{
		ID:      id,
		File:    file,
		Line:    line,
		Column:  column,
		Enabled: true,
	}
	return id
}

// RemoveBreakpoint removes a breakpoint.
func (d *Debugger) RemoveBreakpoint(id BreakpointID) bool {
	d.mu.Lock()
	defer d.mu.Unlock()

	if _, ok := d.breakpoints[id]; ok {
		delete(d.breakpoints, id)
		return true
	}
	return false
}

// EnableBreakpoint enables a breakpoint.
func (d *Debugger) EnableBreakpoint(id BreakpointID) bool {
	d.mu.Lock()
	defer d.mu.Unlock()

	if bp, ok := d.breakpoints[id]; ok {
		bp.Enabled = true
		return true
	}
	return false
}

// DisableBreakpoint disables a breakpoint.
func (d *Debugger) DisableBreakpoint(id BreakpointID) bool {
	d.mu.Lock()
	defer d.mu.Unlock()

	if bp, ok := d.breakpoints[id]; ok {
		bp.Enabled = false
		return true
	}
	return false
}

// Breakpoints returns all breakpoints.
func (d *Debugger) Breakpoints() []*Breakpoint {
	d.mu.RLock()
	defer d.mu.RUnlock()

	result := make([]*Breakpoint, 0, len(d.breakpoints))
	for _, bp := range d.breakpoints {
		result = append(result, bp)
	}
	return result
}

// CheckBreakpoint checks if execution should break at current location.
func (d *Debugger) CheckBreakpoint(mc *MachineContext) *Breakpoint {
	source := mc.CurrentSource()
	if source == nil {
		return nil
	}

	d.mu.RLock()
	defer d.mu.RUnlock()

	for _, bp := range d.breakpoints {
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
func (d *Debugger) ShouldStep(mc *MachineContext) bool {
	if !d.stepping {
		return false
	}

	switch d.stepMode {
	case StepInto:
		return mc.CurrentSource() != nil
	case StepOver:
		// Only break if we're in the same or shallower frame
		return mc.CurrentSource() != nil && mc.CallDepth() <= d.stepFrameDepth
	case StepOut:
		// Only break if we've returned from the target frame
		return mc.cont != d.stepFrame
	}
	return false
}

// Continue resumes execution.
func (d *Debugger) Continue() {
	d.stepping = false
}

// StepInto enables step-into mode.
func (d *Debugger) StepInto() {
	d.stepping = true
	d.stepMode = StepInto
}

// StepOver enables step-over mode.
func (d *Debugger) StepOver(mc *MachineContext) {
	d.stepping = true
	d.stepMode = StepOver
	d.stepFrameDepth = mc.CallDepth()
}

// StepOut enables step-out mode.
func (d *Debugger) StepOut(mc *MachineContext) {
	d.stepping = true
	d.stepMode = StepOut
	d.stepFrame = mc.cont
}

// OnBreak sets the callback for when a breakpoint is hit.
func (d *Debugger) OnBreak(fn func(mc *MachineContext, bp *Breakpoint)) {
	d.onBreak = fn
}

// TriggerBreak calls the break callback if set.
func (d *Debugger) TriggerBreak(mc *MachineContext, bp *Breakpoint) {
	if d.onBreak != nil {
		d.onBreak(mc, bp)
	}
}

// IsStepping returns whether the debugger is in stepping mode.
func (d *Debugger) IsStepping() bool {
	return d.stepping
}
