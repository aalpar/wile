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
	"slices"
	"sync"

	"github.com/aalpar/wile/pkg/values"
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

	// Stepping state — stepMode == StepNone means not stepping. Guarded by mu:
	// under SRFI-18 the thread that sets a step mode is not the thread that
	// evaluates it, and `breakpoints` next to it has always been guarded.
	stepMode StepMode
	// stepFrameDepth is the call depth at the stop that armed the step, for
	// both step-over and step-out; the two differ only in the comparison
	// ShouldStep applies to it.
	stepFrameDepth int

	// breakSnap/breakBP are the most recent break's frozen state, recorded by
	// setBreak and read by BreakState. Guarded by mu for the same reason.
	breakSnap *BreakSnapshot
	breakBP   *Breakpoint

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

	_, ok := p.breakpoints[id]
	if ok {
		delete(p.breakpoints, id)
		return true
	}
	return false
}

// EnableBreakpoint enables a breakpoint.
func (p *Debugger) EnableBreakpoint(id BreakpointID) bool {
	p.mu.Lock()
	defer p.mu.Unlock()

	bp, ok := p.breakpoints[id]
	if ok {
		bp.Enabled = true
		return true
	}
	return false
}

// DisableBreakpoint disables a breakpoint.
func (p *Debugger) DisableBreakpoint(id BreakpointID) bool {
	p.mu.Lock()
	defer p.mu.Unlock()

	bp, ok := p.breakpoints[id]
	if ok {
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

// CheckBreakpoint reports the breakpoint execution should stop at, or nil.
//
// It is not a pure query: a matching breakpoint's HitCount is incremented, and
// mc's de-duplication cursor is advanced on every call. The increment is a write
// and is taken under the WRITE lock, so two SRFI-18 threads sharing one Debugger
// no longer lose increments against each other. That is all the lock buys: the
// returned *Breakpoint is the live object, and every field a caller then reads
// (HitCount, Enabled) is read outside this lock. The returned pointer being live
// is deliberate — callers see Enable/Disable's effect — not a claim of safety.
//
// Each breakpoint fires once per ENTRY to its source line, not once per
// instruction on it: a single source line compiles to several instructions, and
// stopping on each of them reported one stop as several (four, for a `(+ x x)`
// body) and inflated HitCount by the same factor. The cursor advances on every
// call so that leaving a line and returning to it re-arms — which is what makes
// a procedure called four times break four times.
//
// Suppression is per BREAKPOINT, not per line. Keying it on the line alone let
// the first breakpoint to fire mask every other breakpoint on that line for the
// whole entry — including one on a later column that had not yet been reached —
// and left which of them survived to Go's map iteration order.
//
// Known limitation: a recursion or loop whose ENTIRE body is one source line
// never leaves that line, so it breaks once, not once per activation.
func (p *Debugger) CheckBreakpoint(mc *MachineContext) *Breakpoint {
	source := mc.CurrentSource()
	if source == nil {
		return nil
	}

	line := source.Start.Line()
	if source.File != mc.lastBreakFile || line != mc.lastBreakLine {
		mc.lastBreakFile = source.File
		mc.lastBreakLine = line
		mc.lastBreakFired = false
		mc.lastBreakIDs = mc.lastBreakIDs[:0]
	}

	p.mu.Lock()
	defer p.mu.Unlock()

	for _, bp := range p.breakpoints {
		if !bp.Enabled {
			continue
		}
		if bp.File != source.File || bp.Line != line {
			continue
		}
		// Column 0 means "any column on the line"; a column-qualified breakpoint
		// (,break f:2:7) sits on an instruction that is typically not the first
		// one carrying line 2.
		if bp.Column != 0 && bp.Column != source.Start.Column() {
			continue
		}
		if slices.Contains(mc.lastBreakIDs, bp.ID) {
			continue
		}
		bp.HitCount++
		mc.lastBreakIDs = append(mc.lastBreakIDs, bp.ID)
		mc.lastBreakFired = true
		return bp
	}
	return nil
}

// CanStop reports whether anything could stop execution right now: an enabled
// breakpoint, or an armed step mode.
//
// It is the arming test for the break boundary. A boundary nothing can reach is
// not free — it is a real continuation frame, so it shows up in every stack
// trace and costs one unit of the call-depth budget for the whole run.
//
// It is a snapshot, not a subscription: a breakpoint set from another goroutine
// after a run has begun is not seen by that run, which falls back to the
// render-only callback for its remainder.
func (p *Debugger) CanStop() bool {
	p.mu.RLock()
	defer p.mu.RUnlock()

	if p.stepMode != StepNone {
		return true
	}
	for _, bp := range p.breakpoints {
		if bp.Enabled {
			return true
		}
	}
	return false
}

// setBreak records the frozen state of a break so it outlives the suspension and
// the MachineContext that produced it.
func (p *Debugger) setBreak(snap *BreakSnapshot, bp *Breakpoint) {
	p.mu.Lock()
	defer p.mu.Unlock()

	p.breakSnap = snap
	p.breakBP = bp
}

// BreakState returns the most recent break's frozen state and the breakpoint that
// caused it, or (nil, nil) if no break has happened. The breakpoint is nil for a
// step stop.
func (p *Debugger) BreakState() (values.DebugState, *Breakpoint) {
	p.mu.RLock()
	defer p.mu.RUnlock()

	if p.breakSnap == nil {
		// Returning p.breakSnap directly would hand back a non-nil interface
		// holding a nil pointer, which every `== nil` caller reads as "there was
		// a break".
		return nil, nil
	}
	return p.breakSnap, p.breakBP
}

// ShouldStep checks if we should break due to stepping.
func (p *Debugger) ShouldStep(mc *MachineContext) bool {
	p.mu.RLock()
	defer p.mu.RUnlock()

	switch p.stepMode {
	case StepNone:
		return false
	case StepInto:
		return mc.CurrentSource() != nil
	case StepOver:
		// Only break if we're in the same or shallower frame
		return mc.CurrentSource() != nil && mc.CallDepth() <= p.stepFrameDepth
	case StepOut:
		// Only break once we are STRICTLY shallower than the frame that armed
		// the step — that is what "the current frame returned" means. Keying on
		// the frame POINTER instead cannot work across a suspension:
		// SliceContinuationAt deep-copies every frame it captures, so a resumed
		// computation runs on Copy()s and the pointer differs at the first
		// opcode after the resume, turning step-out into step-into.
		return mc.CurrentSource() != nil && mc.CallDepth() < p.stepFrameDepth
	default:
		return false
	}
}

// Continue resumes execution.
func (p *Debugger) Continue() {
	p.mu.Lock()
	defer p.mu.Unlock()

	p.stepMode = StepNone
	p.stepFrameDepth = 0
}

// StepInto enables step-into mode.
func (p *Debugger) StepInto() {
	p.mu.Lock()
	defer p.mu.Unlock()

	p.stepMode = StepInto
	p.stepFrameDepth = 0
}

// StepOver enables step-over mode, relative to the call depth at the stop that
// armed it. Callers pass the depth explicitly because the context that applies
// the verdict is suspended at the break BOUNDARY, whose depth is not the break
// point's.
func (p *Debugger) StepOver(depth int) {
	p.mu.Lock()
	defer p.mu.Unlock()

	p.stepMode = StepOver
	p.stepFrameDepth = depth
}

// StepOut enables step-out mode, relative to the call depth at the stop that
// armed it. See StepOver for why the depth is a parameter.
func (p *Debugger) StepOut(depth int) {
	p.mu.Lock()
	defer p.mu.Unlock()

	p.stepMode = StepOut
	p.stepFrameDepth = depth
}

// OnBreak sets the callback for when a breakpoint is hit.
func (p *Debugger) OnBreak(fn func(mc *MachineContext, bp *Breakpoint)) {
	p.onBreak = fn
}

// TriggerBreak records the break and calls the break callback if set. It is the
// fallback taken when no break boundary is armed on mc: the callback runs INLINE
// on the live context, so it can render the stop but cannot influence execution.
//
// The state is frozen into a snapshot here for the same reason the suspending
// path does it — mc is pool-recycled and zeroed when the evaluation ends, so a
// consumer reading it after the run gets a blank context.
func (p *Debugger) TriggerBreak(mc *MachineContext, bp *Breakpoint) {
	// A nil context is part of this method's contract (see
	// TestDebugger_TriggerBreak_NoCallback): there is simply nothing to freeze.
	if mc != nil {
		p.setBreak(newBreakSnapshot(mc), bp)
	}
	if p.onBreak != nil {
		p.onBreak(mc, bp)
	}
}

// IsStepping returns whether the debugger is in stepping mode. It takes the read
// lock like every other stepMode accessor: an embedder polling this from a
// supervising goroutine while a break handler on the VM goroutine arms a step
// mode is otherwise an unsynchronized read of the same field.
func (p *Debugger) IsStepping() bool {
	p.mu.RLock()
	defer p.mu.RUnlock()

	return p.stepMode != StepNone
}
