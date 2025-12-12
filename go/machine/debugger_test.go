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
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestDebugger_NewDebugger(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()
	c.Assert(d, qt.IsNotNil)
	c.Assert(d.breakpoints, qt.IsNotNil)
	c.Assert(len(d.breakpoints), qt.Equals, 0)
}

func TestDebugger_SetBreakpoint(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()
	id := d.SetBreakpoint("test.scm", 10, 0)

	c.Assert(id, qt.Equals, BreakpointID(0))

	bps := d.Breakpoints()
	c.Assert(len(bps), qt.Equals, 1)
	c.Assert(bps[0].File, qt.Equals, "test.scm")
	c.Assert(bps[0].Line, qt.Equals, 10)
	c.Assert(bps[0].Column, qt.Equals, 0)
	c.Assert(bps[0].Enabled, qt.IsTrue)
}

func TestDebugger_SetBreakpoint_MultipleBreakpoints(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()
	id1 := d.SetBreakpoint("test.scm", 10, 0)
	id2 := d.SetBreakpoint("test.scm", 20, 5)
	id3 := d.SetBreakpoint("other.scm", 5, 0)

	c.Assert(id1, qt.Equals, BreakpointID(0))
	c.Assert(id2, qt.Equals, BreakpointID(1))
	c.Assert(id3, qt.Equals, BreakpointID(2))
	c.Assert(len(d.Breakpoints()), qt.Equals, 3)
}

func TestDebugger_RemoveBreakpoint(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()
	id := d.SetBreakpoint("test.scm", 10, 0)

	removed := d.RemoveBreakpoint(id)
	c.Assert(removed, qt.IsTrue)
	c.Assert(len(d.Breakpoints()), qt.Equals, 0)
}

func TestDebugger_RemoveBreakpoint_NotFound(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()
	removed := d.RemoveBreakpoint(BreakpointID(999))
	c.Assert(removed, qt.IsFalse)
}

func TestDebugger_EnableDisableBreakpoint(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()
	id := d.SetBreakpoint("test.scm", 10, 0)

	// Initially enabled
	c.Assert(d.breakpoints[id].Enabled, qt.IsTrue)

	// Disable
	disabled := d.DisableBreakpoint(id)
	c.Assert(disabled, qt.IsTrue)
	c.Assert(d.breakpoints[id].Enabled, qt.IsFalse)

	// Enable
	enabled := d.EnableBreakpoint(id)
	c.Assert(enabled, qt.IsTrue)
	c.Assert(d.breakpoints[id].Enabled, qt.IsTrue)
}

func TestDebugger_EnableBreakpoint_NotFound(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()
	enabled := d.EnableBreakpoint(BreakpointID(999))
	c.Assert(enabled, qt.IsFalse)
}

func TestDebugger_DisableBreakpoint_NotFound(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()
	disabled := d.DisableBreakpoint(BreakpointID(999))
	c.Assert(disabled, qt.IsFalse)
}

func TestDebugger_StepModes(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()

	// Initially not stepping
	c.Assert(d.IsStepping(), qt.IsFalse)

	// Step into
	d.StepInto()
	c.Assert(d.IsStepping(), qt.IsTrue)
	c.Assert(d.stepMode, qt.Equals, StepInto)

	// Continue
	d.Continue()
	c.Assert(d.IsStepping(), qt.IsFalse)
}

func TestDebugger_OnBreak(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()

	called := false
	var capturedBp *Breakpoint

	d.OnBreak(func(mc *MachineContext, bp *Breakpoint) {
		called = true
		capturedBp = bp
	})

	bp := &Breakpoint{ID: 1, File: "test.scm", Line: 10}
	d.TriggerBreak(nil, bp)

	c.Assert(called, qt.IsTrue)
	c.Assert(capturedBp, qt.Equals, bp)
}

func TestDebugger_TriggerBreak_NoCallback(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()

	// Should not panic when no callback is set
	d.TriggerBreak(nil, nil)
	c.Assert(true, qt.IsTrue) // If we get here, no panic occurred
}

func TestStepMode_Constants(t *testing.T) {
	c := qt.New(t)

	c.Assert(StepNone, qt.Equals, StepMode(0))
	c.Assert(StepInto, qt.Equals, StepMode(1))
	c.Assert(StepOver, qt.Equals, StepMode(2))
	c.Assert(StepOut, qt.Equals, StepMode(3))
}
