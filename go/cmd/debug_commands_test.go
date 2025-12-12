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

package main

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestNewDebugContext(t *testing.T) {
	dc := NewDebugContext()

	qt.Assert(t, dc, qt.IsNotNil)
	qt.Assert(t, dc.Debugger(), qt.IsNotNil)
	qt.Assert(t, dc.currentMC, qt.IsNil)
}

func TestDebugContext_HandleDebugCommand_NotACommand(t *testing.T) {
	dc := NewDebugContext()

	// Non-command lines should return false
	qt.Assert(t, dc.HandleDebugCommand("(+ 1 2)"), qt.IsFalse)
	qt.Assert(t, dc.HandleDebugCommand("  hello"), qt.IsFalse)
	qt.Assert(t, dc.HandleDebugCommand(""), qt.IsFalse)
}

func TestDebugContext_HandleDebugCommand_EmptyCommand(t *testing.T) {
	dc := NewDebugContext()

	// Empty command (just comma) should return true (consumed)
	qt.Assert(t, dc.HandleDebugCommand(","), qt.IsTrue)
	qt.Assert(t, dc.HandleDebugCommand("  ,  "), qt.IsTrue)
}

func TestDebugContext_HandleDebugCommand_Break(t *testing.T) {
	dc := NewDebugContext()

	// Set a breakpoint
	dc.HandleDebugCommand(",break test.scm:10")

	bps := dc.Debugger().Breakpoints()
	qt.Assert(t, bps, qt.HasLen, 1)
	qt.Assert(t, bps[0].File, qt.Equals, "test.scm")
	qt.Assert(t, bps[0].Line, qt.Equals, 10)
	qt.Assert(t, bps[0].Column, qt.Equals, 0)
	qt.Assert(t, bps[0].Enabled, qt.IsTrue)
}

func TestDebugContext_HandleDebugCommand_Break_WithColumn(t *testing.T) {
	dc := NewDebugContext()

	dc.HandleDebugCommand(",break test.scm:10:5")

	bps := dc.Debugger().Breakpoints()
	qt.Assert(t, bps, qt.HasLen, 1)
	qt.Assert(t, bps[0].Column, qt.Equals, 5)
}

func TestDebugContext_HandleDebugCommand_Break_Alias(t *testing.T) {
	dc := NewDebugContext()

	dc.HandleDebugCommand(",b test.scm:20")

	bps := dc.Debugger().Breakpoints()
	qt.Assert(t, bps, qt.HasLen, 1)
	qt.Assert(t, bps[0].Line, qt.Equals, 20)
}

func TestDebugContext_HandleDebugCommand_Delete(t *testing.T) {
	dc := NewDebugContext()

	// Set a breakpoint first
	dc.HandleDebugCommand(",break test.scm:10")
	qt.Assert(t, dc.Debugger().Breakpoints(), qt.HasLen, 1)

	// Delete it
	dc.HandleDebugCommand(",delete 0")
	qt.Assert(t, dc.Debugger().Breakpoints(), qt.HasLen, 0)
}

func TestDebugContext_HandleDebugCommand_EnableDisable(t *testing.T) {
	dc := NewDebugContext()

	dc.HandleDebugCommand(",break test.scm:10")
	bps := dc.Debugger().Breakpoints()
	qt.Assert(t, bps[0].Enabled, qt.IsTrue)

	// Disable
	dc.HandleDebugCommand(",disable 0")
	bps = dc.Debugger().Breakpoints()
	qt.Assert(t, bps[0].Enabled, qt.IsFalse)

	// Enable
	dc.HandleDebugCommand(",enable 0")
	bps = dc.Debugger().Breakpoints()
	qt.Assert(t, bps[0].Enabled, qt.IsTrue)
}

func TestDebugContext_HandleDebugCommand_Step(t *testing.T) {
	dc := NewDebugContext()

	dc.HandleDebugCommand(",step")
	qt.Assert(t, dc.Debugger().IsStepping(), qt.IsTrue)
}

func TestDebugContext_HandleDebugCommand_Step_Alias(t *testing.T) {
	dc := NewDebugContext()

	dc.HandleDebugCommand(",s")
	qt.Assert(t, dc.Debugger().IsStepping(), qt.IsTrue)
}

func TestDebugContext_HandleDebugCommand_Continue(t *testing.T) {
	dc := NewDebugContext()

	// Start stepping
	dc.HandleDebugCommand(",step")
	qt.Assert(t, dc.Debugger().IsStepping(), qt.IsTrue)

	// Continue should stop stepping
	dc.HandleDebugCommand(",continue")
	qt.Assert(t, dc.Debugger().IsStepping(), qt.IsFalse)
}

func TestDebugContext_HandleDebugCommand_Continue_Alias(t *testing.T) {
	dc := NewDebugContext()

	dc.HandleDebugCommand(",step")
	dc.HandleDebugCommand(",c")
	qt.Assert(t, dc.Debugger().IsStepping(), qt.IsFalse)
}

func TestDebugContext_HandleDebugCommand_List(t *testing.T) {
	dc := NewDebugContext()

	// List with no breakpoints should work
	qt.Assert(t, dc.HandleDebugCommand(",list"), qt.IsTrue)

	// Add some breakpoints
	dc.HandleDebugCommand(",break file1.scm:10")
	dc.HandleDebugCommand(",break file2.scm:20")

	// List should still work
	qt.Assert(t, dc.HandleDebugCommand(",list"), qt.IsTrue)
	qt.Assert(t, dc.HandleDebugCommand(",l"), qt.IsTrue)
}

func TestDebugContext_HandleDebugCommand_Help(t *testing.T) {
	dc := NewDebugContext()

	qt.Assert(t, dc.HandleDebugCommand(",help"), qt.IsTrue)
	qt.Assert(t, dc.HandleDebugCommand(",h"), qt.IsTrue)
	qt.Assert(t, dc.HandleDebugCommand(",?"), qt.IsTrue)
}

func TestDebugContext_HandleDebugCommand_Unknown(t *testing.T) {
	dc := NewDebugContext()

	// Unknown command should return true (consumed) but print error
	qt.Assert(t, dc.HandleDebugCommand(",foobar"), qt.IsTrue)
}

func TestParseLocation(t *testing.T) {
	tests := []struct {
		input      string
		wantFile   string
		wantLine   int
		wantColumn int
	}{
		{"test.scm:10", "test.scm", 10, 0},
		{"test.scm:10:5", "test.scm", 10, 5},
		{"path/to/file.scm:100", "path/to/file.scm", 100, 0},
		{"path/to/file.scm:100:25", "path/to/file.scm", 100, 25},
		{"", "", 0, 0},
		{"nocolon", "", 0, 0},
		{"file:notanumber", "", 0, 0},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			file, line, col := parseLocation(tt.input)
			qt.Assert(t, file, qt.Equals, tt.wantFile)
			qt.Assert(t, line, qt.Equals, tt.wantLine)
			qt.Assert(t, col, qt.Equals, tt.wantColumn)
		})
	}
}
