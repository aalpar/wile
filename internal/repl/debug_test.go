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

package repl

import (
	"bytes"
	"slices"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestNewDebugContext(t *testing.T) {
	dc := NewDebugContext()
	qt.Assert(t, dc, qt.IsNotNil)
	qt.Assert(t, dc.Debugger(), qt.IsNotNil)
}

func TestHandleDebugCommand(t *testing.T) {
	tcs := []struct {
		name    string
		setup   func(dc *DebugContext) // optional setup before handling
		input   string
		handled bool
		contain string // substring expected in output (empty = no check)
	}{
		{
			name:    "break adds breakpoint",
			input:   ",break test.scm:10",
			handled: true,
			contain: "Breakpoint 0 set at test.scm:10",
		},
		{
			name:    "list with no breakpoints",
			input:   ",list",
			handled: true,
			contain: "No breakpoints set",
		},
		{
			name: "list with breakpoints",
			setup: func(dc *DebugContext) {
				dc.Debugger().SetBreakpoint("foo.scm", 5, 0)
			},
			input:   ",list",
			handled: true,
			contain: "foo.scm:5",
		},
		{
			name: "delete breakpoint",
			setup: func(dc *DebugContext) {
				dc.Debugger().SetBreakpoint("foo.scm", 5, 0)
			},
			input:   ",delete 0",
			handled: true,
			contain: "deleted",
		},
		{
			name: "enable breakpoint",
			setup: func(dc *DebugContext) {
				dc.Debugger().SetBreakpoint("foo.scm", 5, 0)
				dc.Debugger().DisableBreakpoint(0)
			},
			input:   ",enable 0",
			handled: true,
			contain: "enabled",
		},
		{
			name: "disable breakpoint",
			setup: func(dc *DebugContext) {
				dc.Debugger().SetBreakpoint("foo.scm", 5, 0)
			},
			input:   ",disable 0",
			handled: true,
			contain: "disabled",
		},
		{
			name:    "step sets step mode",
			input:   ",step",
			handled: true,
			contain: "Will step into",
		},
		{
			name:    "next without context",
			input:   ",next",
			handled: true,
			contain: "No active execution context",
		},
		{
			name:    "continue resumes",
			input:   ",continue",
			handled: true,
			contain: "Continuing execution",
		},
		{
			name:    "backtrace without context",
			input:   ",backtrace",
			handled: true,
			contain: "No active execution context",
		},
		{
			name:    "where without context",
			input:   ",where",
			handled: true,
			contain: "No active execution context",
		},
		{
			name:    "unknown command is still handled",
			input:   ",unknown",
			handled: true,
			contain: "Unknown command",
		},
		{
			name:    "non-command returns false",
			input:   "(+ 1 2)",
			handled: false,
			contain: "",
		},
		{
			name:    "empty comma",
			input:   ",",
			handled: true,
			contain: "",
		},
		{
			name:    "help command",
			input:   ",help",
			handled: true,
			contain: "Debug commands",
		},
		{
			name:    "alias b for break",
			input:   ",b test.scm:20",
			handled: true,
			contain: "Breakpoint",
		},
		{
			name:    "alias s for step",
			input:   ",s",
			handled: true,
			contain: "Will step into",
		},
		{
			name:    "alias c for continue",
			input:   ",c",
			handled: true,
			contain: "Continuing execution",
		},
		{
			name:    "finish without context",
			input:   ",finish",
			handled: true,
			contain: "No active execution context",
		},
		{
			name:    "break with column",
			input:   ",break test.scm:10:5",
			handled: true,
			contain: "test.scm:10:5",
		},
		{
			name:    "break missing args",
			input:   ",break",
			handled: true,
			contain: "Usage",
		},
		{
			name:    "delete invalid id",
			input:   ",delete abc",
			handled: true,
			contain: "Invalid breakpoint ID",
		},
		{
			name:    "delete nonexistent breakpoint",
			input:   ",delete 99",
			handled: true,
			contain: "not found",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			dc := NewDebugContext()
			if tc.setup != nil {
				tc.setup(dc)
			}
			var buf bytes.Buffer
			handled := dc.HandleDebugCommand(tc.input, &buf)
			qt.Assert(t, handled, qt.Equals, tc.handled)
			if tc.contain != "" {
				qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
					qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
			}
		})
	}
}

func TestHandleDebugCommand_StepMode(t *testing.T) {
	dc := NewDebugContext()
	qt.Assert(t, dc.Debugger().IsStepping(), qt.IsFalse)

	var buf bytes.Buffer
	dc.HandleDebugCommand(",step", &buf)
	qt.Assert(t, dc.Debugger().IsStepping(), qt.IsTrue)

	buf.Reset()
	dc.HandleDebugCommand(",continue", &buf)
	qt.Assert(t, dc.Debugger().IsStepping(), qt.IsFalse)
}

func TestDebugCommands(t *testing.T) {
	dc := NewDebugContext()
	cmds := dc.DebugCommands()
	qt.Assert(t, len(cmds) > 0, qt.IsTrue)

	names := make([]string, len(cmds))
	for i, cmd := range cmds {
		names[i] = cmd.Name
	}

	for _, expected := range []string{"break", "step", "continue", "backtrace", "where", "list", "delete", "enable", "disable", "next", "finish", "help"} {
		qt.Assert(t, slices.Contains(names, expected), qt.IsTrue,
			qt.Commentf("DebugCommands should contain %q, got %v", expected, names))
	}

	// Verify each command has a non-nil handler
	for _, cmd := range cmds {
		qt.Assert(t, cmd.Handler, qt.IsNotNil,
			qt.Commentf("command %q should have a non-nil handler", cmd.Name))
	}
}

func TestParseLocation(t *testing.T) {
	tcs := []struct {
		name   string
		input  string
		file   string
		line   int
		column int
	}{
		{"file and line", "test.scm:10", "test.scm", 10, 0},
		{"file line column", "test.scm:10:5", "test.scm", 10, 5},
		{"no colon", "test.scm", "", 0, 0},
		{"invalid line", "test.scm:abc", "", 0, 0},
		{"invalid column", "test.scm:10:abc", "test.scm", 10, 0},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			file, line, column := parseLocation(tc.input)
			qt.Assert(t, file, qt.Equals, tc.file)
			qt.Assert(t, line, qt.Equals, tc.line)
			qt.Assert(t, column, qt.Equals, tc.column)
		})
	}
}
