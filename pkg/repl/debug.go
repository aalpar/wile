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

//nolint:errcheck // Debug command output doesn't need error handling
package repl

import (
	"cmp"
	"fmt"
	"io"
	"slices"
	"strconv"
	"strings"

	"github.com/aalpar/wile/pkg/wile"
)

// DebugContext holds the state for debug commands. The break state itself — a
// snapshot frozen at the most recent break, NOT a live VM context — is owned by
// the wrapped wile.Debugger and read through p.debugger.CurrentState().
// DebugContext deliberately keeps no parallel copy, so the two can never
// disagree. Handing back the live context instead is what made ,where and
// ,backtrace report "No source location available": it is pool-recycled and
// zeroed the moment the evaluation ends.
type DebugContext struct {
	debugger *wile.Debugger
}

// NewDebugContext creates a new debug context.
func NewDebugContext() *DebugContext {
	return &DebugContext{
		debugger: wile.NewDebugger(),
	}
}

// Debugger returns the debugger instance.
func (p *DebugContext) Debugger() *wile.Debugger {
	return p.debugger
}

// debugCommandMeta is the single source of truth for debug command metadata.
// Separated from DebugCommandInfo so init() can read metadata without
// allocating a DebugContext (and its wile.Debugger).
type debugCommandMeta struct {
	Name    string
	Aliases []string
	Summary string
	Detail  string
}

var debugCommandMetadata = []debugCommandMeta{
	{"break", []string{"b"}, "Set breakpoint at FILE:LINE[:COLUMN]",
		"Usage: ,break FILE:LINE[:COLUMN]"},
	{"delete", []string{"d"}, "Delete a breakpoint",
		"Usage: ,delete ID"},
	{"list", []string{"l"}, "List breakpoints",
		"Usage: ,list"},
	{"enable", nil, "Enable a breakpoint",
		"Usage: ,enable ID"},
	{"disable", nil, "Disable a breakpoint",
		"Usage: ,disable ID"},
	{"step", []string{"s"}, "Step into",
		"Usage: ,step"},
	{"next", []string{"n"}, "Step over",
		"Usage: ,next"},
	{"finish", []string{"f"}, "Step out",
		"Usage: ,finish"},
	{"continue", []string{"c"}, "Continue execution",
		"Usage: ,continue"},
	{"backtrace", []string{"bt"}, "Show stack trace",
		"Usage: ,backtrace"},
	{"where", nil, "Show current location",
		"Usage: ,where"},
	{"help", []string{"h", "?"}, "Show available debug commands",
		"Usage: ,help"},
}

// DebugCommandInfo describes a single debug command: its canonical name,
// aliases, summary text, detail text, and handler function.
type DebugCommandInfo struct {
	Name    string
	Aliases []string
	Summary string
	Detail  string
	Handler func(args []string, out io.Writer)
}

// DebugCommands returns the canonical list of debug commands with bound handlers.
func (p *DebugContext) DebugCommands() []DebugCommandInfo {
	handlers := []func(args []string, out io.Writer){
		p.cmdBreak, p.cmdDelete, p.cmdList,
		p.cmdEnable, p.cmdDisable,
		p.cmdStep, p.cmdNext, p.cmdFinish, p.cmdContinue,
		p.cmdBacktrace, p.cmdWhere, p.cmdHelp,
	}
	q := make([]DebugCommandInfo, len(debugCommandMetadata))
	for i, m := range debugCommandMetadata {
		q[i] = DebugCommandInfo{
			Name:    m.Name,
			Aliases: m.Aliases,
			Summary: m.Summary,
			Detail:  m.Detail,
			Handler: handlers[i],
		}
	}
	return q
}

// canonicalDebugCommand maps a typed token (comma already stripped) to the
// canonical debug command name, so alias handling lives in one table and the
// break prompt cannot drift from ,help.
func canonicalDebugCommand(token string) (string, bool) {
	for _, m := range debugCommandMetadata {
		if token == m.Name || slices.Contains(m.Aliases, token) {
			return m.Name, true
		}
	}
	return "", false
}

// HandleDebugCommand processes a debug command starting with ','.
// Returns true if a command was handled, false otherwise.
func (p *DebugContext) HandleDebugCommand(line string, out io.Writer) bool {
	line = strings.TrimSpace(line)
	if !strings.HasPrefix(line, ",") {
		return false
	}

	parts := strings.Fields(strings.TrimPrefix(line, ","))
	if len(parts) == 0 {
		return true // Empty command, just consume it
	}

	cmd := parts[0]
	args := parts[1:]

	for _, dc := range p.DebugCommands() {
		if cmd == dc.Name || slices.Contains(dc.Aliases, cmd) {
			dc.Handler(args, out)
			return true
		}
	}

	fmt.Fprintf(out, "Unknown command: %s (type ,help for commands)\n", cmd)
	return true
}

// cmdBreak sets a breakpoint.
func (p *DebugContext) cmdBreak(args []string, out io.Writer) {
	if len(args) < 1 {
		fmt.Fprintln(out, "Usage: ,break FILE:LINE[:COLUMN]")
		return
	}

	file, line, col := parseLocation(args[0])
	if file == "" || line == 0 {
		fmt.Fprintln(out, "Invalid location format. Use FILE:LINE or FILE:LINE:COLUMN")
		return
	}

	id := p.debugger.SetBreakpoint(file, line, col)
	fmt.Fprintf(out, "Breakpoint %d set at %s:%d", id, file, line)
	if col > 0 {
		fmt.Fprintf(out, ":%d", col)
	}
	fmt.Fprintln(out)
}

// breakpointAction parses a breakpoint ID from args and applies the given action.
func (p *DebugContext) breakpointAction(args []string, out io.Writer, verb string, action func(int) bool) {
	if len(args) < 1 {
		fmt.Fprintf(out, "Usage: ,%s ID\n", verb)
		return
	}

	id, err := strconv.ParseUint(args[0], 10, 31)
	if err != nil {
		fmt.Fprintf(out, "Invalid breakpoint ID: %s\n", args[0])
		return
	}

	if action(int(id)) {
		fmt.Fprintf(out, "Breakpoint %d %sd\n", id, verb)
	} else {
		fmt.Fprintf(out, "Breakpoint %d not found\n", id)
	}
}

// cmdDelete removes a breakpoint.
func (p *DebugContext) cmdDelete(args []string, out io.Writer) {
	p.breakpointAction(args, out, "delete", p.debugger.RemoveBreakpoint)
}

// cmdList lists all breakpoints.
func (p *DebugContext) cmdList(_ []string, out io.Writer) {
	bps := p.debugger.Breakpoints()
	if len(bps) == 0 {
		fmt.Fprintln(out, "No breakpoints set")
		return
	}

	// Sort by ID for consistent output
	slices.SortFunc(bps, func(a, b wile.BreakpointInfo) int {
		return cmp.Compare(a.ID, b.ID)
	})

	fmt.Fprintln(out, "Breakpoints:")
	for _, bp := range bps {
		status := "enabled"
		if !bp.Enabled {
			status = "disabled"
		}
		location := fmt.Sprintf("%s:%d", bp.File, bp.Line)
		if bp.Column > 0 {
			location = fmt.Sprintf("%s:%d", location, bp.Column)
		}
		fmt.Fprintf(out, "  %d: %s (%s, hits: %d)\n", bp.ID, location, status, bp.HitCount)
	}
}

// cmdEnable enables a breakpoint.
func (p *DebugContext) cmdEnable(args []string, out io.Writer) {
	p.breakpointAction(args, out, "enable", p.debugger.EnableBreakpoint)
}

// cmdDisable disables a breakpoint.
func (p *DebugContext) cmdDisable(args []string, out io.Writer) {
	p.breakpointAction(args, out, "disable", p.debugger.DisableBreakpoint)
}

// cmdStep steps into the next expression.
func (p *DebugContext) cmdStep(_ []string, out io.Writer) {
	p.debugger.StepInto()
	fmt.Fprintln(out, "Will step into next expression")
}

// cmdNext steps over (same frame).
func (p *DebugContext) cmdNext(_ []string, out io.Writer) {
	if p.debugger.CurrentState() == nil {
		fmt.Fprintln(out, "No active execution context")
		return
	}
	p.debugger.StepOver()
	fmt.Fprintln(out, "Will step over to next expression")
}

// cmdFinish steps out of current function.
func (p *DebugContext) cmdFinish(_ []string, out io.Writer) {
	if p.debugger.CurrentState() == nil {
		fmt.Fprintln(out, "No active execution context")
		return
	}
	p.debugger.StepOut()
	fmt.Fprintln(out, "Will step out of current function")
}

// cmdContinue resumes execution.
func (p *DebugContext) cmdContinue(_ []string, out io.Writer) {
	p.debugger.Continue()
	fmt.Fprintln(out, "Continuing execution")
}

// cmdBacktrace shows the current stack trace.
func (p *DebugContext) cmdBacktrace(_ []string, out io.Writer) {
	state := p.debugger.CurrentState()
	if state == nil {
		fmt.Fprintln(out, "No active execution context")
		return
	}

	trace := state.FormatStackTrace(20)
	if trace == "" {
		fmt.Fprintln(out, "Empty stack trace")
		return
	}

	// No header here: StackTrace.String already emits "Stack trace:". The
	// duplicate was invisible while this command could only ever read a
	// pool-zeroed context, whose trace was always empty.
	fmt.Fprint(out, trace)
}

// cmdWhere shows the current source location.
func (p *DebugContext) cmdWhere(_ []string, out io.Writer) {
	state := p.debugger.CurrentState()
	if state == nil {
		fmt.Fprintln(out, "No active execution context")
		return
	}

	loc := state.CurrentLocation()
	if loc == nil {
		fmt.Fprintln(out, "No source location available")
		return
	}

	fmt.Fprintf(out, "At %s:%d:%d\n", loc.File, loc.Line, loc.Column)
}

// cmdHelp shows available debug commands.
func (p *DebugContext) cmdHelp(_ []string, out io.Writer) {
	fmt.Fprintln(out, "Debug commands:")
	for _, dc := range p.DebugCommands() {
		aliases := formatAliases(dc.Aliases)
		fmt.Fprintf(out, "  ,%-12s %s%s\n", dc.Name, dc.Summary, aliases)
	}
}

// parseLocation parses a location string like "file.scm:10" or "file.scm:10:5".
func parseLocation(s string) (file string, line, column int) {
	parts := strings.Split(s, ":")
	if len(parts) < 2 {
		return "", 0, 0
	}

	file = parts[0]

	line, err := strconv.Atoi(parts[1])
	if err != nil {
		return "", 0, 0
	}

	if len(parts) >= 3 {
		column, _ = strconv.Atoi(parts[2]) // Column is optional, default 0
	}

	return file, line, column
}
