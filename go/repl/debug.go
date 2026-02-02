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
	"fmt"
	"io"
	"sort"
	"strconv"
	"strings"

	"github.com/aalpar/wile/go/machine"
)

// DebugContext holds the state for debug commands.
type DebugContext struct {
	debugger  *machine.Debugger
	currentMC *machine.MachineContext
}

// NewDebugContext creates a new debug context.
func NewDebugContext() *DebugContext {
	return &DebugContext{
		debugger: machine.NewDebugger(),
	}
}

// Debugger returns the debugger instance.
func (p *DebugContext) Debugger() *machine.Debugger {
	return p.debugger
}

// SetCurrentMC sets the current machine context (for inspection commands).
func (p *DebugContext) SetCurrentMC(mc *machine.MachineContext) {
	p.currentMC = mc
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

	switch cmd {
	case "break", "b":
		p.cmdBreak(args, out)
	case "delete", "d":
		p.cmdDelete(args, out)
	case "list", "l":
		p.cmdList(out)
	case "enable":
		p.cmdEnable(args, out)
	case "disable":
		p.cmdDisable(args, out)
	case "step", "s":
		p.cmdStep(out)
	case "next", "n":
		p.cmdNext(out)
	case "finish", "f":
		p.cmdFinish(out)
	case "continue", "c":
		p.cmdContinue(out)
	case "backtrace", "bt":
		p.cmdBacktrace(out)
	case "where":
		p.cmdWhere(out)
	case "help", "h", "?":
		p.cmdHelp(out)
	default:
		fmt.Fprintf(out, "Unknown command: %s (type ,help for commands)\n", cmd)
	}

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

// cmdDelete removes a breakpoint.
func (p *DebugContext) cmdDelete(args []string, out io.Writer) {
	if len(args) < 1 {
		fmt.Fprintln(out, "Usage: ,delete ID")
		return
	}

	id, err := strconv.Atoi(args[0])
	if err != nil {
		fmt.Fprintf(out, "Invalid breakpoint ID: %s\n", args[0])
		return
	}

	if p.debugger.RemoveBreakpoint(machine.BreakpointID(id)) {
		fmt.Fprintf(out, "Breakpoint %d deleted\n", id)
	} else {
		fmt.Fprintf(out, "Breakpoint %d not found\n", id)
	}
}

// cmdList lists all breakpoints.
func (p *DebugContext) cmdList(out io.Writer) {
	bps := p.debugger.Breakpoints()
	if len(bps) == 0 {
		fmt.Fprintln(out, "No breakpoints set")
		return
	}

	// Sort by ID for consistent output
	sort.Slice(bps, func(i, j int) bool {
		return bps[i].ID < bps[j].ID
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
	if len(args) < 1 {
		fmt.Fprintln(out, "Usage: ,enable ID")
		return
	}

	id, err := strconv.Atoi(args[0])
	if err != nil {
		fmt.Fprintf(out, "Invalid breakpoint ID: %s\n", args[0])
		return
	}

	if p.debugger.EnableBreakpoint(machine.BreakpointID(id)) {
		fmt.Fprintf(out, "Breakpoint %d enabled\n", id)
	} else {
		fmt.Fprintf(out, "Breakpoint %d not found\n", id)
	}
}

// cmdDisable disables a breakpoint.
func (p *DebugContext) cmdDisable(args []string, out io.Writer) {
	if len(args) < 1 {
		fmt.Fprintln(out, "Usage: ,disable ID")
		return
	}

	id, err := strconv.Atoi(args[0])
	if err != nil {
		fmt.Fprintf(out, "Invalid breakpoint ID: %s\n", args[0])
		return
	}

	if p.debugger.DisableBreakpoint(machine.BreakpointID(id)) {
		fmt.Fprintf(out, "Breakpoint %d disabled\n", id)
	} else {
		fmt.Fprintf(out, "Breakpoint %d not found\n", id)
	}
}

// cmdStep steps into the next expression.
func (p *DebugContext) cmdStep(out io.Writer) {
	p.debugger.StepInto()
	fmt.Fprintln(out, "Will step into next expression")
}

// cmdNext steps over (same frame).
func (p *DebugContext) cmdNext(out io.Writer) {
	if p.currentMC == nil {
		fmt.Fprintln(out, "No active execution context")
		return
	}
	p.debugger.StepOver(p.currentMC)
	fmt.Fprintln(out, "Will step over to next expression")
}

// cmdFinish steps out of current function.
func (p *DebugContext) cmdFinish(out io.Writer) {
	if p.currentMC == nil {
		fmt.Fprintln(out, "No active execution context")
		return
	}
	p.debugger.StepOut(p.currentMC)
	fmt.Fprintln(out, "Will step out of current function")
}

// cmdContinue resumes execution.
func (p *DebugContext) cmdContinue(out io.Writer) {
	p.debugger.Continue()
	fmt.Fprintln(out, "Continuing execution")
}

// cmdBacktrace shows the current stack trace.
func (p *DebugContext) cmdBacktrace(out io.Writer) {
	if p.currentMC == nil {
		fmt.Fprintln(out, "No active execution context")
		return
	}

	trace := p.currentMC.CaptureStackTrace(20)
	if len(trace) == 0 {
		fmt.Fprintln(out, "Empty stack trace")
		return
	}

	fmt.Fprintln(out, "Stack trace:")
	fmt.Fprint(out, trace.String())
}

// cmdWhere shows the current source location.
func (p *DebugContext) cmdWhere(out io.Writer) {
	if p.currentMC == nil {
		fmt.Fprintln(out, "No active execution context")
		return
	}

	source := p.currentMC.CurrentSource()
	if source == nil {
		fmt.Fprintln(out, "No source location available")
		return
	}

	fmt.Fprintf(out, "At %s:%d:%d\n", source.File, source.Start.Line(), source.Start.Column())
}

// cmdHelp shows available commands.
func (p *DebugContext) cmdHelp(out io.Writer) {
	fmt.Fprintln(out, `Debug commands:
  ,break FILE:LINE[:COL]  Set breakpoint (aliases: ,b)
  ,delete ID              Delete breakpoint (aliases: ,d)
  ,list                   List breakpoints (aliases: ,l)
  ,enable ID              Enable breakpoint
  ,disable ID             Disable breakpoint
  ,step                   Step into next expression (aliases: ,s)
  ,next                   Step over (same frame) (aliases: ,n)
  ,finish                 Step out (return from function) (aliases: ,f)
  ,continue               Continue execution (aliases: ,c)
  ,backtrace              Show stack trace (aliases: ,bt)
  ,where                  Show current location
  ,help                   Show this help (aliases: ,h, ,?)`)
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
