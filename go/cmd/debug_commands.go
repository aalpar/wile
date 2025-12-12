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
	"fmt"
	"wile/machine"
	"sort"
	"strconv"
	"strings"
)

// DebugContext holds the state for debug commands
type DebugContext struct {
	debugger  *machine.Debugger
	currentMC *machine.MachineContext
}

// NewDebugContext creates a new debug context
func NewDebugContext() *DebugContext {
	return &DebugContext{
		debugger: machine.NewDebugger(),
	}
}

// Debugger returns the debugger instance
func (dc *DebugContext) Debugger() *machine.Debugger {
	return dc.debugger
}

// SetCurrentMC sets the current machine context (for inspection commands)
func (dc *DebugContext) SetCurrentMC(mc *machine.MachineContext) {
	dc.currentMC = mc
}

// HandleDebugCommand processes a debug command starting with ','
// Returns true if a command was handled, false otherwise
func (dc *DebugContext) HandleDebugCommand(line string) bool {
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
		dc.cmdBreak(args)
	case "delete", "d":
		dc.cmdDelete(args)
	case "list", "l":
		dc.cmdList()
	case "enable":
		dc.cmdEnable(args)
	case "disable":
		dc.cmdDisable(args)
	case "step", "s":
		dc.cmdStep()
	case "next", "n":
		dc.cmdNext()
	case "finish", "f":
		dc.cmdFinish()
	case "continue", "c":
		dc.cmdContinue()
	case "backtrace", "bt":
		dc.cmdBacktrace()
	case "where":
		dc.cmdWhere()
	case "help", "h", "?":
		dc.cmdHelp()
	default:
		fmt.Printf("Unknown command: %s (type ,help for commands)\n", cmd)
	}

	return true
}

// cmdBreak sets a breakpoint
func (dc *DebugContext) cmdBreak(args []string) {
	if len(args) < 1 {
		fmt.Println("Usage: ,break FILE:LINE[:COLUMN]")
		return
	}

	file, line, col := parseLocation(args[0])
	if file == "" || line == 0 {
		fmt.Println("Invalid location format. Use FILE:LINE or FILE:LINE:COLUMN")
		return
	}

	id := dc.debugger.SetBreakpoint(file, line, col)
	fmt.Printf("Breakpoint %d set at %s:%d", id, file, line)
	if col > 0 {
		fmt.Printf(":%d", col)
	}
	fmt.Println()
}

// cmdDelete removes a breakpoint
func (dc *DebugContext) cmdDelete(args []string) {
	if len(args) < 1 {
		fmt.Println("Usage: ,delete ID")
		return
	}

	id, err := strconv.Atoi(args[0])
	if err != nil {
		fmt.Printf("Invalid breakpoint ID: %s\n", args[0])
		return
	}

	if dc.debugger.RemoveBreakpoint(machine.BreakpointID(id)) {
		fmt.Printf("Breakpoint %d deleted\n", id)
	} else {
		fmt.Printf("Breakpoint %d not found\n", id)
	}
}

// cmdList lists all breakpoints
func (dc *DebugContext) cmdList() {
	bps := dc.debugger.Breakpoints()
	if len(bps) == 0 {
		fmt.Println("No breakpoints set")
		return
	}

	// Sort by ID for consistent output
	sort.Slice(bps, func(i, j int) bool {
		return bps[i].ID < bps[j].ID
	})

	fmt.Println("Breakpoints:")
	for _, bp := range bps {
		status := "enabled"
		if !bp.Enabled {
			status = "disabled"
		}
		location := fmt.Sprintf("%s:%d", bp.File, bp.Line)
		if bp.Column > 0 {
			location = fmt.Sprintf("%s:%d", location, bp.Column)
		}
		fmt.Printf("  %d: %s (%s, hits: %d)\n", bp.ID, location, status, bp.HitCount)
	}
}

// cmdEnable enables a breakpoint
func (dc *DebugContext) cmdEnable(args []string) {
	if len(args) < 1 {
		fmt.Println("Usage: ,enable ID")
		return
	}

	id, err := strconv.Atoi(args[0])
	if err != nil {
		fmt.Printf("Invalid breakpoint ID: %s\n", args[0])
		return
	}

	if dc.debugger.EnableBreakpoint(machine.BreakpointID(id)) {
		fmt.Printf("Breakpoint %d enabled\n", id)
	} else {
		fmt.Printf("Breakpoint %d not found\n", id)
	}
}

// cmdDisable disables a breakpoint
func (dc *DebugContext) cmdDisable(args []string) {
	if len(args) < 1 {
		fmt.Println("Usage: ,disable ID")
		return
	}

	id, err := strconv.Atoi(args[0])
	if err != nil {
		fmt.Printf("Invalid breakpoint ID: %s\n", args[0])
		return
	}

	if dc.debugger.DisableBreakpoint(machine.BreakpointID(id)) {
		fmt.Printf("Breakpoint %d disabled\n", id)
	} else {
		fmt.Printf("Breakpoint %d not found\n", id)
	}
}

// cmdStep steps into the next expression
func (dc *DebugContext) cmdStep() {
	dc.debugger.StepInto()
	fmt.Println("Will step into next expression")
}

// cmdNext steps over (same frame)
func (dc *DebugContext) cmdNext() {
	if dc.currentMC == nil {
		fmt.Println("No active execution context")
		return
	}
	dc.debugger.StepOver(dc.currentMC)
	fmt.Println("Will step over to next expression")
}

// cmdFinish steps out of current function
func (dc *DebugContext) cmdFinish() {
	if dc.currentMC == nil {
		fmt.Println("No active execution context")
		return
	}
	dc.debugger.StepOut(dc.currentMC)
	fmt.Println("Will step out of current function")
}

// cmdContinue resumes execution
func (dc *DebugContext) cmdContinue() {
	dc.debugger.Continue()
	fmt.Println("Continuing execution")
}

// cmdBacktrace shows the current stack trace
func (dc *DebugContext) cmdBacktrace() {
	if dc.currentMC == nil {
		fmt.Println("No active execution context")
		return
	}

	trace := dc.currentMC.CaptureStackTrace(20)
	if len(trace) == 0 {
		fmt.Println("Empty stack trace")
		return
	}

	fmt.Println("Stack trace:")
	fmt.Print(trace.String())
}

// cmdWhere shows the current source location
func (dc *DebugContext) cmdWhere() {
	if dc.currentMC == nil {
		fmt.Println("No active execution context")
		return
	}

	source := dc.currentMC.CurrentSource()
	if source == nil {
		fmt.Println("No source location available")
		return
	}

	fmt.Printf("At %s:%d:%d\n", source.File, source.Start.Line(), source.Start.Column())
}

// cmdHelp shows available commands
func (dc *DebugContext) cmdHelp() {
	fmt.Println(`Debug commands:
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

// parseLocation parses a location string like "file.scm:10" or "file.scm:10:5"
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
