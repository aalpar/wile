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

package values

import (
	"fmt"
	"os/exec"
)

var _ Value = (*Process)(nil)

// Process represents a running OS process.
// Wraps *exec.Cmd and its connected pipes. Accessors return
// the ports for stdout, stderr, and stdin.
type Process struct {
	cmd     *exec.Cmd
	command string
	stdin   *PortObject
	stdout  *PortObject
	stderr  *PortObject
}

// NewProcess creates a Process value. The cmd may be nil for testing.
// Ports may be nil if the process was not started with pipes.
func NewProcess(
	command string,
	cmd *exec.Cmd,
	stdin *PortObject,
	stdout *PortObject,
	stderr *PortObject,
) *Process {
	return &Process{
		cmd:     cmd,
		command: command,
		stdin:   stdin,
		stdout:  stdout,
		stderr:  stderr,
	}
}

// Command returns the command name.
func (p *Process) Command() string {
	return p.command
}

// Cmd returns the underlying *exec.Cmd.
func (p *Process) Cmd() *exec.Cmd {
	return p.cmd
}

// Stdin returns the output port connected to the process stdin.
func (p *Process) Stdin() *PortObject {
	return p.stdin
}

// Stdout returns the input port connected to the process stdout.
func (p *Process) Stdout() *PortObject {
	return p.stdout
}

// Stderr returns the input port connected to the process stderr.
func (p *Process) Stderr() *PortObject {
	return p.stderr
}

// SchemeString returns the Scheme external representation.
func (p *Process) SchemeString() string {
	if p.cmd != nil && p.cmd.Process != nil {
		return fmt.Sprintf(`#<process %q pid=%d>`, p.command, p.cmd.Process.Pid)
	}
	return fmt.Sprintf(`#<process %q>`, p.command)
}

// IsVoid reports whether this process value is void.
// A nil *Process is considered void to satisfy the values.Value contract.
func (p *Process) IsVoid() bool {
	return p == nil
}

// EqualTo returns true only for identity (same pointer).
func (p *Process) EqualTo(v Value) bool {
	other, ok := v.(*Process)
	return ok && p == other
}
