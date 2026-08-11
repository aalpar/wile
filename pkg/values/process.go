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
	"sync"
	"sync/atomic"
)

var _ Value = (*Process)(nil)

// Process represents a running OS process.
// Wraps *exec.Cmd and its connected pipes. Accessors return
// the ports for stdout, stderr, and stdin.
//
// Wait is the only supported way to reap the child. exec.Cmd.Wait is not safe
// for concurrent use — two callers both read a nil c.ProcessState, both wait4
// the same pid, and both write c.ProcessState — and this value is reachable from
// several goroutines at once by design: SRFI-18 threads share one engine, so two
// of them can sit in (process-wait p), and Engine.Close reaps the same child from
// the host's goroutine. Wait serialises them and memoises the outcome.
type Process struct {
	cmd     *exec.Cmd
	command string
	stdin   *PortObject
	stdout  *PortObject
	stderr  *PortObject

	// waitMu admits one cmd.Wait at a time; waitErr is its memoised result,
	// guarded by waitMu. reaped is the lock-free "has a Wait completed?" query,
	// so a caller deciding whether a child still needs killing never blocks
	// behind a caller that is inside Wait.
	waitMu  sync.Mutex
	waitErr error
	reaped  atomic.Bool
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

// Wait waits for the process to exit and returns the result of the single
// underlying exec.Cmd.Wait. Concurrent callers serialise; every one of them gets
// that one result, so a second (process-wait p) reports the child's exit status
// again rather than "Wait was already called".
//
// A Process with no command is a test fixture (NewProcess accepts a nil cmd) and
// reports success without waiting on anything.
func (p *Process) Wait() error {
	if p.cmd == nil {
		return nil
	}
	p.waitMu.Lock()
	defer p.waitMu.Unlock()
	if p.reaped.Load() {
		return p.waitErr
	}
	p.waitErr = p.cmd.Wait()
	p.reaped.Store(true)
	return p.waitErr
}

// Reaped reports whether a Wait has already completed for this process. Callers
// use it in place of reading cmd.ProcessState, which races with a concurrent
// Wait's write of that field.
func (p *Process) Reaped() bool {
	return p.reaped.Load()
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
