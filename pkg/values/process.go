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
	"context"
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
// Wait and WaitContext are the only supported ways to reap the child.
// exec.Cmd.Wait is not safe for concurrent use (two callers both read a nil
// c.ProcessState, both wait4 the same pid, and both write c.ProcessState), and
// this value is reachable from several goroutines at once by design: SRFI-18
// threads share one engine, so two of them can sit in (process-wait p), and
// Engine.Close reaps the same child from the host's goroutine. One reaper
// goroutine makes the single cmd.Wait and memoises its outcome; every waiter
// waits on that reaper, so a waiter can stop waiting without stopping the reap.
type Process struct {
	cmd     *exec.Cmd
	command string
	stdin   *PortObject
	stdout  *PortObject
	stderr  *PortObject

	// reapOnce starts the reaper on the first wait. The reaper stores waitErr,
	// then reaped, then closes done, so a waiter that sees done closed reads
	// waitErr race-free. reaped is the lock-free "has the reap completed?" query,
	// so a caller deciding whether a child still needs killing never blocks.
	reapOnce sync.Once
	done     chan struct{}
	waitErr  error
	reaped   atomic.Bool
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
		done:    make(chan struct{}),
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
// underlying exec.Cmd.Wait, without ctx cancellation: WaitContext with a
// background ctx, for callers with no ctx to thread (Engine.Close's reaper).
func (p *Process) Wait() error {
	_, err := p.WaitContext(context.Background())
	return err
}

// WaitContext waits for the process to exit and returns true with the result of
// the single underlying exec.Cmd.Wait. Concurrent callers share that one result,
// so a second (process-wait p) reports the child's exit status again rather than
// "Wait was already called".
//
// When ctx ends first, WaitContext returns (false, nil), and that is all this
// layer says: it follows Mutex.LockContext's (bool, error) cancel shape, not
// Thread.Join's raise, and process-wait reads the ctx and raises.
// The child is not signalled: cancelling a wait is not cancelling the process,
// which stays the caller's to wait for again or kill. The reaper keeps waiting,
// so its cmd.Wait still closes the stdout and stderr pipes when the child exits,
// as any completed wait does (exec.Cmd's "Wait before the reads finish" caveat).
// An exited child outranks an ended ctx, as in Thread.Join: the outcome exists,
// so it is reported.
//
// A Process with no command is a test fixture (NewProcess accepts a nil cmd) and
// reports success without waiting on anything.
func (p *Process) WaitContext(ctx context.Context) (bool, error) {
	if p.cmd == nil {
		return true, nil
	}
	p.reapOnce.Do(func() {
		go p.reap()
	})
	select {
	case <-p.done:
		return true, p.waitErr
	default:
	}
	select {
	case <-p.done:
		return true, p.waitErr
	case <-ctx.Done():
		return false, nil
	}
}

// reap makes the process's one cmd.Wait and publishes its outcome.
func (p *Process) reap() {
	p.waitErr = p.cmd.Wait()
	p.reaped.Store(true)
	close(p.done)
}

// Reaped reports whether the reap has already completed for this process. Callers
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
