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

package process

import (
	"context"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"syscall"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// startDirectory returns the directory a spawned child must start in, having
// gated it as file:exec — POSIX x on a directory is traverse, and the child
// enters this one. The caller assigns the result to cmd.Dir; gate and assignment
// are two halves of one operation, so nothing runs in a directory the policy did
// not see.
//
// Under a root-confining authorizer the child starts at the CONFINEMENT ROOT,
// not the host's working directory. cmd.Dir defaults to the caller's cwd, so a
// sandboxed engine whose host happens to run outside its own root would
// otherwise hand the child a starting point the policy could never have allowed
// — and every relative path the child opened would resolve there. Pinning makes
// the sandbox effective rather than merely obstructive.
//
// With no confining authorizer the target is the cwd the child would have
// inherited anyway, so assigning it changes nothing and keeps the gated string
// identical to the directory that is actually used.
func startDirectory(mc machine.CallContext, op string) (string, error) {
	auth := mc.Authorizer()
	q, ok := security.ConfinementRootOf(auth)
	if !ok {
		wd, err := os.Getwd()
		if err != nil {
			return "", werr.WrapForeignProcessError(err, op, ".")
		}
		q = wd
	}
	err := security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionExec,
		Target:   q,
	})
	if err != nil {
		return "", err
	}
	return q, nil
}

// gateExecutable gates, as file:exec, the binary cmd will actually run.
//
// The target is what the OS executes, not what the program named. exec.Command
// LookPath-resolves a bare name into cmd.Path, and os/exec resolves a RELATIVE
// cmd.Path against cmd.Dir — so gating the caller's own string would authorize
// one file and run another. Call this after cmd.Dir is set.
func gateExecutable(mc machine.CallContext, cmd *exec.Cmd) error {
	target := cmd.Path
	if !filepath.IsAbs(target) {
		target = filepath.Join(cmd.Dir, target)
	}
	return security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionExec,
		Target:   target,
	})
}

// PrimSystem implements the (system) primitive.
// Runs a shell command via /bin/sh -c and returns the exit code.
func PrimSystem(mc machine.CallContext) error {
	command, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "system")
	if err != nil {
		return err
	}
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceProcess,
		Action:   security.ActionExecShell,
		Target:   command.Value,
	})
	if err != nil {
		return err
	}
	cmd := exec.CommandContext(mc.Context(), "/bin/sh", "-c", command.Value)
	// The shell is a host binary this primitive names on the program's behalf.
	// process:exec-shell above answers "may it run a shell at all"; these two
	// answer "this shell, from this directory" — the questions a path-confining
	// authorizer can act on, and the reason (system …) can no longer reach a
	// /bin/sh that lies outside a confinement root.
	dir, err := startDirectory(mc, "system")
	if err != nil {
		return err
	}
	cmd.Dir = dir
	err = gateExecutable(mc, cmd)
	if err != nil {
		return err
	}
	runErr := cmd.Run()
	if runErr != nil {
		var exitErr *exec.ExitError
		if errors.As(runErr, &exitErr) {
			mc.SetValue(values.NewInteger(int64(exitErr.ExitCode())))
			return nil
		}
		return werr.WrapForeignProcessError(runErr, "system", command.Value)
	}
	mc.SetValue(values.NewInteger(0))
	return nil
}

// PrimProcessSpawn implements the (process-spawn) primitive.
// Creates a subprocess with stdin/stdout/stderr pipes.
//
// A spawned child is recorded against the calling engine's namespace so
// Engine.Close can kill and reap it (close.go). A spawn that failed is not
// recorded.
func PrimProcessSpawn(mc machine.CallContext) error {
	command, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "process-spawn")
	if err != nil {
		return err
	}
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceProcess,
		Action:   security.ActionExec,
		Target:   command.Value,
	})
	if err != nil {
		return err
	}

	// Collect string arguments from the rest list.
	var args []string
	rest := mc.Arg(1)
	ctx := mc.Context()
	restTuple, ok := rest.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(
			werr.ErrNotAList,
			"process-spawn: arguments must be a proper list, got %T", rest,
		)
	}
	iterErr := helpers.ForEachList(ctx, restTuple, "process-spawn", func(_ context.Context, _ int, _ bool, v values.Value) error {
		s, ok := v.(*values.String)
		if !ok {
			return werr.WrapForeignErrorf(
				werr.ErrNotAString,
				"process-spawn: argument is not a string: %T", v,
			)
		}
		args = append(args, s.Value)
		return nil
	})
	if iterErr != nil {
		return iterErr
	}

	cmd := exec.CommandContext(ctx, command.Value, args...)
	// cmd.Err carries a LookPath failure (including exec.ErrDot). Report it here
	// rather than letting Start() surface it, so the file:exec gate below is never
	// asked about an empty or unresolved path. process:exec has already passed, so
	// this leaks nothing a permitted caller could not learn by spawning.
	if cmd.Err != nil {
		return werr.WrapForeignProcessError(cmd.Err, "process-spawn", command.Value)
	}
	dir, err := startDirectory(mc, "process-spawn")
	if err != nil {
		return err
	}
	cmd.Dir = dir
	err = gateExecutable(mc, cmd)
	if err != nil {
		return err
	}

	stdinPipe, err := cmd.StdinPipe()
	if err != nil {
		return werr.WrapForeignProcessError(err, "process-spawn", command.Value)
	}
	stdoutPipe, err := cmd.StdoutPipe()
	if err != nil {
		stdinPipe.Close() //nolint:errcheck
		return werr.WrapForeignProcessError(err, "process-spawn", command.Value)
	}
	stderrPipe, err := cmd.StderrPipe()
	if err != nil {
		stdinPipe.Close()  //nolint:errcheck
		stdoutPipe.Close() //nolint:errcheck
		return werr.WrapForeignProcessError(err, "process-spawn", command.Value)
	}

	err = cmd.Start()
	if err != nil {
		stdinPipe.Close()  //nolint:errcheck
		stdoutPipe.Close() //nolint:errcheck
		stderrPipe.Close() //nolint:errcheck
		return werr.WrapForeignProcessError(err, "process-spawn", command.Value)
	}

	q := values.NewProcess(
		command.Value,
		cmd,
		values.NewCharacterOutputPortFromWriter(stdinPipe),
		values.NewCharacterInputPortFromReader(stdoutPipe),
		values.NewCharacterInputPortFromReader(stderrPipe),
	)
	trackSpawnedProcess(mc, q)
	mc.SetValue(q)
	return nil
}

// PrimProcessStdout implements the (process-stdout) primitive.
var PrimProcessStdout = helpers.MakeUnaryAccessor(werr.ErrNotAProcess, "process-stdout", func(proc *values.Process) values.Value {
	return proc.Stdout()
})

// PrimProcessStderr implements the (process-stderr) primitive.
var PrimProcessStderr = helpers.MakeUnaryAccessor(werr.ErrNotAProcess, "process-stderr", func(proc *values.Process) values.Value {
	return proc.Stderr()
})

// PrimProcessStdin implements the (process-stdin) primitive.
var PrimProcessStdin = helpers.MakeUnaryAccessor(werr.ErrNotAProcess, "process-stdin", func(proc *values.Process) values.Value {
	return proc.Stdin()
})

// PrimProcessWait implements the (process-wait) primitive.
// Blocks until the process exits and returns the exit code.
//
// The wait goes through values.Process.Wait, not cmd.Wait: two SRFI-18 threads
// in (process-wait p), or one of them racing Engine.Close's reaper, would
// otherwise wait4 the same pid twice and tear cmd.ProcessState between them.
func PrimProcessWait(mc machine.CallContext) error {
	proc, err := helpers.RequireArg[*values.Process](mc, 0, werr.ErrNotAProcess, "process-wait")
	if err != nil {
		return err
	}
	cmd := proc.Cmd()
	if cmd == nil {
		return werr.WrapForeignErrorf(
			werr.ErrInvalidArgument,
			"process-wait: process has no underlying command",
		)
	}
	waitErr := proc.Wait()
	if waitErr != nil {
		var exitErr *exec.ExitError
		if errors.As(waitErr, &exitErr) {
			mc.SetValue(values.NewInteger(int64(exitErr.ExitCode())))
			return nil
		}
		return werr.WrapForeignProcessError(waitErr, "process-wait", proc.Command())
	}
	mc.SetValue(values.NewInteger(0))
	return nil
}

// signalMap maps Scheme signal symbols to OS signals.
var signalMap = map[string]syscall.Signal{
	"term": syscall.SIGTERM,
	"kill": syscall.SIGKILL,
	"int":  syscall.SIGINT,
	"hup":  syscall.SIGHUP,
}

// PrimProcessKill implements the (process-kill) primitive.
// Sends a signal to the process. Signal is a symbol: term, kill, int, hup.
func PrimProcessKill(mc machine.CallContext) error {
	proc, err := helpers.RequireArg[*values.Process](mc, 0, werr.ErrNotAProcess, "process-kill")
	if err != nil {
		return err
	}
	sigSym, err := helpers.RequireArg[*values.Symbol](mc, 1, werr.ErrNotASymbol, "process-kill")
	if err != nil {
		return err
	}
	sig, ok := signalMap[sigSym.Key]
	if !ok {
		return werr.WrapForeignErrorf(
			werr.ErrInvalidArgument,
			"process-kill: unknown signal '%s' (expected term, kill, int, or hup)",
			sigSym.Key,
		)
	}
	cmd := proc.Cmd()
	if cmd == nil || cmd.Process == nil {
		return werr.WrapForeignErrorf(
			werr.ErrInvalidArgument,
			"process-kill: process is not running",
		)
	}
	killErr := cmd.Process.Signal(sig)
	if killErr != nil {
		return werr.WrapForeignProcessError(killErr, "process-kill", proc.Command())
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimProcessQ implements the (process?) predicate.
func PrimProcessQ(mc machine.CallContext) error {
	_, ok := mc.Arg(0).(*values.Process)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}
