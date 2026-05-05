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
	"os/exec"
	"syscall"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

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
	tail, iterErr := values.ForEach(ctx, rest, func(_ context.Context, _ int, _ bool, v values.Value) error {
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
	if !values.IsEmptyList(tail) {
		return werr.WrapForeignErrorf(
			werr.ErrNotAList,
			"process-spawn: arguments must be a proper list",
		)
	}

	cmd := exec.CommandContext(ctx, command.Value, args...)

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

	proc := values.NewProcess(
		command.Value,
		cmd,
		values.NewCharacterOutputPortFromWriter(stdinPipe),
		values.NewCharacterInputPortFromReader(stdoutPipe),
		values.NewCharacterInputPortFromReader(stderrPipe),
	)
	mc.SetValue(proc)
	return nil
}

// PrimProcessStdout implements the (process-stdout) primitive.
func PrimProcessStdout(mc machine.CallContext) error {
	proc, err := helpers.RequireArg[*values.Process](mc, 0, werr.ErrNotAProcess, "process-stdout")
	if err != nil {
		return err
	}
	mc.SetValue(proc.Stdout())
	return nil
}

// PrimProcessStderr implements the (process-stderr) primitive.
func PrimProcessStderr(mc machine.CallContext) error {
	proc, err := helpers.RequireArg[*values.Process](mc, 0, werr.ErrNotAProcess, "process-stderr")
	if err != nil {
		return err
	}
	mc.SetValue(proc.Stderr())
	return nil
}

// PrimProcessStdin implements the (process-stdin) primitive.
func PrimProcessStdin(mc machine.CallContext) error {
	proc, err := helpers.RequireArg[*values.Process](mc, 0, werr.ErrNotAProcess, "process-stdin")
	if err != nil {
		return err
	}
	mc.SetValue(proc.Stdin())
	return nil
}

// PrimProcessWait implements the (process-wait) primitive.
// Blocks until the process exits and returns the exit code.
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
	waitErr := cmd.Wait()
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
