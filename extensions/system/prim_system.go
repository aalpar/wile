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

package system

import (
	"os"
	"time"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/values"
)

// ProgramStartTime is used for current-jiffy to measure elapsed time.
var ProgramStartTime = time.Now()

// commandLineArgs holds the script-relative command-line arguments set by the CLI.
// When set, PrimCommandLine returns these instead of os.Args.
var commandLineArgs []string

// SetCommandLine sets the command-line arguments returned by (command-line).
// The first element should be the script name, followed by script arguments.
func SetCommandLine(args []string) {
	commandLineArgs = args
}

// PrimCommandLine implements the (command-line) primitive per R7RS §6.14.
// Returns a list whose first element is the script name and the rest are
// script arguments. Falls back to os.Args when no script is being executed.
// Reading the host argv is gated as process:read, symmetric with the
// env:read gate on get-environment-variable: an embedder that installs a
// denying authorizer does not leak os.Args.
func PrimCommandLine(mc machine.CallContext) error {
	err := security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceProcess,
		Action:   security.ActionRead,
	})
	if err != nil {
		return err
	}
	args := commandLineArgs
	if args == nil {
		args = os.Args
	}
	strs := make([]values.Value, len(args))
	for i, arg := range args {
		strs[i] = values.NewString(arg)
	}
	mc.SetValue(values.List(strs...))
	return nil
}

// exitWithCode implements the shared logic for exit and emergency-exit.
// Both first gate on process:exit via security.CheckWithAuthorizer, so a denied
// call returns that error instead of exiting. Both then parse an optional status
// argument (#f → 1, integer → value, default → 0), run the SetExitHook function
// if one is registered, and call os.Exit. The hook is the host's, not the
// program's, so it is not the cleanup R7RS lets emergency-exit skip (outstanding
// dynamic-wind after thunks); both primitives run it.
// Currently identical; the distinction exists for R7RS
// compliance (emergency-exit should skip cleanup, which is not yet implemented).
func exitWithCode(mc machine.CallContext) error {
	err := security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceProcess,
		Action:   security.ActionExit,
	})
	if err != nil {
		return err
	}
	rest := mc.Arg(0)
	code := 0
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(values.Tuple)
		if ok && !pr.IsEmptyList() {
			switch v := pr.Car().(type) {
			case *values.Integer:
				code = int(v.Value)
			case *values.Boolean:
				if !v.Value {
					code = 1
				}
			default:
				// R7RS §6.14: "exit should translate obj into an appropriate
				// exit value for the operating system, if possible" and "must
				// not signal an exception or return to its continuation." So a
				// non-#t/#f, non-integer status must NOT raise. Translate it to
				// a failure code (1) rather than the old silent 0, so that
				// e.g. (exit "some error") no longer terminates as success.
				code = 1
			}
		}
	}
	if exitHook != nil {
		exitHook()
	}
	os.Exit(code)
	return nil
}

// PrimExit implements the (exit) primitive.
// Exits the program with an optional status code.
func PrimExit(mc machine.CallContext) error {
	return exitWithCode(mc)
}

// PrimEmergencyExit implements the (emergency-exit) primitive.
// Exits the program immediately without cleanup or finalization.
func PrimEmergencyExit(mc machine.CallContext) error {
	return exitWithCode(mc)
}

// PrimCurrentSecond implements the (current-second) primitive.
// Returns current time in seconds since Unix epoch.
func PrimCurrentSecond(mc machine.CallContext) error {
	now := time.Now()
	secs := float64(now.Unix()) + float64(now.Nanosecond())/1e9
	mc.SetValue(values.NewFloat(secs))
	return nil
}

// PrimCurrentJiffy implements the (current-jiffy) primitive.
// Returns current time in jiffies since program start.
func PrimCurrentJiffy(mc machine.CallContext) error {
	elapsed := time.Since(ProgramStartTime)
	jiffies := elapsed.Nanoseconds()
	mc.SetValue(values.NewInteger(jiffies))
	return nil
}

// PrimJiffiesPerSecond implements the (jiffies-per-second) primitive.
// Returns the number of jiffies per second (1 billion nanoseconds).
func PrimJiffiesPerSecond(mc machine.CallContext) error {
	mc.SetValue(values.NewInteger(1000000000)) // 1 billion nanoseconds per second
	return nil
}

// exitHook is the function SetExitHook registered, or nil.
var exitHook func()

// SetExitHook registers fn to run before the process terminates through exit or
// emergency-exit. os.Exit skips deferred functions, so a host that writes
// something at the end of a run (the CLI's coverage report and profiles) has no
// other way to reach the end of a program that exits. The hook is process-wide,
// like os.Exit itself and like SetCommandLine: it belongs to the host, not to
// an engine. It runs on the calling goroutine, which for an SRFI-18 thread is
// not the main one. Passing nil removes the hook.
func SetExitHook(fn func()) {
	exitHook = fn
}
