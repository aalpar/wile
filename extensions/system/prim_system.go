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
	"slices"
	"time"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
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
func PrimCommandLine(mc machine.CallContext) error {
	args := commandLineArgs
	if args == nil {
		args = os.Args
	}
	list := values.EmptyList
	for i := range slices.Backward(args) {
		list = values.NewCons(values.NewString(args[i]), list)
	}
	mc.SetValue(list)
	return nil
}

// exitWithCode implements the shared logic for exit and emergency-exit.
// Both parse an optional status argument (#f → 1, integer → value, default → 0)
// and call os.Exit. Currently identical; the distinction exists for R7RS
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
				// R7RS §6.14 permits implementation-defined translation of a
				// non-integer, non-boolean status. Rather than silently coerce
				// it to 0 (masking a programming error under a success exit),
				// reject it so the mistake surfaces as a catchable error.
				return werr.WrapForeignErrorf(werr.ErrNotANumber,
					"exit: status must be an integer or boolean")
			}
		}
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
