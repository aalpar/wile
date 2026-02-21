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
	"strings"
	"time"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
)

// ProgramStartTime is used for current-jiffy to measure elapsed time.
var ProgramStartTime = time.Now()

// PrimCommandLine implements the (command-line) primitive.
// Returns a list of command line arguments.
func PrimCommandLine(mc *machine.MachineContext) error {
	args := os.Args
	list := values.EmptyList
	for i := len(args) - 1; i >= 0; i-- {
		list = values.NewCons(values.NewString(args[i]), list)
	}
	mc.SetValue(list)
	return nil
}

// PrimExit implements the (exit) primitive.
// Exits the program with an optional status code.
func PrimExit(mc *machine.MachineContext) error {
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
			}
		}
	}
	os.Exit(code)
	return nil
}

// PrimEmergencyExit implements the (emergency-exit) primitive.
// Exits the program immediately without cleanup or finalization.
func PrimEmergencyExit(mc *machine.MachineContext) error {
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
			}
		}
	}
	os.Exit(code)
	return nil
}

// PrimGetEnvironmentVariable implements the (get-environment-variable) primitive.
// Gets environment variable value.
func PrimGetEnvironmentVariable(mc *machine.MachineContext) error {
	name, err := helpers.RequireArg[*values.String](mc, 0, values.ErrNotAString, "get-environment-variable")
	if err != nil {
		return err
	}
	val, exists := os.LookupEnv(name.Value)
	if exists {
		mc.SetValue(values.NewString(val))
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimGetEnvironmentVariables implements the (get-environment-variables) primitive.
// Returns all environment variables.
func PrimGetEnvironmentVariables(mc *machine.MachineContext) error {
	env := os.Environ()
	list := values.EmptyList
	for i := len(env) - 1; i >= 0; i-- {
		parts := strings.SplitN(env[i], "=", 2)
		if len(parts) == 2 {
			pair := values.NewCons(values.NewString(parts[0]), values.NewString(parts[1]))
			list = values.NewCons(pair, list)
		}
	}
	mc.SetValue(list)
	return nil
}

// PrimCurrentSecond implements the (current-second) primitive.
// Returns current time in seconds since Unix epoch.
func PrimCurrentSecond(mc *machine.MachineContext) error {
	now := time.Now()
	secs := float64(now.Unix()) + float64(now.Nanosecond())/1e9
	mc.SetValue(values.NewFloat(secs))
	return nil
}

// PrimCurrentJiffy implements the (current-jiffy) primitive.
// Returns current time in jiffies since program start.
func PrimCurrentJiffy(mc *machine.MachineContext) error {
	elapsed := time.Since(ProgramStartTime)
	jiffies := elapsed.Nanoseconds()
	mc.SetValue(values.NewInteger(jiffies))
	return nil
}

// PrimJiffiesPerSecond implements the (jiffies-per-second) primitive.
// Returns the number of jiffies per second (1 billion nanoseconds).
func PrimJiffiesPerSecond(mc *machine.MachineContext) error {
	mc.SetValue(values.NewInteger(1000000000)) // 1 billion nanoseconds per second
	return nil
}

// PrimFeatures implements the (features) primitive.
// Returns list of implementation features.
func PrimFeatures(mc *machine.MachineContext) error {
	features := machine.AllFeatures()

	// Build a list of symbols
	result := values.EmptyList
	// Build the list in reverse order to get correct ordering
	for i := len(features) - 1; i >= 0; i-- {
		sym := mc.EnvironmentFrame().InternSymbol(values.NewSymbol(features[i]))
		result = values.NewCons(sym, result)
	}

	mc.SetValue(result)
	return nil
}
