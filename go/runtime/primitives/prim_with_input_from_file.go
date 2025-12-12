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

package primitives

import (
	"context"
	"errors"
	"os"

	"wile/machine"
	"wile/values"
)

// PrimWithInputFromFile implements the with-input-from-file primitive (R7RS).
// Opens a file for reading, temporarily sets it as current-input-port,
// calls the thunk, then restores the previous port and closes the file.
// (with-input-from-file string thunk)
func PrimWithInputFromFile(ctx context.Context, mc *machine.MachineContext) error {
	filenameVal := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	thunkVal := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	filename, ok := filenameVal.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "with-input-from-file: expected a string but got %T", filenameVal)
	}

	thunk, ok := thunkVal.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "with-input-from-file: expected a procedure but got %T", thunkVal)
	}

	// Open the file
	file, err := os.Open(filename.Value)
	if err != nil {
		return values.WrapForeignErrorf(err, "with-input-from-file: %v", err)
	}
	defer file.Close()

	// Save current port and set new one
	savedPort := GetCurrentInputPort()
	newPort := values.NewCharacterInputPortFromReader(file)
	SetCurrentInputPort(newPort)
	defer SetCurrentInputPort(savedPort)

	// Call thunk in sub-context
	sub := mc.NewSubContext()
	if _, err := sub.Apply(thunk); err != nil {
		return err
	}
	if err := sub.Run(ctx); err != nil {
		// Propagate continuation escapes
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(err, &escapeErr) {
			return err
		}
		if !errors.Is(err, machine.ErrMachineHalt) {
			return err
		}
	}

	mc.SetValue(sub.GetValue())
	return nil
}
