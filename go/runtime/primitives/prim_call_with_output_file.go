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

// PrimCallWithOutputFile implements the call-with-output-file primitive.
// Opens a file for writing, calls the procedure with the port, then closes the port.
// (call-with-output-file string proc)
func PrimCallWithOutputFile(ctx context.Context, mc *machine.MachineContext) error {
	filenameVal := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	procVal := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	filename, ok := filenameVal.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "call-with-output-file: expected a string but got %T", filenameVal)
	}

	proc, ok := procVal.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "call-with-output-file: expected a procedure but got %T", procVal)
	}

	// Open the file for writing (create or truncate)
	file, err := os.Create(filename.Value)
	if err != nil {
		return values.WrapForeignErrorf(err, "call-with-output-file: %v", err)
	}

	// Create the port
	port := values.NewCharacterOutputPort(file)

	// Ensure file is closed when we're done
	defer file.Close()

	// Call the procedure with the port
	sub := mc.NewSubContext()
	if _, err := sub.Apply(proc, port); err != nil {
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
