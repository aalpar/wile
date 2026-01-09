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
	"os"

	"wile/machine"
	"wile/values"
)

// PrimWithInputFromFile implements the with-input-from-file primitive (R7RS).
// Opens a file for reading, temporarily sets it as current-input-port,
// calls the thunk, then restores the previous port and closes the file.
// (with-input-from-file string thunk)
// TODO: code here looks duplicated among the I/O primitives; refactor common parts
func PrimWithInputFromFile(ctx context.Context, mc *machine.MachineContext) error {
	filenameVal := mc.Arg(0)
	thunkVal := mc.Arg(1)

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
	defer file.Close() //nolint:errcheck

	// Save current port and set new one
	savedPort := GetCurrentInputPort()
	newPort := values.NewCharacterInputPortFromReader(file)
	SetCurrentInputPort(newPort)
	defer SetCurrentInputPort(savedPort)

	return duplicated1(ctx, mc, thunk)
}
