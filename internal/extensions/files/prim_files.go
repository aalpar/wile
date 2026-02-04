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

package files

import (
	"context"
	"errors"
	"os"

	extio "github.com/aalpar/wile/internal/extensions/io"
	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
)

// PrimOpenInputFile implements the open-input-file primitive.
// Opens a file for reading and returns an input port.
func PrimOpenInputFile(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	filename, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "open-input-file: expected a string but got %T", o)
	}
	file, err := os.Open(filename.Value)
	if err != nil {
		return values.WrapForeignFileError(err, "open-input-file", filename.Value)
	}
	mc.SetValue(values.NewCharacterInputPortFromReader(file))
	return nil
}

// PrimOpenOutputFile implements the open-output-file primitive.
// Opens a file for writing and returns an output port.
func PrimOpenOutputFile(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	filename, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "open-output-file: expected a string but got %T", o)
	}
	file, err := os.Create(filename.Value)
	if err != nil {
		return values.WrapForeignFileError(err, "open-output-file", filename.Value)
	}
	mc.SetValue(values.NewCharacterOutputPortFromWriter(file))
	return nil
}

// PrimOpenBinaryInputFile implements the open-binary-input-file primitive (R7RS).
// Opens a file for binary reading and returns a binary input port.
func PrimOpenBinaryInputFile(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	filename, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "open-binary-input-file: expected a string but got %T", o)
	}
	file, err := os.Open(filename.Value)
	if err != nil {
		return values.WrapForeignFileError(err, "open-binary-input-file", filename.Value)
	}
	mc.SetValue(values.NewBinaryInputPortFromReader(file))
	return nil
}

// PrimOpenBinaryOutputFile implements the open-binary-output-file primitive (R7RS).
// Opens a file for binary writing and returns a binary output port.
func PrimOpenBinaryOutputFile(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	filename, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "open-binary-output-file: expected a string but got %T", o)
	}
	file, err := os.Create(filename.Value)
	if err != nil {
		return values.WrapForeignFileError(err, "open-binary-output-file", filename.Value)
	}
	mc.SetValue(values.NewBinaryOutputPortFromWriter(file))
	return nil
}

// PrimFileExistsQ implements the (file-exists?) primitive.
// Returns #t if file exists.
func PrimFileExistsQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	filename, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "file-exists?: expected a string but got %T", o)
	}
	_, err := os.Stat(filename.Value)
	mc.SetValue(schemeutil.BoolToBoolean(err == nil))
	return nil
}

// PrimDeleteFile implements the (delete-file) primitive.
// Deletes a file from the filesystem.
func PrimDeleteFile(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	filename, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "delete-file: expected a string but got %T", o)
	}
	err := os.Remove(filename.Value)
	if err != nil {
		return values.WrapForeignFileError(err, "delete-file", filename.Value)
	}
	mc.SetValues()
	return nil
}

// callWithFile is a helper for call-with-input-file and call-with-output-file.
// Takes filename at index 0, proc at index 1. Opens file, creates port, calls proc.
//
//nolint:unparam
func callWithFile(
	ctx context.Context,
	mc *machine.MachineContext,
	name string,
	opener func(string) (*os.File, error),
	portCreator func(*os.File) values.Value,
) error {
	filenameVal := mc.Arg(0)
	procVal := mc.Arg(1)

	filename, ok := filenameVal.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "%s: expected a string but got %T", name, filenameVal)
	}

	proc, ok := procVal.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "%s: expected a procedure but got %T", name, procVal)
	}

	file, err := opener(filename.Value)
	if err != nil {
		return values.WrapForeignFileError(err, name, filename.Value)
	}
	defer file.Close() //nolint:errcheck

	port := portCreator(file)

	sub := mc.NewSubContext()
	_, err = sub.Apply(proc, port)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
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

// PrimCallWithInputFile implements the call-with-input-file primitive.
func PrimCallWithInputFile(ctx context.Context, mc *machine.MachineContext) error {
	return callWithFile(ctx, mc, "call-with-input-file", os.Open,
		func(f *os.File) values.Value { return values.NewCharacterInputPortFromReader(f) })
}

// PrimCallWithOutputFile implements the call-with-output-file primitive.
func PrimCallWithOutputFile(ctx context.Context, mc *machine.MachineContext) error {
	return callWithFile(ctx, mc, "call-with-output-file", os.Create,
		func(f *os.File) values.Value { return values.NewCharacterOutputPortFromWriter(f) })
}

// runThunk runs a thunk in a sub-context and returns the result.
func runThunk(ctx context.Context, mc *machine.MachineContext, thunk *machine.MachineClosure) error { //nolint:unparam
	sub := mc.NewSubContext()
	_, err := sub.Apply(thunk)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
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

// PrimWithInputFromFile implements the with-input-from-file primitive (R7RS).
// Opens a file for reading, temporarily sets it as current-input-port,
// calls the thunk, then restores the previous port and closes the file.
// (with-input-from-file string thunk)
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
		return values.WrapForeignFileError(err, "with-input-from-file", filename.Value)
	}
	defer file.Close() //nolint:errcheck

	// Save current port and set new one
	savedPort := extio.GetCurrentInputPort()
	newPort := values.NewCharacterInputPortFromReader(file)
	extio.SetCurrentInputPort(newPort)
	defer extio.SetCurrentInputPort(savedPort)

	return runThunk(ctx, mc, thunk)
}

// PrimWithOutputToFile implements the with-output-to-file primitive (R7RS).
// Opens a file for writing, temporarily sets it as current-output-port,
// calls the thunk, then restores the previous port and closes the file.
// (with-output-to-file string thunk)
func PrimWithOutputToFile(ctx context.Context, mc *machine.MachineContext) error {
	filenameVal := mc.Arg(0)
	thunkVal := mc.Arg(1)

	filename, ok := filenameVal.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "with-output-to-file: expected a string but got %T", filenameVal)
	}

	thunk, ok := thunkVal.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "with-output-to-file: expected a procedure but got %T", thunkVal)
	}

	// Open the file for writing (create or truncate)
	file, err := os.Create(filename.Value)
	if err != nil {
		return values.WrapForeignFileError(err, "with-output-to-file", filename.Value)
	}
	defer file.Close() //nolint:errcheck

	// Save current port and set new one
	savedPort := extio.GetCurrentOutputPort()
	newPort := values.NewCharacterOutputPortFromWriter(file)
	extio.SetCurrentOutputPort(newPort)
	defer extio.SetCurrentOutputPort(savedPort)

	return runThunk(ctx, mc, thunk)
}
