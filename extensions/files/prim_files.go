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
	"os"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
)

// PrimOpenInputFile implements the open-input-file primitive.
// Opens a file for reading and returns an input port.
func PrimOpenInputFile(_ context.Context, mc *machine.MachineContext) error {
	filename, err := helpers.RequireArg[*values.String](mc, 0, values.ErrNotAString, "open-input-file")
	if err != nil {
		return err
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
	filename, err := helpers.RequireArg[*values.String](mc, 0, values.ErrNotAString, "open-output-file")
	if err != nil {
		return err
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
	filename, err := helpers.RequireArg[*values.String](mc, 0, values.ErrNotAString, "open-binary-input-file")
	if err != nil {
		return err
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
	filename, err := helpers.RequireArg[*values.String](mc, 0, values.ErrNotAString, "open-binary-output-file")
	if err != nil {
		return err
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
	filename, err := helpers.RequireArg[*values.String](mc, 0, values.ErrNotAString, "file-exists?")
	if err != nil {
		return err
	}
	_, err = os.Stat(filename.Value)
	mc.SetValue(values.BoolToBoolean(err == nil))
	return nil
}

// PrimDeleteFile implements the (delete-file) primitive.
// Deletes a file from the filesystem.
func PrimDeleteFile(_ context.Context, mc *machine.MachineContext) error {
	filename, err := helpers.RequireArg[*values.String](mc, 0, values.ErrNotAString, "delete-file")
	if err != nil {
		return err
	}
	err = os.Remove(filename.Value)
	if err != nil {
		return values.WrapForeignFileError(err, "delete-file", filename.Value)
	}
	mc.SetValue(values.Void)
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
	filename, err := helpers.RequireType[*values.String](mc.Arg(0), values.ErrNotAString, name)
	if err != nil {
		return err
	}

	proc, err := helpers.RequireType[*machine.MachineClosure](mc.Arg(1), values.ErrNotAProcedure, name)
	if err != nil {
		return err
	}

	file, err := opener(filename.Value)
	if err != nil {
		return values.WrapForeignFileError(err, name, filename.Value)
	}
	defer file.Close() //nolint:errcheck

	port := portCreator(file)

	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	_, err = sub.Apply(proc, port)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
		return err
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

// PrimWithInputFromFile and PrimWithOutputToFile have been moved to
// with_file_macros.scm as macros using parameterize. This ensures proper
// integration with the continuation system (fixes T3 from architectural review).
