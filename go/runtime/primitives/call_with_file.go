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

// callWithFile is a helper for call-with-input-file and call-with-output-file.
// Takes filename at index 0, proc at index 1. Opens file, creates port, calls proc.
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
		return values.WrapForeignErrorf(err, "%s: %v", name, err)
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
