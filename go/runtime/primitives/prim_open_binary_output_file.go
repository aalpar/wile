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

// PrimOpenBinaryOutputFile implements the open-binary-output-file primitive (R7RS).
// Opens a file for binary writing and returns a binary output port.
func PrimOpenBinaryOutputFile(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	filename, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "open-binary-output-file: expected a string but got %T", o)
	}
	file, err := os.Create(filename.Value)
	if err != nil {
		return values.WrapForeignErrorf(err, "open-binary-output-file: %v", err)
	}
	mc.SetValue(values.NewBinaryOutputPort(file))
	return nil
}
