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
	"io"

	"wile/machine"
	"wile/values"
)

// PrimDisplay implements the (display) primitive.
// Writes a human-readable representation of an object to an output port.
func PrimDisplay(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "expected a pair but got %T", o)
	}
	if !pr.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %s", pr.SchemeString())
	}
	var writer io.Writer
	if values.IsEmptyList(pr) {
		writer = GetCurrentOutputPort().Value
	} else {
		port := pr.Car()
		switch p := port.(type) {
		case *values.CharacterOutputPort:
			writer = p.Value
		case *values.StringOutputPort:
			writer = p
		case *values.BytevectorOutputPort:
			writer = p
		default:
			return values.WrapForeignErrorf(values.ErrNotAnOutputPort, "expected an output port but got %T", port)
		}
	}
	_, err := writer.Write([]byte(StringValue(obj)))
	if err != nil {
		return values.WrapForeignErrorf(err, "error writing to output port")
	}
	mc.SetValues()
	return nil
}
