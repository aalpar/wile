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
	"unicode/utf8"

	"wile/environment"
	"wile/machine"
	"wile/values"
)

// PrimWriteChar implements the write-char primitive.
// Writes a character to the current output port or to the specified output port.
func PrimWriteChar(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := obj.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "expected a character but got %T", obj)
	}
	o := mc.EnvironmentFrame().GetLocalBinding(environment.NewLocalIndex(1, 0)).Value()
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
	buf := make([]byte, 0, utf8.UTFMax)
	_, err := writer.Write(utf8.AppendRune(buf, ch.Value))
	if err != nil {
		return values.WrapForeignErrorf(err, "error writing character to output port")
	}
	mc.SetValues()
	return nil
}
