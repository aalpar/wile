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

	"wile/machine"
	"wile/values"
)

// PrimStringSet implements the string-set! primitive.
// Stores char in element k of string.
// R7RS §6.7: (string-set! string k char)
func PrimStringSet(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	k := mc.Arg(1)
	c := mc.Arg(2)
	s, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-set!: expected a string but got %T", o)
	}
	idx, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "string-set!: expected an integer but got %T", k)
	}
	char, ok := c.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "string-set!: expected a character but got %T", c)
	}
	length := s.Len()
	if idx.Value < 0 || idx.Value >= int64(length) {
		return values.NewForeignError("string-set!: index out of bounds")
	}
	s.SetChar(int(idx.Value), char.Value)
	mc.SetValue(values.Void)
	return nil
}
