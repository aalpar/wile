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
	"strings"

	"wile/machine"
	"wile/values"
)

// PrimMakeString implements the make-string primitive.
// (make-string k) creates a string of k unspecified characters.
// (make-string k char) creates a string of k copies of char.
func PrimMakeString(_ context.Context, mc *machine.MachineContext) error {
	k := mc.Arg(0)
	kInt, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInteger, "make-string: expected an integer but got %T", k)
	}
	if kInt.Value < 0 {
		return values.NewForeignError("make-string: length must be non-negative")
	}

	fillChar := rune(0) // default fill character (NUL)
	rest := mc.Arg(1)
	if rest != values.EmptyList {
		pair, ok := rest.(*values.Pair)
		if ok {
			ch, ok := pair.Car().(*values.Character)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotACharacter, "make-string: expected a character but got %T", pair.Car())
			}
			fillChar = ch.Value
		}
	}

	q := values.NewString(strings.Repeat(string(fillChar), int(kInt.Value)))
	mc.SetValue(q)
	return nil
}
