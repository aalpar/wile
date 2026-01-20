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

// PrimStringFill implements the string-fill! primitive.
// Fills the string (or a portion of it) with the given character.
// R7RS §6.7: (string-fill! string fill [start [end]])
func PrimStringFill(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	rest := mc.Arg(1)

	s, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-fill!: expected a string but got %T", o)
	}

	// Parse variadic arguments: fill [start [end]]
	var args []values.Value
	current := rest
	for current != values.EmptyList {
		pair, ok := current.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "string-fill!: improper argument list")
		}
		args = append(args, pair.Car())
		current = pair.Cdr()
	}

	if len(args) < 1 {
		return values.NewForeignError("string-fill!: expected at least 2 arguments")
	}

	char, ok := args[0].(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "string-fill!: expected a character but got %T", args[0])
	}

	length := s.Len()
	start := 0
	end := length

	if len(args) >= 2 {
		startVal, ok := args[1].(*values.Integer)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "string-fill!: expected an integer for start but got %T", args[1])
		}
		start = int(startVal.Value)
	}

	if len(args) >= 3 {
		endVal, ok := args[2].(*values.Integer)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "string-fill!: expected an integer for end but got %T", args[2])
		}
		end = int(endVal.Value)
	}

	if start < 0 || end > length || start > end {
		return values.NewForeignError("string-fill!: invalid indices")
	}

	s.Fill(char.Value, start, end)
	mc.SetValue(values.Void)
	return nil
}
