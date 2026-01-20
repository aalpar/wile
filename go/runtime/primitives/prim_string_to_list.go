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

// PrimStringToList implements the string->list primitive.
// R7RS §6.7: (string->list string [start [end]])
// Converts a string (or substring) to a list of characters.
func PrimStringToList(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	rest := mc.Arg(1)

	s, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string->list: expected a string but got %T", o)
	}

	runes := s.Runes()
	length := len(runes)
	start := 0
	end := length

	// Parse optional arguments: [start [end]]
	if rest != values.EmptyList {
		pair, ok := rest.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "string->list: improper argument list")
		}

		// Parse start
		startVal, ok := pair.Car().(*values.Integer)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "string->list: expected an integer for start but got %T", pair.Car())
		}
		start = int(startVal.Value)

		// Check for end argument
		if pair.Cdr() != values.EmptyList {
			pair2, ok := pair.Cdr().(*values.Pair)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAList, "string->list: improper argument list")
			}
			endVal, ok := pair2.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotANumber, "string->list: expected an integer for end but got %T", pair2.Car())
			}
			end = int(endVal.Value)
		}
	}

	// Validate indices
	if start < 0 || end > length || start > end {
		return values.NewForeignError("string->list: invalid indices")
	}

	// Build the list from the substring
	var result values.Value = values.EmptyList
	for i := end - 1; i >= start; i-- {
		result = values.NewCons(values.NewCharacter(runes[i]), result)
	}
	mc.SetValue(result)
	return nil
}
