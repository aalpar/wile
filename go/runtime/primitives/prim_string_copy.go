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

// PrimStringCopy implements the string-copy primitive.
// R7RS §6.7: (string-copy string [start [end]])
// Returns a newly allocated copy of the given string (or substring).
// The returned string is mutable and distinct from the original.
func PrimStringCopy(_ context.Context, mc *machine.MachineContext) error {
	s := mc.Arg(0)
	rest := mc.Arg(1)

	str, ok := s.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-copy: expected a string but got %T", s)
	}

	runes := str.Runes()
	length := len(runes)
	start := 0
	end := length

	// Parse optional arguments: [start [end]]
	if rest != values.EmptyList {
		pair, ok := rest.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "string-copy: improper argument list")
		}

		// Parse start
		startVal, ok := pair.Car().(*values.Integer)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "string-copy: expected an integer for start but got %T", pair.Car())
		}
		start = int(startVal.Value)

		// Check for end argument
		if pair.Cdr() != values.EmptyList {
			pair2, ok := pair.Cdr().(*values.Pair)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAList, "string-copy: improper argument list")
			}
			endVal, ok := pair2.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotANumber, "string-copy: expected an integer for end but got %T", pair2.Car())
			}
			end = int(endVal.Value)
		}
	}

	// Validate indices
	if start < 0 || end > length || start > end {
		return values.NewForeignError("string-copy: invalid indices")
	}

	// Use NewMutableString to ensure the copy is not interned and can be mutated
	mc.SetValue(values.NewMutableString(string(runes[start:end])))
	return nil
}
