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

	"wile/machine"
	"wile/values"
)

// PrimStringForEach implements the string-for-each primitive.
// Applies proc element-wise to the characters of the strings for side effects.
// R7RS §6.7: (string-for-each proc string1 string2 ...)
func PrimStringForEach(ctx context.Context, mc *machine.MachineContext) error {
	proc := mc.Arg(0)
	stringsVal := mc.Arg(1)

	mcls, ok := proc.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "string-for-each: expected a procedure but got %T", proc)
	}

	if values.IsEmptyList(stringsVal) {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "string-for-each: expected at least one string")
	}

	// Collect all strings into a slice
	var strings []*values.String
	current := stringsVal
	for !values.IsEmptyList(current) {
		pair, ok := current.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "string-for-each: improper argument list")
		}
		s, ok := pair.Car().(*values.String)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAString, "string-for-each: expected a string but got %T", pair.Car())
		}
		strings = append(strings, s)
		current = pair.Cdr()
	}

	if len(strings) == 0 {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "string-for-each: expected at least one string")
	}

	// Convert all strings to rune slices and find minimum length
	runeSlices := make([][]rune, len(strings))
	minLen := -1
	for i, s := range strings {
		runeSlices[i] = s.Runes()
		if minLen < 0 || len(runeSlices[i]) < minLen {
			minLen = len(runeSlices[i])
		}
	}

	// Apply proc to each position
	sub := mc.NewSubContext()

	for i := 0; i < minLen; i++ {
		// Collect one character from each string
		args := make(values.Vector, len(strings))
		for j := range strings {
			args[j] = values.NewCharacter(runeSlices[j][i])
		}

		// Apply proc to collected arguments
		_, err := sub.Apply(mcls, args...)
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
	}

	mc.SetValue(values.Void)
	return nil
}
