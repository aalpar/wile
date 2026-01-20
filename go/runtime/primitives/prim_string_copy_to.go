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

// PrimStringCopyTo implements the string-copy! primitive.
// Copies characters from string from to string to, starting at position at.
// R7RS §6.7: (string-copy! to at from [start [end]])
func PrimStringCopyTo(_ context.Context, mc *machine.MachineContext) error {
	toArg := mc.Arg(0)
	rest := mc.Arg(1)

	to, ok := toArg.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-copy!: expected a string for 'to' but got %T", toArg)
	}

	// Parse variadic arguments: at from [start [end]]
	var args []values.Value
	current := rest
	for current != values.EmptyList {
		pair, ok := current.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "string-copy!: improper argument list")
		}
		args = append(args, pair.Car())
		current = pair.Cdr()
	}

	if len(args) < 2 {
		return values.NewForeignError("string-copy!: expected at least 3 arguments")
	}

	atVal, ok := args[0].(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "string-copy!: expected an integer for 'at' but got %T", args[0])
	}
	at := int(atVal.Value)

	from, ok := args[1].(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-copy!: expected a string for 'from' but got %T", args[1])
	}

	fromLen := from.Len()
	start := 0
	end := fromLen

	if len(args) >= 3 {
		startVal, ok := args[2].(*values.Integer)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "string-copy!: expected an integer for start but got %T", args[2])
		}
		start = int(startVal.Value)
	}

	if len(args) >= 4 {
		endVal, ok := args[3].(*values.Integer)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "string-copy!: expected an integer for end but got %T", args[3])
		}
		end = int(endVal.Value)
	}

	// Validate indices
	if start < 0 || end > fromLen || start > end {
		return values.NewForeignError("string-copy!: invalid source indices")
	}

	toLen := to.Len()
	copyLen := end - start
	if at < 0 || at+copyLen > toLen {
		return values.NewForeignError("string-copy!: destination index out of bounds")
	}

	// Perform the copy using Go's optimized copy function
	toRunes := to.Runes()
	fromRunes := from.Runes()
	copy(toRunes[at:], fromRunes[start:end])
	to.SetValue(string(toRunes))

	mc.SetValue(values.Void)
	return nil
}
