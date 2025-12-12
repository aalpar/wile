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

// PrimForEach implements the (for-each) primitive.
// Applies procedure to each list element for side effects.
func PrimForEach(ctx context.Context, mc *machine.MachineContext) error {
	proc := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	listsVal := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	mcls, ok := proc.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "for-each: expected a procedure but got %T", proc)
	}

	if values.IsEmptyList(listsVal) {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "for-each: expected at least one list")
	}

	// Collect all lists into a slice
	var lists []values.Value
	current := listsVal
	for !values.IsEmptyList(current) {
		pair, ok := current.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "for-each: improper argument list")
		}
		lists = append(lists, pair.Car())
		current = pair.Cdr()
	}

	// Check if any list is empty
	for i, lst := range lists {
		if values.IsEmptyList(lst) {
			mc.SetValues()
			return nil
		}
		if _, ok := lst.(*values.Pair); !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "for-each: argument %d is not a list", i+1)
		}
	}

	sub := mc.NewSubContext()

	// Iterate through all lists in parallel
	for {
		// Collect one element from each list
		args := make(values.Vector, len(lists))
		allDone := false
		for i, lst := range lists {
			if values.IsEmptyList(lst) {
				allDone = true
				break
			}
			pair, ok := lst.(*values.Pair)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAList, "for-each: argument %d is an improper list", i+1)
			}
			args[i] = pair.Car()
			lists[i] = pair.Cdr()
		}
		if allDone {
			break
		}

		// Apply proc to collected arguments
		if _, err := sub.Apply(mcls, args...); err != nil {
			return err
		}
		if err := sub.Run(ctx); err != nil {
			var escapeErr *machine.ErrContinuationEscape
			if errors.As(err, &escapeErr) {
				return err
			}
			if !errors.Is(err, machine.ErrMachineHalt) {
				return err
			}
		}
	}

	mc.SetValues()
	return nil
}
