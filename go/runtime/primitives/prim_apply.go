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

// PrimApply implements the apply primitive.
// Applies a procedure to a list of arguments.
func PrimApply(ctx context.Context, mc *machine.MachineContext) error {
	proc := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	restVal := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	mcls, ok := proc.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "apply: expected a procedure but got %T", proc)
	}

	// R7RS: (apply proc arg1 ... args) combines arg1 ... with the final list args
	// restVal is a list containing (arg1 ... args) where args is the final list
	restList, ok := restVal.(*values.Pair)
	if !ok || values.IsEmptyList(restVal) {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "apply: expected at least one argument list")
	}

	// Collect all elements from rest except the last one, which is the final args list
	var prefixArgs values.Vector
	var finalList values.Value
	for {
		car := restList.Car()
		cdr := restList.Cdr()
		if values.IsEmptyList(cdr) {
			// This is the last element - it's the final args list
			finalList = car
			break
		}
		prefixArgs = append(prefixArgs, car)
		restList, ok = cdr.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "apply: improper rest argument list")
		}
	}

	// Now append elements from finalList to prefixArgs
	if !values.IsEmptyList(finalList) {
		finalPair, ok := finalList.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "apply: final argument must be a list but got %T", finalList)
		}
		v, err := finalPair.ForEach(nil, func(_ context.Context, i int, hasNext bool, elem values.Value) error {
			prefixArgs = append(prefixArgs, elem)
			return nil
		})
		if err != nil {
			return err
		}
		if !values.IsEmptyList(v) {
			return values.WrapForeignErrorf(values.ErrNotAList, "apply: final argument is an improper list")
		}
	}

	sub := mc.NewSubContext()
	if _, err := sub.Apply(mcls, prefixArgs...); err != nil {
		return err
	}
	if err := sub.Run(ctx); err != nil {
		// Propagate continuation escapes
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(err, &escapeErr) {
			return err
		}
		if !errors.Is(err, machine.ErrMachineHalt) {
			return err
		}
	}
	mc.SetValue(sub.GetValue())
	return nil
}
