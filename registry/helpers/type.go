// Copyright 2026 Aaron Alpar
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

package helpers

import (
	"context"

	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
)

// MakeTypePredicate creates a type predicate primitive function.
// The check function should return true if the value matches the expected type.
func MakeTypePredicate(check func(values.Value) bool) func(context.Context, *machine.MachineContext) error {
	return func(_ context.Context, mc *machine.MachineContext) error {
		o := mc.Arg(0)
		mc.SetValue(schemeutil.BoolToBoolean(check(o)))
		return nil
	}
}

// MakeNumericPredicate creates a numeric predicate primitive that extracts
// arg 0 via RequireArg[T] and applies a boolean test. Unlike MakeTypePredicate,
// this returns an error if the argument doesn't satisfy the type constraint
// (e.g., "exact? requires a number").
//
// T is typically values.Number or values.RealNumber.
func MakeNumericPredicate[T any](
	name string,
	sentinel error,
	test func(T) bool,
) func(context.Context, *machine.MachineContext) error {
	return func(_ context.Context, mc *machine.MachineContext) error {
		n, err := RequireArg[T](mc, 0, sentinel, name)
		if err != nil {
			return err
		}
		mc.SetValue(schemeutil.BoolToBoolean(test(n)))
		return nil
	}
}

// ChainEquality implements variadic chain equality comparison for primitives
// like boolean=? and symbol=?.
//
// The function takes:
//   - mc: Machine context with first arg at index 0, rest at index 1
//   - name: Primitive name for error messages
//   - typeCheck: Validates each argument is the correct type
//   - equals: Compares two values for equality
//
// Returns #t if all arguments pass type check and are equal to the first,
// #f on first inequality. Short-circuits and returns error on type mismatch.
func ChainEquality(
	mc *machine.MachineContext,
	name string,
	typeCheck func(values.Value) error,
	equals func(values.Value, values.Value) bool,
) error {
	first := mc.Arg(0)
	rest := mc.Arg(1)

	err := typeCheck(first)
	if err != nil {
		return err
	}

	current := rest
	for !values.IsEmptyList(current) {
		tuple, ok := current.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "%s: improper argument list", name)
		}
		arg := tuple.Car()
		err := typeCheck(arg)
		if err != nil {
			return err
		}
		if !equals(first, arg) {
			mc.SetValue(values.FalseValue)
			return nil
		}
		current = tuple.Cdr()
	}

	mc.SetValue(values.TrueValue)
	return nil
}
