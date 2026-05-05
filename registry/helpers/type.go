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
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// MakeTypePredicate creates a type predicate primitive function.
// The check function should return true if the value matches the expected type.
func MakeTypePredicate(check func(values.Value) bool) machine.ForeignFunction {
	return func(mc machine.CallContext) error {
		o := mc.Arg(0)
		mc.SetValue(values.BoolToBoolean(check(o)))
		return nil
	}
}

// MakeNumericPredicate creates a numeric predicate primitive that extracts
// arg 0 via RequireArg[T] and applies a boolean test. Unlike MakeTypePredicate,
// this returns an error if the argument doesn't satisfy the type constraint
// (e.g., "exact? requires a number").
//
// T is typically values.Number or values.RealNumber. typeName is the
// human-readable expected-type phrase (e.g., "a number").
func MakeNumericPredicate[T any](
	name string,
	sentinel error,
	typeName string,
	test func(T) bool,
) machine.ForeignFunction {
	return func(mc machine.CallContext) error {
		n, err := RequireArg[T](mc, 0, sentinel, typeName, name)
		if err != nil {
			return err
		}
		mc.SetValue(values.BoolToBoolean(test(n)))
		return nil
	}
}

// MakeCharPredicate creates a character predicate primitive that extracts
// arg 0 as a Character and applies a boolean test on the rune value.
func MakeCharPredicate(name string, test func(rune) bool) machine.ForeignFunction {
	return func(mc machine.CallContext) error {
		ch, err := RequireArg[*values.Character](mc, 0, werr.ErrNotACharacter, "a character", name)
		if err != nil {
			return err
		}
		mc.SetValue(values.BoolToBoolean(test(ch.Value)))
		return nil
	}
}

// MakeCharTransform creates a character transformation primitive that extracts
// arg 0 as a Character, applies a rune transformation, and returns a new Character.
func MakeCharTransform(name string, transform func(rune) rune) machine.ForeignFunction {
	return func(mc machine.CallContext) error {
		ch, err := RequireArg[*values.Character](mc, 0, werr.ErrNotACharacter, "a character", name)
		if err != nil {
			return err
		}
		mc.SetValue(values.NewCharacter(transform(ch.Value)))
		return nil
	}
}
