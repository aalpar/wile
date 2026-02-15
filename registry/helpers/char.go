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
)

// variadicCompare is a generic helper for variadic comparison primitives.
// It extracts values, compares them pairwise, and returns a boolean result.
func variadicCompare[T any, V values.Value](
	mc *machine.MachineContext,
	name string,
	extract func(values.Value) (V, bool),
	getValue func(V) T,
	cmp func(T, T) bool,
	errType error,
	typeName string,
) error {
	first := mc.Arg(0)
	val1, ok := extract(first)
	if !ok {
		return values.WrapForeignErrorf(errType, "%s: expected %s but got %T", name, typeName, first)
	}

	rest := mc.Arg(1)
	prev := getValue(val1)

	for !values.IsEmptyList(rest) {
		pair, ok := rest.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a list", name)
		}
		val, ok := extract(pair.Car())
		if !ok {
			return values.WrapForeignErrorf(errType, "%s: expected %s but got %T", name, typeName, pair.Car())
		}
		current := getValue(val)
		if !cmp(prev, current) {
			mc.SetValue(values.FalseValue)
			return nil
		}
		prev = current
		rest = pair.Cdr()
	}

	mc.SetValue(values.TrueValue)
	return nil
}

// CharCompare is a helper for character comparison primitives.
// It extracts two characters from local bindings and applies the comparator.
func CharCompare(mc *machine.MachineContext, name string, cmp func(a, b rune) bool) error {
	c1 := mc.Arg(0)
	c2 := mc.Arg(1)
	ch1, ok := c1.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "%s: expected a character but got %T", name, c1)
	}
	ch2, ok := c2.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "%s: expected a character but got %T", name, c2)
	}
	mc.SetValue(values.BoolToBoolean(cmp(ch1.Value, ch2.Value)))
	return nil
}

// CharCompareVariadic is a helper for variadic character comparison primitives.
// It extracts characters from the variadic args and applies the comparator pairwise.
func CharCompareVariadic(mc *machine.MachineContext, name string, cmp func(a, b rune) bool) error {
	return variadicCompare(mc, name,
		func(v values.Value) (*values.Character, bool) {
			c, ok := v.(*values.Character)
			return c, ok
		},
		func(c *values.Character) rune {
			return c.Value
		},
		cmp,
		values.ErrNotACharacter,
		"a character")
}
