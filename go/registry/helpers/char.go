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
	"wile/machine"
	"wile/utils"
	"wile/values"
)

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
	mc.SetValue(utils.BoolToBoolean(cmp(ch1.Value, ch2.Value)))
	return nil
}

// CharCompareVariadic is a helper for variadic character comparison primitives.
// It extracts characters from the variadic args and applies the comparator pairwise.
func CharCompareVariadic(mc *machine.MachineContext, name string, cmp func(a, b rune) bool) error {
	c1 := mc.Arg(0)
	ch1, ok := c1.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "%s: expected a character but got %T", name, c1)
	}

	rest := mc.Arg(1)
	prev := ch1.Value

	for rest != values.EmptyList {
		pair, ok := rest.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a list", name)
		}
		ch, ok := pair.Car().(*values.Character)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotACharacter, "%s: expected a character but got %T", name, pair.Car())
		}
		if !cmp(prev, ch.Value) {
			mc.SetValue(values.FalseValue)
			return nil
		}
		prev = ch.Value
		rest = pair.Cdr()
	}

	mc.SetValue(values.TrueValue)
	return nil
}
