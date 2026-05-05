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

// CharCompare is a helper for binary character comparison primitives.
// It extracts two characters from the primitive's arguments and applies the comparator.
func CharCompare(mc machine.CallContext, name string, cmp func(a, b rune) bool) error {
	c1 := mc.Arg(0)
	c2 := mc.Arg(1)
	ch1, ok := c1.(*values.Character)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotACharacter, "%s: expected a character but got %T", name, c1)
	}
	ch2, ok := c2.(*values.Character)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotACharacter, "%s: expected a character but got %T", name, c2)
	}
	mc.SetValue(values.BoolToBoolean(cmp(ch1.Value, ch2.Value)))
	return nil
}

// CharCompareVariadic is a helper for variadic character comparison primitives.
// It extracts characters from the variadic args and applies the comparator pairwise.
func CharCompareVariadic(mc machine.CallContext, name string, cmp func(a, b rune) bool) error {
	return CompareVariadic(mc, name, werr.ErrNotACharacter,
		func(c *values.Character) rune { return c.Value }, cmp)
}

// CompareVariadic implements pairwise chain comparison over a homogeneous
// variadic list. It collects fixed+rest args of type T via VariadicArgs, then
// applies cmp to every consecutive pair after extracting the underlying value
// V from each. Sets #t if all pairs satisfy cmp, #f on the first failure.
//
// Used for comparison primitives like char<?, string<?, etc. where the call
// shape is `(op a b c ...)` with all args of the same type.
func CompareVariadic[T values.Value, V any](
	mc machine.CallContext,
	name string,
	sentinel error,
	extract func(T) V,
	cmp func(V, V) bool,
) error {
	args, err := VariadicArgs[T](mc, 2, sentinel, name)
	if err != nil {
		return err
	}
	for i := 1; i < len(args); i++ {
		if !cmp(extract(args[i-1]), extract(args[i])) {
			mc.SetValue(values.FalseValue)
			return nil
		}
	}
	mc.SetValue(values.TrueValue)
	return nil
}
