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

// StringCompare is a helper for string comparison primitives.
// It extracts two strings from local bindings and applies the comparator.
func StringCompare(mc *machine.MachineContext, name string, cmp func(a, b string) bool) error {
	s1 := mc.Arg(0)
	s2 := mc.Arg(1)
	str1, ok := s1.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "%s: expected a string but got %T", name, s1)
	}
	str2, ok := s2.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "%s: expected a string but got %T", name, s2)
	}
	mc.SetValue(utils.BoolToBoolean(cmp(str1.Value, str2.Value)))
	return nil
}

// StringCompareVariadic is a helper for variadic string comparison primitives.
// It extracts strings from the variadic args and applies the comparator pairwise.
func StringCompareVariadic(mc *machine.MachineContext, name string, cmp func(a, b string) bool) error {
	s1 := mc.Arg(0)
	str1, ok := s1.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "%s: expected a string but got %T", name, s1)
	}

	rest := mc.Arg(1)
	prev := str1.Value

	for rest != values.EmptyList {
		pair, ok := rest.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a list", name)
		}
		str, ok := pair.Car().(*values.String)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAString, "%s: expected a string but got %T", name, pair.Car())
		}
		if !cmp(prev, str.Value) {
			mc.SetValue(values.FalseValue)
			return nil
		}
		prev = str.Value
		rest = pair.Cdr()
	}

	mc.SetValue(values.TrueValue)
	return nil
}
