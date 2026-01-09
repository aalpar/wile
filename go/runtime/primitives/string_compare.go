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
	"wile/machine"
	"wile/utils"
	"wile/values"
)

// stringCompare is a helper for string comparison primitives.
// It extracts two strings from local bindings and applies the comparator.
func stringCompare(mc *machine.MachineContext, name string, cmp func(a, b string) bool) error {
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
