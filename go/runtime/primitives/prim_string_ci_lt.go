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
	"strings"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

func PrimStringCiLt(_ context.Context, mc *machine.MachineContext) error {
	s1 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	s2 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	str1, ok := s1.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-ci<?: expected a string but got %T", s1)
	}
	str2, ok := s2.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-ci<?: expected a string but got %T", s2)
	}
	mc.SetValue(utils.BoolToBoolean(strings.ToLower(str1.Value) < strings.ToLower(str2.Value)))
	return nil
}
