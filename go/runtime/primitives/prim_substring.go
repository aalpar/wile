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

	"wile/machine"
	"wile/values"
)

// PrimSubstring implements the substring primitive.
// Returns a substring between the given start and end indices.
func PrimSubstring(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	start := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	end := mc.EnvironmentFrame().GetLocalBindingByIndex(2).Value()
	s, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "substring: expected a string but got %T", o)
	}
	startIdx, ok := start.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "substring: expected an integer but got %T", start)
	}
	endIdx, ok := end.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "substring: expected an integer but got %T", end)
	}
	runes := []rune(s.Value)
	if startIdx.Value < 0 || endIdx.Value > int64(len(runes)) || startIdx.Value > endIdx.Value {
		return values.NewForeignError("substring: invalid indices")
	}
	mc.SetValue(values.NewString(string(runes[startIdx.Value:endIdx.Value])))
	return nil
}
