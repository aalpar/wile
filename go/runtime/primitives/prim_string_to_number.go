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
	"strconv"

	"wile/machine"
	"wile/values"
)

// PrimStringToNumber implements the string->number primitive.
// Parses a string as a number with optional radix. Returns #f on parse failure.
func PrimStringToNumber(_ context.Context, mc *machine.MachineContext) error {
	s := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	str, ok := s.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string->number: expected a string but got %T", s)
	}
	radix := 10
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(*values.Pair)
		if ok && !values.IsEmptyList(pr) {
			r, ok := pr.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotANumber, "string->number: expected an integer radix but got %T", pr.Car())
			}
			radix = int(r.Value)
		}
	}
	// Try integer first
	if i, err := strconv.ParseInt(str.Value, radix, 64); err == nil {
		mc.SetValue(values.NewInteger(i))
		return nil
	}
	// Try float (only for base 10)
	if radix == 10 {
		if f, err := strconv.ParseFloat(str.Value, 64); err == nil {
			mc.SetValue(values.NewFloat(f))
			return nil
		}
	}
	mc.SetValue(values.FalseValue)
	return nil
}
