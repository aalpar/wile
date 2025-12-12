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

// SRFI-18 Time Primitives
// See: https://srfi.schemers.org/srfi-18/srfi-18.html

package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimCurrentTime returns the current time
// (current-time) -> time
func PrimCurrentTime(_ context.Context, mc *machine.MachineContext) error {
	mc.SetValue(values.CurrentTime())
	return nil
}

// PrimTimeQ tests if an object is a time
// (time? obj) -> boolean
func PrimTimeQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*values.Time)
	if ok {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimTimeToSeconds converts a time to seconds
// (time->seconds time) -> number
func PrimTimeToSeconds(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	t, ok := o.(*values.Time)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotATime, "time->seconds: expected time, got %T", o)
	}
	mc.SetValue(values.NewFloat(t.Seconds()))
	return nil
}

// PrimSecondsToTime converts seconds to a time
// (seconds->time x) -> time
func PrimSecondsToTime(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

	var seconds float64
	switch v := o.(type) {
	case *values.Integer:
		seconds = float64(v.Value)
	case *values.Float:
		seconds = v.Value
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "seconds->time: expected number, got %T", o)
	}

	mc.SetValue(values.NewTimeFromSeconds(seconds))
	return nil
}
