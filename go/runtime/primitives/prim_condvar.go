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

// SRFI-18 Condition Variable Primitives
// See: https://srfi.schemers.org/srfi-18/srfi-18.html

package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimConditionVariableQ tests if an object is a condition variable
// (condition-variable? obj) -> boolean
func PrimConditionVariableQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*values.ConditionVariable)
	if ok {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimMakeConditionVariable creates a new condition variable
// (make-condition-variable [name]) -> condition-variable
func PrimMakeConditionVariable(_ context.Context, mc *machine.MachineContext) error {
	restVal := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

	name := ""
	// Parse optional name from rest list
	if !values.IsEmptyList(restVal) {
		if restList, ok := restVal.(*values.Pair); ok {
			nameVal := restList.Car()
			if s, ok := nameVal.(*values.String); ok {
				name = s.Value
			} else if sym, ok := nameVal.(*values.Symbol); ok {
				name = sym.Key
			}
		}
	}

	cv := values.NewConditionVariable(name)
	mc.SetValue(cv)
	return nil
}

// PrimConditionVariableName returns the condition variable's name
// (condition-variable-name cv) -> string or symbol
func PrimConditionVariableName(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	cv, ok := o.(*values.ConditionVariable)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAConditionVariable, "condition-variable-name: expected condition-variable, got %T", o)
	}
	mc.SetValue(values.NewString(cv.Name()))
	return nil
}

// PrimConditionVariableSpecific returns the condition variable's specific field
// (condition-variable-specific cv) -> value
func PrimConditionVariableSpecific(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	cv, ok := o.(*values.ConditionVariable)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAConditionVariable, "condition-variable-specific: expected condition-variable, got %T", o)
	}
	v := cv.Specific()
	if v == nil {
		mc.SetValue(values.Void)
	} else {
		mc.SetValue(v)
	}
	return nil
}

// PrimConditionVariableSpecificSet sets the condition variable's specific field
// (condition-variable-specific-set! cv obj) -> void
func PrimConditionVariableSpecificSet(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	val := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	cv, ok := o.(*values.ConditionVariable)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAConditionVariable, "condition-variable-specific-set!: expected condition-variable, got %T", o)
	}

	cv.SetSpecific(val)
	mc.SetValue(values.Void)
	return nil
}

// PrimConditionVariableSignal signals one waiting thread
// (condition-variable-signal! cv) -> void
func PrimConditionVariableSignal(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	cv, ok := o.(*values.ConditionVariable)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAConditionVariable, "condition-variable-signal!: expected condition-variable, got %T", o)
	}
	cv.Signal()
	mc.SetValue(values.Void)
	return nil
}

// PrimConditionVariableBroadcast signals all waiting threads
// (condition-variable-broadcast! cv) -> void
func PrimConditionVariableBroadcast(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	cv, ok := o.(*values.ConditionVariable)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAConditionVariable, "condition-variable-broadcast!: expected condition-variable, got %T", o)
	}
	cv.Broadcast()
	mc.SetValue(values.Void)
	return nil
}
