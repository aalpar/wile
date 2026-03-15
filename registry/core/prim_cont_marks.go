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

package core

import (
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimCurrentContinuationMarks implements (current-continuation-marks [prompt-tag]).
// Walks the continuation chain and returns a ContinuationMarkSet snapshot.
func PrimCurrentContinuationMarks(mc *machine.MachineContext) error {
	tag := machine.DefaultPromptTag
	v, ok := helpers.ParseOptionalArg(mc.Arg(0))
	if ok {
		var err error
		tag, err = helpers.RequireType[*machine.PromptTag](v, werr.ErrNotAPromptTag, "current-continuation-marks")
		if err != nil {
			return err
		}
	}
	mc.SetValue(mc.CollectContinuationMarks(tag))
	return nil
}

// PrimContinuationMarkSetToList implements (continuation-mark-set->list mark-set key).
// Extracts a list of values for key across all frames in the mark set.
func PrimContinuationMarkSetToList(mc *machine.MachineContext) error {
	cms, err := helpers.RequireType[*machine.ContinuationMarkSet](mc.Arg(0), werr.ErrNotAContinuationMarkSet, "continuation-mark-set->list")
	if err != nil {
		return err
	}
	mc.SetValue(cms.ToList(mc.Arg(1)))
	return nil
}

// PrimContinuationMarkSetFirst implements (continuation-mark-set-first mark-set key [default]).
// Returns the value for key from the nearest frame, or default (#f if omitted).
func PrimContinuationMarkSetFirst(mc *machine.MachineContext) error {
	cms, err := helpers.RequireType[*machine.ContinuationMarkSet](mc.Arg(0), werr.ErrNotAContinuationMarkSet, "continuation-mark-set-first")
	if err != nil {
		return err
	}
	key := mc.Arg(1)
	defaultVal := values.Value(values.FalseValue)
	v, ok := helpers.ParseOptionalArg(mc.Arg(2))
	if ok {
		defaultVal = v
	}
	mc.SetValue(cms.First(key, defaultVal))
	return nil
}

// PrimContinuationMarkSetQ implements (continuation-mark-set? obj).
var PrimContinuationMarkSetQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*machine.ContinuationMarkSet)
	return ok
})

// PrimCallWithImmediateContMark implements
// (call-with-immediate-continuation-mark key proc [default]).
//
// Gets the nearest mark for key in the current continuation, then calls proc
// with that value. If no mark is set, calls proc with default (#f if omitted).
//
// Uses GetImmediateMark, which checks the live frame first, then the saved
// continuation frame — covering both tail and non-tail compilation contexts.
// In tail position, with-continuation-mark writes to mc.marks. In non-tail
// position, SaveContinuation moves mc.marks to mc.cont before the call.
func PrimCallWithImmediateContMark(mc *machine.MachineContext) error {
	key := mc.Arg(0)
	proc := mc.Arg(1)
	val := mc.GetImmediateMark(key)
	if val == nil {
		val = values.FalseValue
		v, ok := helpers.ParseOptionalArg(mc.Arg(2))
		if ok {
			val = v
		}
	}
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	sub.SetWindingStack(mc.WindingStack())
	_, err := sub.ApplyCallable(proc, val)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
		return err
	}
	mc.SetValues(sub.GetValues()...)
	return nil
}
