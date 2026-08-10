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
	"context"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// PrimCurrentContinuationMarks implements (current-continuation-marks [prompt-tag]).
// Walks the continuation chain and returns a ContinuationMarkSet snapshot.
func PrimCurrentContinuationMarks(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "current-continuation-marks")
	if err != nil {
		return err
	}
	tag := machine.DefaultPromptTag
	v, ok, err := helpers.ParseOptionalArg(mc.Arg(0), "current-continuation-marks")
	if err != nil {
		return err
	}
	if ok {
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
func PrimContinuationMarkSetToList(mc machine.CallContext) error {
	cms, err := helpers.RequireType[*machine.ContinuationMarkSet](mc.Arg(0), werr.ErrNotAContinuationMarkSet, "continuation-mark-set->list")
	if err != nil {
		return err
	}
	mc.SetValue(cms.ToList(mc.Arg(1)))
	return nil
}

// PrimContinuationMarkSetFirst implements (continuation-mark-set-first mark-set key [default]).
// Returns the value for key from the nearest frame, or default (#f if omitted).
func PrimContinuationMarkSetFirst(mc machine.CallContext) error {
	cms, err := helpers.RequireType[*machine.ContinuationMarkSet](mc.Arg(0), werr.ErrNotAContinuationMarkSet, "continuation-mark-set-first")
	if err != nil {
		return err
	}
	key := mc.Arg(1)
	defaultVal := values.Value(values.FalseValue)
	v, ok, err := helpers.ParseOptionalArg(mc.Arg(2), "continuation-mark-set-first")
	if err != nil {
		return err
	}
	if ok {
		defaultVal = v
	}
	mc.SetValue(cms.First(key, defaultVal))
	return nil
}

// PrimContinuationMarkSetToListStar implements
// (continuation-mark-set->list* mark-set key-list [none-v]).
// Returns a list of vectors, one per frame that has at least one of the keys.
// Each vector position corresponds to a key in key-list.
// Missing keys use none-v (default #f).
//
// Racket §10.5: continuation-mark-set->list*
func PrimContinuationMarkSetToListStar(mc machine.CallContext) error {
	cms, err := helpers.RequireType[*machine.ContinuationMarkSet](mc.Arg(0), werr.ErrNotAContinuationMarkSet, "continuation-mark-set->list*")
	if err != nil {
		return err
	}

	keyListVal := mc.Arg(1)
	keyTuple, ok := keyListVal.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "continuation-mark-set->list*: expected a list of keys")
	}

	// The canonical proper-list eliminator, not a hand-rolled walk: it carries
	// the Brent cycle detector and the deadline poll. Without them a key list
	// closed into a cycle by set-cdr! grew `keys` at gigabytes per second and
	// never returned — and nothing could see it, because non-termination plus
	// allocation is not something the VM boundary's recover can catch.
	var keys []values.Value
	err = helpers.ForEachList(mc.Context(), keyTuple, "continuation-mark-set->list*",
		func(_ context.Context, _ int, _ bool, key values.Value) error {
			keys = append(keys, key)
			return nil
		})
	if err != nil {
		return err
	}

	noneVal := values.Value(values.FalseValue)
	v, ok, err := helpers.ParseOptionalArg(mc.Arg(2), "continuation-mark-set->list*")
	if err != nil {
		return err
	}
	if ok {
		noneVal = v
	}

	mc.SetValue(cms.ToListStar(keys, noneVal))
	return nil
}

// PrimContinuationMarkSetQ implements (continuation-mark-set? obj).
var PrimContinuationMarkSetQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*machine.ContinuationMarkSet)
	return ok
})

// PrimContinuationMarks implements (continuation-marks cont [prompt-tag]).
// Extracts a ContinuationMarkSet from a captured continuation (the result of
// call/cc), optionally stopping at prompt-tag.
func PrimContinuationMarks(mc machine.CallContext) error {
	capt, err := helpers.RequireType[*machine.CapturedContinuation](mc.Arg(0), werr.ErrNotAContinuation, "continuation-marks")
	if err != nil {
		return err
	}
	tag := machine.DefaultPromptTag
	v, ok, err := helpers.ParseOptionalArg(mc.Arg(1), "continuation-marks")
	if err != nil {
		return err
	}
	if ok {
		tag, err = helpers.RequireType[*machine.PromptTag](v, werr.ErrNotAPromptTag, "continuation-marks")
		if err != nil {
			return err
		}
	}
	mc.SetValue(machine.CollectMarksFromContinuation(capt.ComposableContinuation().Cont(), tag))
	return nil
}

// PrimContinuationQ implements (continuation? obj).
var PrimContinuationQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*machine.CapturedContinuation)
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
func PrimCallWithImmediateContMark(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "call-with-immediate-continuation-mark")
	if err != nil {
		return err
	}
	key := mc.Arg(0)
	proc := mc.Arg(1)
	val := mc.GetImmediateMark(key)
	if val == nil {
		val = values.FalseValue
		v, ok, err := helpers.ParseOptionalArg(mc.Arg(2), "call-with-immediate-continuation-mark")
		if err != nil {
			return err
		}
		if ok {
			val = v
		}
	}
	// Run proc in place (the apply recipe, prim_control.go) rather than in a
	// sub-context: proc's result flows through the continuation already on mc.cont,
	// and a continuation captured inside proc spans the live chain instead of a
	// walled-off sub-context. This fixes the non-tail-escape truncation
	// (continuation_subcontext_truncation_red_test.go, call-with-immediate-continuation-mark
	// case). There is no post-thunk work and no abort tag here, so no RunBodyUnder*
	// chain frame is needed — the transparent delivery is exactly what apply does.
	_, err = mc.ApplyCallable(proc, val)
	return err
}
