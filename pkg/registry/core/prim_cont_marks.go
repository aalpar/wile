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
		cms, err := collectMarksUpToPrompt(mc, tag, "current-continuation-marks")
		if err != nil {
			return err
		}
		mc.SetValue(cms)
		return nil
	}
	mc.SetValue(mc.CollectContinuationMarks(tag))
	return nil
}

// requireMarkSetOrCurrent returns the continuation mark set in argument 0 of the
// primitive called name. Racket lets #f stand for the current continuation's marks
// there, so #f yields the set (current-continuation-marks tag) would; any other
// value that is not a mark set is a type error. Only the #f arm needs the machine
// context, so a real mark set still works under a foreign CallContext.
//
// tag is the accessor's optional trailing prompt tag (nil when absent). Racket
// also truncates an explicit mark set at that tag's prompt; a Wile mark set
// records no prompt boundaries, so that combination is refused rather than
// answered from the whole set.
func requireMarkSetOrCurrent(cc machine.CallContext, name string, tag *machine.PromptTag) (*machine.ContinuationMarkSet, error) {
	arg := cc.Arg(0)
	if arg != values.FalseValue {
		cms, ok := arg.(*machine.ContinuationMarkSet)
		if !ok {
			return nil, werr.WrapForeignErrorf(werr.ErrNotAContinuationMarkSet,
				"%s: expected a continuation mark set or #f but got %T", name, arg)
		}
		if tag != nil {
			return nil, werr.WrapForeignErrorf(werr.ErrNotImplemented,
				"%s: prompt tag with an explicit mark set is not implemented (the set records no prompt boundaries; pass #f)", name)
		}
		return cms, nil
	}
	mc, err := machine.RequireMachineContext(cc, name)
	if err != nil {
		return nil, err
	}
	if tag == nil {
		return mc.CollectContinuationMarks(machine.DefaultPromptTag), nil
	}
	return collectMarksUpToPrompt(mc, tag, name)
}

// collectMarksUpToPrompt collects the current marks up to the prompt tagged
// tag, which a caller named explicitly: with no such prompt on the
// continuation Racket raises, where the collector alone would silently walk the
// whole chain. Same refusal and sentinel as call-with-composable-continuation's.
// A prompt beyond an eval or load boundary counts (PromptOwner), as the
// collection crosses it too. The default tag is exempt: Racket gives every
// continuation a default prompt at its root.
func collectMarksUpToPrompt(mc *machine.MachineContext, tag *machine.PromptTag, name string) (*machine.ContinuationMarkSet, error) {
	if mc.PromptOwner(tag) == nil && tag != machine.DefaultPromptTag {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAPromptTag, "%s: no prompt found for tag %s", name, tag.SchemeString())
	}
	return mc.CollectContinuationMarks(tag), nil
}

// optionalPromptTag parses an accessor's trailing optional prompt tag from rest,
// which holds nothing else; nil when absent.
func optionalPromptTag(rest values.Value, name string) (*machine.PromptTag, error) {
	v, ok, err := helpers.ParseOptionalArg(rest, name)
	if err != nil {
		return nil, err
	}
	if !ok {
		return nil, nil
	}
	return helpers.RequireType[*machine.PromptTag](v, werr.ErrNotAPromptTag, name)
}

// optionalValueAndPromptTag parses an accessor's trailing (value [prompt-tag])
// from rest: the value, whether it was given, and the tag (nil when absent).
func optionalValueAndPromptTag(rest values.Value, name string) (values.Value, bool, *machine.PromptTag, error) {
	if values.IsEmptyList(rest) {
		return nil, false, nil, nil
	}
	v, tail, err := helpers.Uncons(rest, name, "optional argument")
	if err != nil {
		return nil, false, nil, err
	}
	tag, err := optionalPromptTag(tail, name)
	if err != nil {
		return nil, false, nil, err
	}
	return v, true, tag, nil
}

// PrimRequireContinuationMarkSet returns its argument if it is a continuation mark
// set and raises ErrNotAContinuationMarkSet otherwise. #f is refused: it is the
// accessors' alias for the current marks, which (wile control)'s
// continuation-mark-set->context does not share with them (Racket raises there).
func PrimRequireContinuationMarkSet(mc machine.CallContext) error {
	cms, err := helpers.RequireArg[*machine.ContinuationMarkSet](mc, 0, werr.ErrNotAContinuationMarkSet,
		"continuation-mark-set->context")
	if err != nil {
		return err
	}
	mc.SetValue(cms)
	return nil
}

// PrimContinuationMarkSetToList implements (continuation-mark-set->list mark-set key).
// Extracts a list of values for key across all frames in the mark set; #f for
// mark-set means the current continuation's marks.
func PrimContinuationMarkSetToList(mc machine.CallContext) error {
	tag, err := optionalPromptTag(mc.Arg(2), "continuation-mark-set->list")
	if err != nil {
		return err
	}
	cms, err := requireMarkSetOrCurrent(mc, "continuation-mark-set->list", tag)
	if err != nil {
		return err
	}
	mc.SetValue(cms.ToList(mc.Arg(1)))
	return nil
}

// PrimContinuationMarkSetFirst implements (continuation-mark-set-first mark-set key [default]).
// Returns the value for key from the nearest frame, or default (#f if omitted);
// #f for mark-set means the current continuation's marks.
func PrimContinuationMarkSetFirst(mc machine.CallContext) error {
	v, ok, tag, err := optionalValueAndPromptTag(mc.Arg(2), "continuation-mark-set-first")
	if err != nil {
		return err
	}
	cms, err := requireMarkSetOrCurrent(mc, "continuation-mark-set-first", tag)
	if err != nil {
		return err
	}
	key := mc.Arg(1)
	defaultVal := values.Value(values.FalseValue)
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
// #f for mark-set means the current continuation's marks.
//
// Racket §10.5: continuation-mark-set->list*
func PrimContinuationMarkSetToListStar(mc machine.CallContext) error {
	v, noneGiven, tag, err := optionalValueAndPromptTag(mc.Arg(2), "continuation-mark-set->list*")
	if err != nil {
		return err
	}
	cms, err := requireMarkSetOrCurrent(mc, "continuation-mark-set->list*", tag)
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
	if noneGiven {
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
// call/cc) or a composable one (call-with-composable-continuation), optionally
// stopping at prompt-tag.
func PrimContinuationMarks(mc machine.CallContext) error {
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
	// Racket's (continuation-marks #f) is the EMPTY set, not the current marks
	// that #f means to the accessors. The tag is checked first, as Racket does.
	if mc.Arg(0) == values.FalseValue {
		mc.SetValue(&machine.ContinuationMarkSet{})
		return nil
	}
	switch k := mc.Arg(0).(type) {
	case *machine.CapturedContinuation:
		mc.SetValue(machine.CollectMarksFromCapture(k.ComposableContinuation(), tag))
	case *machine.ComposableContinuation:
		// The segment ends at its prompt, and the walk at the segment's bottom frame
		// (even after an invocation grafted it onto a caller), so its marks stop
		// there whatever the tag.
		mc.SetValue(machine.CollectMarksFromCapture(k, tag))
	default:
		return werr.WrapForeignErrorf(werr.ErrNotAContinuation,
			"continuation-marks: expected a continuation but got %T", mc.Arg(0))
	}
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
