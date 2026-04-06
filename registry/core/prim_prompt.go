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
	"errors"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimMakeContinuationPromptTag creates a new continuation prompt tag.
// With no arguments, creates an anonymous tag. With one argument (a symbol),
// creates a named tag for debugging purposes.
//
// Follows Racket's make-continuation-prompt-tag.
func PrimMakeContinuationPromptTag(mc machine.CallContext) error {
	restVal := mc.Arg(0)
	name := ""
	if !values.IsEmptyList(restVal) {
		tuple, ok := restVal.(values.Tuple)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAList, "make-continuation-prompt-tag: bad argument list")
		}
		sym, ok := tuple.Car().(*values.Symbol)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotASymbol, "make-continuation-prompt-tag: expected a symbol, got %T", tuple.Car())
		}
		name = sym.Key
	}
	mc.SetValue(machine.NewPromptTag(name))
	return nil
}

// PrimDefaultContinuationPromptTag returns the default continuation prompt tag.
func PrimDefaultContinuationPromptTag(mc machine.CallContext) error {
	mc.SetValue(machine.DefaultPromptTag)
	return nil
}

// PrimContinuationPromptTagQ tests whether a value is a continuation prompt tag.
var PrimContinuationPromptTagQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*machine.PromptTag)
	return ok
})

// PrimCallWithContinuationPrompt installs a continuation prompt and runs the thunk.
//
// (call-with-continuation-prompt thunk tag handler)
//
// The prompt frame is pushed onto the continuation chain. If the thunk aborts
// to this prompt's tag, the handler is invoked with the abort values.
// If the thunk returns normally, its value is the result.
//
// Follows Racket's call-with-continuation-prompt.
func PrimCallWithContinuationPrompt(cc machine.CallContext) error {
	mc := cc.(*machine.MachineContext)
	thunk := mc.Arg(0)
	tagVal := mc.Arg(1)
	handlerVal := mc.Arg(2)

	thunkCls, err := helpers.RequireType[machine.Closure](thunk, werr.ErrNotAProcedure, "call-with-continuation-prompt")
	if err != nil {
		return err
	}

	tag, err := helpers.RequireType[*machine.PromptTag](tagVal, werr.ErrNotAPromptTag, "call-with-continuation-prompt")
	if err != nil {
		return err
	}

	var handlerCls machine.Closure
	if handlerVal != values.FalseValue {
		var handlerErr error
		handlerCls, handlerErr = helpers.RequireType[machine.Closure](handlerVal, werr.ErrNotAProcedure, "call-with-continuation-prompt")
		if handlerErr != nil {
			return handlerErr
		}
	}

	// Run thunk in a sub-context with the prompt tag set on the context.
	// The prompt tag marks this sub-context as a prompt boundary. When
	// call-with-composable-continuation searches for the prompt, it finds
	// it on the context. When abort propagates, it reaches this primitive
	// which catches it.
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	sub.SetPromptTag(tag)

	_, err = sub.ApplyCallable(thunkCls)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
		var abortErr *machine.ErrPromptAbort
		if errors.As(err, &abortErr) {
			if abortErr.Tag != tag {
				// Not our prompt - propagate
				return err
			}
			// Unwind dynamic-wind extents in the sub-context before handling.
			if sub.WindingStack().Depth() > mc.WindingStack().Depth() {
				unwindErr := sub.UnwindTo(mc.WindingStack().Depth())
				if unwindErr != nil {
					return unwindErr
				}
			}
			// This abort targets our prompt. Invoke the handler.
			if handlerCls != nil {
				handlerSub := mc.NewSubContext()
				defer machine.ReleaseSubContext(handlerSub)
				_, applyErr := handlerSub.ApplyCallable(handlerCls, abortErr.Values...)
				if applyErr != nil {
					return applyErr
				}
				runErr := handlerSub.Run()
				if runErr != nil {
					return runErr
				}
				mc.SetValues(handlerSub.GetValues()...)
				return nil
			}
			// No handler: return first abort value
			if len(abortErr.Values) > 0 {
				mc.SetValue(abortErr.Values[0])
			} else {
				mc.SetValue(values.Void)
			}
			return nil
		}
		return err
	}

	mc.SetValues(sub.GetValues()...)
	return nil
}

// PrimAbortCurrentContinuation aborts to the nearest prompt with the given tag.
//
// (abort-current-continuation tag v ...)
//
// Returns an ErrPromptAbort that propagates up through Run() to the
// enclosing call-with-continuation-prompt or RunWithEscapeHandling.
func PrimAbortCurrentContinuation(mc machine.CallContext) error {
	tag, err := helpers.RequireArg[*machine.PromptTag](mc, 0, werr.ErrNotAPromptTag, "abort-current-continuation")
	if err != nil {
		return err
	}
	restVal := mc.Arg(1)

	var abortValues []values.Value
	if !values.IsEmptyList(restVal) {
		current := restVal
		for !values.IsEmptyList(current) {
			tuple, ok := current.(values.Tuple)
			if !ok {
				return werr.WrapForeignErrorf(werr.ErrNotAList, "abort-current-continuation: improper argument list")
			}
			abortValues = append(abortValues, tuple.Car())
			current = tuple.Cdr()
		}
	}

	return &machine.ErrPromptAbort{
		Tag:    tag,
		Values: abortValues,
	}
}

// PrimContinuationPromptAvailableQ tests whether a prompt with the given tag
// is on the current continuation chain.
//
// (continuation-prompt-available? tag) -> boolean
//
// Walks the continuation chain using FindPrompt, which checks both
// continuation frames and the context-level prompt.
//
// Racket §10.4: continuation-prompt-available?
func PrimContinuationPromptAvailableQ(cc machine.CallContext) error {
	mc := cc.(*machine.MachineContext)
	tag, err := helpers.RequireArg[*machine.PromptTag](mc, 0, werr.ErrNotAPromptTag, "continuation-prompt-available?")
	if err != nil {
		return err
	}
	_, found := mc.FindPrompt(tag)
	mc.SetValue(values.BoolToBoolean(found))
	return nil
}

// PrimCallWithComposableContinuation captures a composable (delimited)
// continuation up to the nearest prompt with the given tag, then invokes
// proc with that continuation as its argument.
//
// (call-with-composable-continuation proc tag)
//
// The captured continuation is a ComposableContinuation value that, when
// applied, splices its frames onto the current continuation chain.
//
// Follows Racket's call-with-composable-continuation.
func PrimCallWithComposableContinuation(cc machine.CallContext) error {
	mc := cc.(*machine.MachineContext)
	proc := mc.Arg(0)
	tagVal := mc.Arg(1)

	procCls, err := helpers.RequireType[machine.Closure](proc, werr.ErrNotAProcedure, "call-with-composable-continuation")
	if err != nil {
		return err
	}

	tag, err := helpers.RequireType[*machine.PromptTag](tagVal, werr.ErrNotAPromptTag, "call-with-composable-continuation")
	if err != nil {
		return err
	}

	// Find the prompt in the continuation chain or on the context itself.
	prompt, found := mc.FindPrompt(tag)
	if !found {
		return werr.WrapForeignErrorf(werr.ErrNotAPromptTag, "call-with-composable-continuation: no prompt found for tag %s", tag.SchemeString())
	}

	// Slice the continuation chain from mc.cont to the prompt.
	// prompt is nil when the prompt is on the context boundary (sub-context level).
	segment := mc.SliceContinuationAt(prompt)

	// Capture the current winding stack
	windingStack := mc.WindingStack().Copy()

	// Create the composable continuation value, recording the barrier context at
	// capture time so applyComposableContinuation can detect barrier crossings.
	comp := machine.NewComposableContinuation(segment, windingStack, mc.ThreadID(), mc.BarrierValid())

	// Call proc with the composable continuation.
	// The result of proc becomes the value delivered to the prompt boundary.
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	_, err = sub.ApplyCallable(procCls, comp)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
		return err
	}

	// Abort to the prompt with the proc's result.
	// This skips past the captured frames in the current context,
	// delivering the result directly to the prompt boundary.
	return &machine.ErrPromptAbort{
		Tag:    tag,
		Values: []values.Value{sub.GetValue()},
	}
}
