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
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
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
	mc, err := machine.RequireMachineContext(cc, "call-with-continuation-prompt")
	if err != nil {
		return err
	}
	thunk := mc.Arg(0)
	tagVal := mc.Arg(1)
	handlerVal := mc.Arg(2)

	thunkCls, err := helpers.RequireCallable(thunk, "call-with-continuation-prompt")
	if err != nil {
		return err
	}

	tag, err := helpers.RequireType[*machine.PromptTag](tagVal, werr.ErrNotAPromptTag, "call-with-continuation-prompt")
	if err != nil {
		return err
	}

	var handlerCls values.Callable
	if handlerVal != values.FalseValue {
		var handlerErr error
		handlerCls, handlerErr = helpers.RequireCallable(handlerVal, "call-with-continuation-prompt")
		if handlerErr != nil {
			return handlerErr
		}
	}

	// Reify the prompt as a continuation-CHAIN frame run inline: push a transparent
	// prompt frame carrying tag (+ handler) onto mc.cont and inline-apply thunk on the
	// live chain. On normal return thunk's value(s) flow through the transparent frame
	// unchanged. An abort to tag is routed by the driver's FindPrompt to this frame —
	// the driver reconciles dynamic-wind, then invokes the handler (if any) with the
	// abort values or delivers them as the prompt's result. A continuation captured
	// inside thunk spans this frame, and call-with-composable-continuation's
	// FindPrompt(tag) now finds the CHAIN FRAME so SliceContinuationAt(frame) delimits
	// at it. (Replaces the old sub-context + context-level SetPromptTag + Go-stack
	// errors.As catch, which truncated any continuation captured inside thunk.)
	_, err = mc.RunBodyUnderPrompt(thunkCls, tag, handlerCls)
	return err
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
	mc, err := machine.RequireMachineContext(cc, "continuation-prompt-available?")
	if err != nil {
		return err
	}
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
// Follows Racket's call-with-composable-continuation: like call/cc but (a) proc
// runs IN PLACE — the current continuation is NOT removed, so proc's result flows
// through the live delimited frames — and (b) the captured ComposableContinuation
// is non-abortive: applying it COMPOSES (extends) the current continuation rather
// than replacing it, so the captured frames may run more than once. This is the
// raw composable capture; shift/control add their own abort on top (wile/control.scm).
func PrimCallWithComposableContinuation(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "call-with-composable-continuation")
	if err != nil {
		return err
	}
	proc := mc.Arg(0)
	tagVal := mc.Arg(1)

	procCls, err := helpers.RequireCallable(proc, "call-with-composable-continuation")
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

	// Apply proc with the composable continuation, mirroring call/cc's two modes
	// (prim_control.go:188-221) — NOT an abort. Racket's
	// call-with-composable-continuation runs proc IN PLACE (the current continuation is
	// not removed) and applying the continuation COMPOSES (extends), so proc's result
	// flows through the live delimited frames and (comp v) inside proc splices the
	// captured copy onto the live chain (applyComposableContinuation). Verified against
	// Racket v9.2: (+ 100 (cwcc (lambda (k) (k 5)) t)) => 205, (* 2 (cwcc (lambda (k)
	// (+ 1 (k 5))) dflt)) => 22 — compose-in-place, not the old control/frame-removing
	// abort. This also fixes the sub-context truncation: a call/cc captured inside proc
	// now spans the live chain.
	// Single apply seam, mirroring call/cc (prim_control.go): capture is shared,
	// mode is one target selection, not two hand-written apply arms (staff-sweep
	// #9). target is the ambient context when a live chain exists (proc runs in
	// place, composing), or a fresh sub-context when rootless.
	target := mc
	if mc.Parent() == nil {
		// Rootless cwcc (inside another foreign sub-context or a thread root): run
		// proc under its OWN DefaultPromptTag driver, exactly as call/cc's
		// sub-context mode.
		sub := mc.NewSubContext()
		defer machine.ReleaseSubContext(sub)
		target = sub
	}

	_, err = target.ApplyCallable(procCls, comp)
	if err != nil {
		return err
	}

	if target != mc {
		// RunWithEscapeHandling (not RunWithinBoundary) so a call/cc or reified
		// boundary inside proc has a driver, then deliver proc's value(s) to mc.
		err = target.RunWithEscapeHandling()
		if err != nil {
			return err
		}
		mc.SetValues(target.GetValues()...)
	}
	return nil
}
