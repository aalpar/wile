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

package machine

// Boundary reification substrate for the continuation-resume cluster.
//
// These helpers replace the "sub-context + sub.Run() + Go-catch" pattern that
// truncates captured continuations with a continuation-CHAIN frame run INLINE on
// the live context. A continuation captured inside the body then spans the frame
// and everything below it; an abort to a frame-carried tag routes through the chain
// (FindPrompt) to the nearest driver (RunResumable at the top, RunWithinBoundary in
// a kept sub-context), not to a Go-stack errors.As catch.
//
// PROVEN GROUNDWORK — NOT WIRED (2026-06-28). The full boundary cluster was
// implemented on these helpers (RunBodyUnderConsumer→call-with-values,
// RunBodyUnderPrompt→call-with-continuation-prompt, RunBodyUnderExitFrame→
// call-with-exit, RunWithinBoundary→barrier/RaiseInPlace/composable/sweep) and the
// genuine fixes were CONFIRMED by an A/B crosscheck (truncation fixed; marks survive
// call-with-exit; call/cc delimiting fixed). But the SAME crosscheck found 4 CRITICAL
// regressions + `make ci` RED, all from one root: reification makes boundaries depend
// on driver routing + winding-from-the-escape-point, yet four paths still run
// boundary-bearing code under plain Run()/non-reconciling re-raise (the resume sub;
// the dynamic-wind thunks on the FORWARD-escape path; the deeper-sub winding the
// exit/RunWithinBoundary re-raise drops; the RaiseInPlace handler-mark dropped by
// RunBodyUnderFrame's p.cont mutation). Routing them all overflows ctak under the kept
// nest-then-abort resume. CONCLUSION: the reification and the winding-aware resume
// (the flip) are INSEPARABLE — one atomic change. The wiring was reverted; these
// helpers stay as proven substrate for the unified reification+flip change. Full spec
// (the 4 CRITICALs + root paths) in
// plans/2026-06-28-continuation-cluster-reification-impl.local.md and memory
// continuation-cwv-reification-validated-coupling-mapped.

import "github.com/aalpar/wile/pkg/values"

// returnTemplate is a one-instruction template whose sole opcode returns control
// to the executing frame's parent, leaving the value register untouched. A
// synthetic prompt frame (RunBodyUnderPrompt) carries it so that:
//
//   - On NORMAL completion of the body, the body's result — single OR multiple
//     values, since RestoreAndRelease/Restore never touch the value register —
//     flows transparently through the prompt frame to its parent, exactly as if
//     the prompt were not there.
//   - On ABORT to the prompt's tag, RunResumable restores this frame and the same
//     OpRestoreContinuation delivers the abort values to the parent.
//
// INVARIANT: returnTemplate MUST remain this single instruction and MUST NOT be
// mutated — it is shared by every prompt frame.
var returnTemplate = &NativeTemplate{
	code: []Instruction{{Op: OpRestoreContinuation}},
}

// applyToValuesCode is the body of an apply-frame: a frame that applies a stored
// closure (literal 0) to the 0/1/N values its body left live in the value register.
// It runs when the body returns normally:
//
//	OpPush          — spread the body's 0/1/N values onto the eval stack
//	                  (pushValueRegisterTo is multiple-values-aware)
//	OpLoadLiteral 0 — load the applied closure (literal 0) into the value register
//	OpApply         — apply that closure to the spread args, draining the stack
//
// Its result then flows to the frame's parent. Two callers:
//   - call-with-values: literal = the consumer; no prompt tag.
//   - call-with-exit:   literal = a finalizer closure that clears the one-shot and
//     forwards the value(s); a prompt tag so an (exit v) abort routes here.
//
// Either way a continuation captured inside the body spans this chain-resident
// frame, so re-entry replays it instead of truncating.
//
// INVARIANT: shared, read-only — the VM never mutates template code.
var applyToValuesCode = []Instruction{
	{Op: OpPush},
	{Op: OpLoadLiteral, Arg: 0},
	{Op: OpApply},
}

// RunBodyUnderFrame pushes frame as mc.cont and inline-applies body on this
// context (no sub-context, no nested Run), so body runs ON THE LIVE CHAIN under
// frame: O(1) Go frames, and a continuation captured in body spans frame and
// everything below it. frame.parent MUST already be the current p.cont (the
// continuation constructors used by the callers below set it).
//
// frame is either a transparent prompt frame (RunBodyUnderPrompt) or a plain
// continuation frame whose code runs on the body's normal return
// (RunBodyUnderConsumer). On normal completion the VM restores frame and
// executes its template; on abort to a frame-carried tag the driver's FindPrompt
// routes to it.
func (p *MachineContext) RunBodyUnderFrame(frame *MachineContinuation, body values.Value, args ...values.Value) (*MachineContext, error) {
	frame.threadID = p.threadID
	if len(p.windingStack) > 0 {
		frame.windingStack = p.windingStack.Copy()
	}
	// Carry the caller's live activation marks onto the pushed frame so they survive
	// the frame's restore. The body runs in the caller's dynamic extent and keeps
	// p.marks intact (Apply does not touch them); but when the frame is later restored
	// — to run a consumer (call-with-values) or finalizer (call-with-exit), or to
	// resolve a re-raise inside an inline exception handler (RaiseInPlace) — mc.marks is
	// reloaded from the frame. Without this, a mark on the live activation (e.g. the
	// %exception-handlers mark a tail-position call-with-values inherits, or the
	// parent-handler mark RaiseInPlace installs) would be dropped at that boundary,
	// leaving the consumer/handler unable to see the current exception handler.
	if len(p.marks) > 0 {
		frame.marks = cloneMarks(p.marks)
	}
	p.cont = frame
	return p.ApplyCallable(body, args...)
}

// RunBodyUnderPrompt installs a continuation-chain prompt frame on top of the
// current chain and applies body INLINE on this context, so the body runs on the
// live continuation. A continuation captured inside body spans the prompt frame
// and everything below it; an abort to tag routes through the chain (FindPrompt)
// to this frame's handler in RunResumable.
//
// The frame is transparent on normal completion (returnTemplate). tag identifies
// the prompt for abort/capture. handler may be nil: nil means the abort values
// become the prompt's result delivered to the parent (call-with-continuation-prompt
// with a #f handler); a non-nil handler is invoked by the driver with the abort
// values (call-with-continuation-prompt). call-with-exit does NOT use this wrapper —
// it needs a non-transparent finalizer frame (RunBodyUnderExitFrame).
func (p *MachineContext) RunBodyUnderPrompt(body values.Value, tag *PromptTag, handler Closure, args ...values.Value) (*MachineContext, error) {
	frame := NewMachineContinuationWithPrompt(p.cont, returnTemplate, p.env, tag, handler)
	return p.RunBodyUnderFrame(frame, body, args...)
}

// runBodyUnderApplyFrame pushes an apply-frame (applyToValuesCode) whose literal is
// `applied`, then inline-applies body (with args) on the live chain. When tag is
// non-nil the frame is a prompt frame carrying tag (so an abort to tag routes here);
// when nil it is a plain frame. `applied` rides as literal 0 of a per-call template
// so it travels in a captured chain via Copy() (which shares the template pointer).
func (p *MachineContext) runBodyUnderApplyFrame(body values.Value, applied values.Value, tag *PromptTag, args ...values.Value) (*MachineContext, error) {
	tpl := &NativeTemplate{
		code:     applyToValuesCode,
		literals: MultipleValues{applied},
	}
	var frame *MachineContinuation
	if tag != nil {
		frame = NewMachineContinuationWithPrompt(p.cont, tpl, p.env, tag, nil)
	} else {
		frame = NewMachineContinuation(p.cont, tpl, p.env)
	}
	return p.RunBodyUnderFrame(frame, body, args...)
}

// RunBodyUnderConsumer reifies call-with-values. It pushes a consumer apply-frame
// and inline-applies producer on the live chain, so a continuation captured inside
// producer spans the consumer frame and the rest of the program (fixing the
// sub-context producer truncation). On producer's normal return the consumer frame
// applies consumer to the produced values exactly once; a full call/cc continuation
// invoked inside producer aborts to DefaultPromptTag, discarding the chain-resident
// consumer frame (escape-past preserved).
func (p *MachineContext) RunBodyUnderConsumer(producer values.Value, consumer values.Value) (*MachineContext, error) {
	return p.runBodyUnderApplyFrame(producer, consumer, nil)
}

// RunBodyUnderExitFrame reifies call-with-exit. It pushes a prompt apply-frame
// carrying tag whose literal is `finalizer`, then inline-applies body (proc) with
// exitArg (the one-shot exit closure). On proc's normal return the frame applies
// finalizer to proc's value(s) — clearing the one-shot and forwarding the value(s);
// an (exit v) abort to tag routes via the driver's FindPrompt to this frame (no
// handler), which delivers v and re-enters the same finalizer template (idempotent).
// The frame is NOT transparent (returnTemplate): the one-shot must be cleared on the
// normal-return path, which RunBodyUnderPrompt's returnTemplate cannot do.
func (p *MachineContext) RunBodyUnderExitFrame(body values.Value, tag *PromptTag, finalizer values.Value, exitArg values.Value) (*MachineContext, error) {
	return p.runBodyUnderApplyFrame(body, finalizer, tag, exitArg)
}
