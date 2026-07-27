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
// WIRED (2026-06-28, feat/continuation-resume-trampoline). The full boundary cluster
// runs on these helpers as the live continuation path:
//   - RunBodyUnderConsumer → call-with-values        (prim_control.go)
//   - RunBodyUnderExitFrame → call-with-exit         (prim_exit.go)
//   - RunBodyUnderPrompt    → call-with-continuation-prompt  (prim_prompt.go)
//   - RunBodyUnderFrame     → raise-continuable and the non-continuable raise
//                             escalator                       (exception_raise.go)
//   - RunWithinBoundary     → barrier / RaiseInPlace handler / composable / the sweep
//                             sites (machine_context.go and call sites)
//
// An earlier A/B crosscheck proved the reification and the winding-aware resume (the
// "flip") are INSEPARABLE — reified boundaries depend on driver routing and winding
// reconciled from the escape point, and the kept nest-then-abort resume overflowed
// ctak. So both landed together as one atomic change: resume now runs on the driver
// (RunResumable), giving O(1) Go frames and a single winding reconcile. Full history
// (the 4 CRITICALs that forced the atomic landing, and the later crosscheck that
// caught the sticky-isolatedMarks escalation bug) in
// plans/2026-06-28-continuation-cluster-reification-impl.local.md and memory
// continuation-cwv-reification-validated-coupling-mapped.

import "github.com/aalpar/wile/pkg/values"

// returnTemplate is a one-instruction template whose sole opcode returns control
// to the executing frame's parent, leaving the value register untouched. A
// transparent chain frame carries it (RunBodyUnderPrompt's prompt frame,
// RunBodyUnderBarrier's barrier frame, raise-continuable's frame) so that:
//
//   - On NORMAL completion of the body, the body's result — single OR multiple
//     values, since RestoreAndRelease/Restore never touch the value register —
//     flows transparently through the prompt frame to its parent, exactly as if
//     the prompt were not there.
//   - On ABORT to the prompt's tag, RunResumable restores this frame and the same
//     OpRestoreContinuation delivers the abort values to the parent.
//
// INVARIANT: returnTemplate MUST remain this single instruction and MUST NOT be
// mutated: it is shared by every transparent frame.
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
// Its result then flows to the frame's parent. Callers:
//   - call-with-values (via RunBodyUnderConsumer): literal = the consumer; no prompt tag.
//   - make-parameter (via RunBodyUnderConsumer): literal = a finalizer that wraps the
//     converted value in a Parameter; no prompt tag.
//   - call-with-exit (via RunBodyUnderExitFrame): literal = a finalizer closure that
//     clears the one-shot and forwards the value(s); a prompt tag so an (exit v) abort
//     routes here.
//   - with-timeout (via RunBodyUnderTimer): literal = the idempotent teardown finalizer;
//     a prompt tag so a fired timer routes here.
//   - the non-continuable raise escalator (exception_raise.go), which builds its own
//     template directly on this code: literal = the escalator; no prompt tag.
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
// frame comes in three shapes:
//
//   - a transparent prompt frame (RunBodyUnderPrompt): carries a tag, and
//     returnTemplate passes the body's value(s) straight through on normal return;
//   - a plain continuation frame (RunBodyUnderConsumer, raise-continuable, the raise
//     escalator): no tag, so it is not an abort target, and its code runs on normal
//     return;
//   - a non-transparent prompt frame (RunBodyUnderExitFrame, RunBodyUnderTimer): carries
//     a tag AND runs a finalizer template on normal return, so it is reached both ways.
//
// On normal completion the VM restores frame and executes its template; on abort to a
// frame-carried tag the driver's FindPrompt routes to it.
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
	// The reified frame inherits the current barrier so a continuation captured in the
	// body records it (crossing detection still fires) and the frame's restore reverts it.
	frame.barrierValid = p.barrierValid
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

// RunBodyUnderConsumer reifies a "run a body, then apply a consumer to its result"
// boundary on the live chain. It pushes a consumer apply-frame and inline-applies
// producer (to args) on the live chain, so a continuation captured inside producer
// spans the consumer frame and the rest of the program (fixing the sub-context
// producer truncation). On producer's normal return the consumer frame applies
// consumer to the produced values exactly once; a full call/cc continuation invoked
// inside producer aborts to DefaultPromptTag, discarding the chain-resident consumer
// frame (escape-past preserved).
//
// call-with-values passes no args — its producer is a thunk. make-parameter passes
// the init value as the single arg to the user converter (the producer), with the
// consumer being a finalizer that wraps the converted value in a Parameter. The args
// path is the same one RunBodyUnderExitFrame already exercises.
func (p *MachineContext) RunBodyUnderConsumer(producer values.Value, consumer values.Value, args ...values.Value) (*MachineContext, error) {
	return p.runBodyUnderApplyFrame(producer, consumer, nil, args...)
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

// RunBodyUnderBarrier reifies with-continuation-barrier. It pushes a transparent chain
// frame carrying the OUTER barrier token (the restore target), flips the live barrier to
// `token` for the body, and inline-applies body on the live chain (no sub-context). On
// the body's normal return the transparent frame's restore reverts p.barrierValid to the
// outer token and forwards the body's value(s); a continuation captured inside the body
// spans the frame, so resume-out replays the same revert. An abort skips the frame and
// lands on an outer frame whose stamped barrierValid reverts via the driver's Restore.
// Because barrierValid is now a chain-carried register, the crossing check at the (k v)
// site (applyCapturedContinuation / applyComposableContinuation) is unchanged and correct
// at every transition: a continuation captured inside records `token`; one captured
// outside records the outer barrier; invoking across the boundary trips the pointer check.
func (p *MachineContext) RunBodyUnderBarrier(body values.Value, token *BarrierToken) (*MachineContext, error) {
	frame := NewMachineContinuation(p.cont, returnTemplate, p.env)
	frame.threadID = p.threadID
	if len(p.windingStack) > 0 {
		frame.windingStack = p.windingStack.Copy()
	}
	if len(p.marks) > 0 {
		frame.marks = cloneMarks(p.marks)
	}
	frame.barrierValid = p.barrierValid // outer token = restore target on frame exit
	p.cont = frame
	p.barrierValid = token // flip the live barrier for the body
	return p.ApplyCallable(body)
}
