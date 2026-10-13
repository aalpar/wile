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

import "sync/atomic"

// boundaryToken identifies one context's boundary, the place a call/cc
// continuation is delimited at when no prompt frame delimits it: the top level,
// a thread root, a sub-context running a dynamic-wind thunk, eval or load, or an
// expansion-time root (a transformer body, a library body, the bootstrap). The
// driver whose context holds the token resolves a resume of the continuation,
// after the nearest default-tag frame on the invoking chain and before a root
// (resumePrompt).
//
// Every call/cc capture also records the token of its context chain's ROOT: the
// nearest context driven by RunResumable (its promptTag says so), or the
// parentless context the chain ends at. An expansion-time root is parentless and
// driven by RunWithinBoundary (MachineContext.expansionRoot), but may well have a
// runtime driver above it on the Go stack (an eval expanding a transformer); the
// root token tells it what it may resolve (a continuation captured under itself,
// or under an expansion-time root whose run has ended) from a continuation of a
// runtime root's, live or stale, which it passes up.
//
// Pointer identity, as BarrierToken. The context's field is zeroed with the
// struct on release, so a reused struct cannot match a continuation captured on
// its previous occupant; the token itself outlives the context in every
// continuation that carries it, which is what the two flags are for.
type boundaryToken struct {
	// closed is set when the run of the context the token names is over: an
	// expansion-time root's when its driver returns, any context's when it is
	// released to the pool. Read only on a root token, and only by an
	// expansion-time root (resumePrompt). Atomic because a macro context carries
	// no thread identity, so a continuation captured on one goroutine's
	// expansion can be invoked, and the token read, on another's; the atomic
	// keeps that read defined, it does not make the cross-thread resume safe
	// (TODO.md: stamp thread identity on expansion contexts).
	closed atomic.Bool
	// runtime records which driver drove the context the token was minted on,
	// not the phase of its code; read only on a root token. False on an
	// expansion-time root (a context RunWithinBoundary drives with no parent: a
	// transformer body, a library body instantiated at expansion time whatever
	// its phase, the bootstrap), true on every other context's (the top level, a
	// thread root, an Engine.Call). Copied from expansionRoot at the first mint,
	// which every driven root reaches after its driver's entry; a Go transformer
	// (the ForeignClosure arm of invokeTransformerClosure) enters no driver, so a
	// capture it ran in a sub-context would read as a runtime one.
	runtime bool
}

// boundaryIdentity returns p's boundary token, allocating it on first use.
func (p *MachineContext) boundaryIdentity() *boundaryToken {
	if p.boundary == nil {
		p.boundary = &boundaryToken{runtime: !p.expansionRoot}
	}
	return p.boundary
}

// rootContext returns the root of p's context chain, p included: the nearest
// context RunResumable drives, else the context with no parent. It is both where
// a capture with no default prompt of its own is delimited (ResumeWindingDepth)
// and the context whose token names the capture's owner (rootIdentity), so the
// two cannot disagree.
func (p *MachineContext) rootContext() *MachineContext {
	q := p
	for q.parentMC != nil && q.promptTag != DefaultPromptTag {
		q = q.parentMC
	}
	return q
}

// rootIdentity returns the boundary token of the root of p's context chain.
func (p *MachineContext) rootIdentity() *boundaryToken {
	return p.rootContext().boundaryIdentity()
}

// closeBoundary marks p's boundary gone, for the continuations still delimited
// at or under it. Called by the pool's release paths before the struct is
// zeroed, and by RunWithinBoundary when an expansion-time root's run ends: the
// expander keeps a transformer's context alive while it expands the
// transformer's output, and a macro in that output may invoke a continuation
// captured in the body, which is stale by then.
func closeBoundary(p *MachineContext) {
	if p.boundary != nil {
		p.boundary.closed.Store(true)
	}
}

// DelimitAt records where comp, a call/cc capture sliced on p, was delimited:
// at prompt, a frame on p's chain found by FindPrompt(DefaultPromptTag), or at
// p's own boundary when prompt is nil. Either way it also records the root of
// p's context chain. One call per capture, from call/cc; a composable
// continuation (call-with-composable-continuation, the timer and break
// interrupts) is applied in place and never carries either token.
func (p *MachineContext) DelimitAt(comp *ComposableContinuation, prompt *MachineContinuation) {
	if prompt == nil {
		comp.boundary = p.boundaryIdentity()
	}
	comp.root = p.rootIdentity()
}
