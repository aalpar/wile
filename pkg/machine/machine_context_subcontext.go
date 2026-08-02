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

import (
	"context"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// mutableRuntimeFor resolves the mutable runtime global for a sub-context of p. It
// tries p's lexical env chain first (the common case: one frame, with a namespace),
// then falls back along the parentMC chain — necessary when p.env is a detached
// transient frame (nil parent, nil namespace), whose owning namespace is reachable
// only through the synchronous parent context.
//
// If NO context in the chain carries a namespace, panic rather than degrade silently:
// the environment package's stated policy is "no legacy fallback — all environments
// MUST have a Namespace" (environment/CLAUDE.local.md), and the prior direct
// p.env.MutableRuntime() panicked on this case. Returning a detached env here would
// silently land user defines in the wrong global. The recover at the VM boundary
// (RunWithEscapeHandling) contains the panic as a returned error.
func mutableRuntimeFor(p *MachineContext) *environment.EnvironmentFrame {
	for c := p; c != nil; c = c.parentMC {
		rt := c.env.MutableRuntimeOrNil()
		if rt != nil {
			return rt
		}
	}
	panic(werr.WrapForeignErrorf(werr.ErrInternal,
		"mutableRuntimeFor: no namespace reachable via env chain or parentMC"))
}

// NewSubContext creates a new MachineContext for running sub-calls (e.g., apply, map, for-each).
// The sub-context shares the global environment but has a fresh call stack, eval stack, and value register.
// This allows foreign functions to call Scheme closures without corrupting the parent context's state.
//
// The parent's dynamic-wind winding stack is inherited automatically so that continuations
// captured inside the sub-context preserve the enclosing dynamic-wind context. For the rare
// sites that need a different stack (unwind truncation, exception cleanup), use
// NewSubContextWithWinding.
//
// Note: Sub-contexts have isolated continuation chains (cont = nil). When call/cc captures a
// continuation inside a sub-context, it captures mc.Parent() which refers to the sub-context's
// chain (nil). For continuations to escape back to the outer context, the escape error propagates
// up through the call stack and is handled by RunWithEscapeHandling at the top level.
//
// The parentMC field tracks the parent context, allowing call/cc to find an outer continuation
// for proper R7RS continuation semantics when captured inside sub-contexts.
//
// The escapeCont field is inherited, allowing nested sub-contexts to know where execution
// should continue after their completion (set by dynamic-wind and similar constructs).
func (p *MachineContext) NewSubContext() *MachineContext {
	p.counters.SubContextsCreated++
	mc := acquireSubContext()
	mc.ctx = p.ctx
	mc.pools = p.pools // same goroutine: share the parent thread's freelists
	// envPooled: zero value (false) — sub-context env is top-level, not from pool.
	// Capture the mutable runtime: load/eval/call-with-exit run here and their defines
	// must land in the user-visible mutable global (it reaches the sealed base via its
	// parent walk). p.env is usually a namespace-carrying frame, but some transient
	// execution frames are detached (nil parent, nil namespace) — e.g. a procedure
	// body entered while running a call-with-values producer, the site RaiseInPlace
	// dispatches a handler from. There the namespace is reachable only via parentMC,
	// so fall back along that chain rather than dereferencing a nil namespace.
	mc.env = mutableRuntimeFor(p)
	mc.evals = mc.acquireStack()
	mc.threadID = p.threadID
	mc.parentMC = p
	mc.escapeCont = p.escapeCont
	mc.thread = p.thread
	mc.maxCallDepth = p.maxCallDepth
	mc.maxStackSize = p.maxStackSize
	mc.barrierValid = p.barrierValid // inherit barrier context
	mc.windingStack = p.windingStack
	return mc
}

// NewSubContextWithWinding creates a sub-context with an explicit winding stack
// instead of inheriting the parent's. Use this only when the sub-context must run
// with a winding stack that differs from the parent — for example, a truncated
// stack during unwind (machine_context_winding.go) or exception cleanup
// (prim_exceptions.go). All other sub-context creation should use NewSubContext.
func (p *MachineContext) NewSubContextWithWinding(windingStack WindingStack) *MachineContext {
	mc := p.NewSubContext()
	mc.windingStack = windingStack
	return mc
}

// NewSubContextWithTemplate creates a sub-context configured to execute the
// given template in the given environment. This is the correct way for
// primitives like eval and load to create execution contexts — it propagates
// all parent fields automatically via NewSubContext, preventing the "forgotten
// field" bug class that NewMachineContext + manual setters is vulnerable to.
//
// The template and env override NewSubContext's defaults (nil template, the
// parent's mutable runtime global). pc starts at 0 (pool zero-value).
func (p *MachineContext) NewSubContextWithTemplate(
	tpl *NativeTemplate,
	env *environment.EnvironmentFrame,
) *MachineContext {
	mc := p.NewSubContext()
	mc.template = tpl
	mc.env = env
	return mc
}

// SubContextParams holds the parent state copied into a thread's sub-context
// across the goroutine boundary. It deliberately carries NO pointer to the parent
// MachineContext: a thread runs CONCURRENTLY with its parent, so a live parentMC
// link would let the thread's parentMC walks (CaptureStackTrace,
// findParameterInMarks, the subContextPool release counter) read the parent's
// still-mutating VM fields — an unsynchronized data race (see TODO.md Tier 1;
// reproduced by TestMutexAbandonedOnTermination under -race). Only values safe to
// snapshot once at spawn time are captured here.
type SubContextParams struct {
	Ctx          context.Context
	Env          *environment.EnvironmentFrame
	EscapeCont   *MachineContinuation
	MaxCallDepth int
	MaxStackSize uint64
	WindingStack WindingStack
}

// CaptureSubContextParams extracts the state needed to create a sub-context in a different goroutine.
// This is used by thread creation to avoid race conditions when accessing the parent MachineContext
// from a child goroutine (T4 from architectural review).
//
// Call this in the parent goroutine before creating the child goroutine, then pass the result
// to NewThreadSubContext in the child goroutine.
func (p *MachineContext) CaptureSubContextParams() SubContextParams {
	return SubContextParams{
		Ctx: p.ctx,
		// SRFI-18 threads share the engine's one mutable runtime global (and see the
		// sealed base through its parent). MutableRuntime() captures that shared global;
		// capturing the sealed base instead would corrupt shared state.
		Env:          p.env.MutableRuntime(),
		EscapeCont:   p.escapeCont,
		MaxCallDepth: p.maxCallDepth,
		MaxStackSize: p.maxStackSize,
		// Copy, not the bare header. A header carries the parent's spare
		// CAPACITY, and WindingStack.Pop deliberately retains capacity, so after
		// any completed dynamic-wind the parent sits at len < cap and a shared
		// header hands the child a writable alias of the parent's backing array.
		// Both then push into the same slot, concurrently, across a goroutine
		// boundary — reproduced under -race from ordinary Scheme (a dynamic-wind
		// to seed capacity, then thread-start!, then winding on both sides).
		//
		// Capping instead of copying is NOT enough: the child's first push would
		// reallocate, but until then it still READS indices the parent can pop
		// and overwrite. Copy is also what all seven continuation-capture sites
		// already do; this one was the holdout, and it is the only one crossing
		// a goroutine boundary. A thread spawn is nowhere near hot enough to
		// care about one clone.
		WindingStack: p.windingStack.Copy(),
	}
}

// NewThreadSubContext creates a sub-context for a thread using previously captured parent state.
// Unlike NewSubContext, this doesn't access the parent MachineContext fields, making it safe to call
// from a different goroutine. The thread parameter should be the new thread object, which provides
// the thread identity for the new context.
//
// This function is specifically designed for SRFI-18 thread creation. For other uses of sub-contexts
// (like map, for-each, dynamic-wind), use NewSubContext instead.
func NewThreadSubContext(params SubContextParams, thread *values.Thread) *MachineContext {
	sub := &MachineContext{
		ctx: params.Ctx,
		vmState: vmState{
			env:          params.Env,
			evals:        NewStack(),
			windingStack: params.WindingStack,
			// threadID will be set by SetThread below
		},
		// parentMC is intentionally nil (zero value): a thread is an independent
		// root, NOT a synchronous sub-context. Severing it stops every parentMC walk
		// (stack trace, parameter marks, pool counter) at the thread boundary, which
		// is what makes them race-free across the goroutine boundary — and is also
		// the correct semantics (a thread's stack trace is its own, and SRFI-18
		// parameter inheritance is a creation-time snapshot, not a live link to the
		// concurrently-evolving parent). See SubContextParams above.
		escapeCont:   params.EscapeCont,
		maxCallDepth: params.MaxCallDepth,
		maxStackSize: params.MaxStackSize,
		pools:        newThreadPools(), // new goroutine: its own freelists
		// thread will be set by SetThread below
	}
	sub.SetThread(thread) // Sets both thread object and threadID from thread.ID()
	return sub
}
