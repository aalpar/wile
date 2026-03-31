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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
)

// NewSubContext creates a new MachineContext for running sub-calls (e.g., apply, map, for-each).
// The sub-context shares the global environment but has a fresh call stack, eval stack, and value register.
// This allows foreign functions to call Scheme closures without corrupting the parent context's state.
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
	// envPooled: zero value (false) — sub-context env is top-level, not from pool.
	mc.env = p.env.TopLevel()
	mc.evals = acquireStack()
	mc.threadID = p.threadID
	mc.parentMC = p
	mc.escapeCont = p.escapeCont
	mc.thread = p.thread
	mc.exceptionHandler = p.exceptionHandler
	mc.maxCallDepth = p.maxCallDepth
	mc.windingStack = p.windingStack // inherit dynamic-wind extent
	mc.barrierValid = p.barrierValid // inherit barrier context
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

// SubContextParams holds the parent state needed to create a thread's sub-context.
// This is used to avoid race conditions when creating sub-contexts across goroutine boundaries.
type SubContextParams struct {
	Ctx              context.Context
	Env              *environment.EnvironmentFrame
	ParentMC         *MachineContext
	EscapeCont       *MachineContinuation
	ExceptionHandler *ExceptionHandler
	MaxCallDepth     uint64
}

// CaptureSubContextParams extracts the state needed to create a sub-context in a different goroutine.
// This is used by thread creation to avoid race conditions when accessing the parent MachineContext
// from a child goroutine (T4 from architectural review).
//
// Call this in the parent goroutine before creating the child goroutine, then pass the result
// to NewThreadSubContext in the child goroutine.
func (p *MachineContext) CaptureSubContextParams() SubContextParams {
	return SubContextParams{
		Ctx:              p.ctx,
		Env:              p.env.TopLevel(),
		ParentMC:         p,
		EscapeCont:       p.escapeCont,
		ExceptionHandler: p.exceptionHandler,
		MaxCallDepth:     p.maxCallDepth,
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
			env:   params.Env,
			evals: NewStack(),
			// threadID will be set by SetThread below
		},
		parentMC:         params.ParentMC,
		escapeCont:       params.EscapeCont,
		exceptionHandler: params.ExceptionHandler,
		maxCallDepth:     params.MaxCallDepth,
		// thread will be set by SetThread below
	}
	sub.SetThread(thread) // Sets both thread object and threadID from thread.ID()
	return sub
}
