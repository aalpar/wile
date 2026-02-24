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

import "context"

// Object pooling (sync.Pool) recycles short-lived allocations that follow
// an acquire/release lifecycle. Each non-tail call creates a continuation
// frame and eval stack; pooling avoids per-call heap allocations.
// See BIBLIOGRAPHY.md "Object Pooling".

// stackInitialCap is the initial capacity for pooled eval stacks.
// Most call sites use 0-4 stack slots (procedure + 1-3 arguments).
// Profiling shows >97% of PopAll depths are ≤4.
const stackInitialCap = 8

// pools is the package-level pool manager. It aggregates all pools for
// unified observation and control (stats, drain, enable/disable).
var pools = NewPoolManager()

// stackPool recycles Stack allocations. Stacks are created on every
// non-tail call (SaveContinuation) and discarded on return (Restore).
// Pooling avoids repeated heap allocation of the backing slice.
var stackPool = registerPool(pools, NewPool("stack",
	func() *Stack {
		s := make(Stack, 0, stackInitialCap)
		return &s
	},
	func(s *Stack) {
		full := (*s)[:cap(*s)]
		for i := range full {
			full[i] = nil
		}
		*s = full[:0]
	},
))

// subContextPool recycles MachineContext structs used as sub-contexts.
// Sub-contexts are created by NewSubContext for every foreign function
// that needs to call back into Scheme, and are immediately dead after
// the call returns.
var subContextPool = registerPool(pools, NewPool("sub_context",
	func() *MachineContext {
		return &MachineContext{}
	},
	func(mc *MachineContext) {
		releaseStack(mc.evals)
		*mc = MachineContext{}
	},
))

// continuationPool recycles MachineContinuation frames. Frames are created
// on every non-tail call (SaveContinuation) and consumed on every normal
// return (RestoreAndRelease). Only the normal-return path pools frames;
// call/cc, escape, and composable continuation paths must not pool because
// the frame may be re-invoked.
var continuationPool = registerPool(pools, NewPool("continuation",
	func() *MachineContinuation {
		return &MachineContinuation{}
	},
	func(cont *MachineContinuation) {
		releaseStack(cont.evals)
		*cont = MachineContinuation{}
	},
))

// acquireStack returns a zeroed-length Stack from the pool.
func acquireStack() *Stack {
	return stackPool.Acquire()
}

// releaseStack nils out all accessible elements (so the GC can collect
// referenced values) and returns the Stack to the pool.
func releaseStack(s *Stack) {
	if s == nil {
		return
	}
	stackPool.Release(s)
}

// acquireSubContext returns a zeroed MachineContext from the pool.
func acquireSubContext() *MachineContext {
	return subContextPool.Acquire()
}

// ReleaseSubContext zeros the MachineContext and returns it to the pool.
// Exported because call sites live in other packages (registry/, extensions/).
func ReleaseSubContext(mc *MachineContext) {
	if mc == nil {
		return
	}
	if mc.parentMC != nil {
		mc.parentMC.counters.SubContextPoolReleases++
	}
	subContextPool.Release(mc)
}

// acquireMacroContext returns a pooled MachineContext initialized for running
// a macro transformer closure. Callers must defer ReleaseSubContext(mc).
//
// This replaces NewMachineContextFromMachineClosure for the two macro expansion
// call sites, eliminating the intermediate MachineContinuation allocation.
func acquireMacroContext(ctx context.Context, cls *MachineClosure) *MachineContext {
	mc := acquireSubContext()
	mc.ctx = ctx
	mc.env = cls.env
	mc.template = cls.template
	mc.evals = acquireStack()
	return mc
}

// acquireContinuation returns a zeroed MachineContinuation from the pool.
func acquireContinuation() *MachineContinuation {
	return continuationPool.Acquire()
}

// releaseContinuation returns the continuation's evals stack to the stack
// pool, zeros all fields (breaking GC references), and returns the frame
// to the continuation pool. Nil-safe.
//
// Shared frames (shared == true) must NOT be passed to this function.
// RestoreAndRelease skips pooling for shared frames, leaving them for GC.
func releaseContinuation(cont *MachineContinuation) {
	if cont == nil {
		return
	}
	continuationPool.Release(cont)
}
