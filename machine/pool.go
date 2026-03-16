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
)

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

// envFramePool recycles EnvironmentFrame structs used in the Apply copy path.
// Frames are created on every non-tail closure call and stored in the
// continuation chain. On normal return (RestoreAndRelease, unshared path),
// the old mc.env is overwritten — the pool recycles it instead of leaving it
// for GC. Shared frames (marked by call/cc) are never pooled.
var envFramePool = registerPool(pools, NewPool("env_frame",
	func() *environment.EnvironmentFrame {
		return &environment.EnvironmentFrame{}
	},
	func(f *environment.EnvironmentFrame) {
		f.ResetForPool()
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

// AcquireTopLevelContext returns a pooled MachineContext initialized for
// top-level execution (no parent continuation). This eliminates the
// intermediate MachineContinuation allocation that NewMachineContinuation +
// NewMachineContext would otherwise perform when parent is nil.
//
// The caller MUST call ReleaseTopLevelContext after Run returns.
func AcquireTopLevelContext(ctx context.Context, tpl *NativeTemplate, env *environment.EnvironmentFrame) *MachineContext {
	mc := subContextPool.Acquire()
	mc.ctx = ctx
	mc.env = env
	mc.template = tpl
	mc.evals = acquireStack()
	mc.counters.opcodeHits = newOpcodeHits()
	mc.counters.callCounts = newCallCounts()
	return mc
}

// ReleaseTopLevelContext zeros the MachineContext and returns it to the pool.
// Exported because the primary call sites live in the root wile package (engine.go).
func ReleaseTopLevelContext(mc *MachineContext) {
	if mc == nil {
		return
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
	// envPooled: zero value (false) — macro context uses closure's env, not from pool.
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

// acquireEnvFrame returns a zeroed EnvironmentFrame from the pool.
func acquireEnvFrame() *environment.EnvironmentFrame {
	return envFramePool.Acquire()
}

// releaseEnvFrame zeros the EnvironmentFrame (breaking GC references) and
// returns it to the pool. Nil-safe.
//
// Must NOT be called on frames stored in shared continuations — those frames
// may be re-invoked by call/cc and must remain live for GC.
func releaseEnvFrame(f *environment.EnvironmentFrame) {
	if f == nil {
		return
	}
	envFramePool.Release(f)
}
