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
)

// Object pooling recycles short-lived allocations that follow an acquire/release
// lifecycle. Each non-tail call creates a continuation frame and eval stack;
// pooling avoids per-call heap allocations.
//
// Three pool implementations are used: Pool[T] (sync.Pool-backed) for
// sub-contexts; unsyncFreeList[T] (lock-free, single-goroutine) for the
// per-thread stack, continuation, and environment-frame freelists that serve
// every rooted context; and FreeList[T] (mutex-guarded slice) as the
// process-global fallback for rootless contexts, where sync.Pool's GC-clearing
// behavior causes a feedback loop (high alloc rate → frequent GC → pool cleared
// → more allocs).
//
// See BIBLIOGRAPHY.md "Object Pooling".

// stackInitialCap is the initial capacity for pooled eval stacks.
// Most call sites use 0-4 stack slots (procedure + 1-3 arguments).
// Profiling shows >97% of PopAll depths are ≤4.
const stackInitialCap = 8

// pools is the package-level pool manager. It aggregates all pools for
// unified observation and control (stats, drain, enable/disable).
var pools = NewPoolManager()

// stackPool recycles Stack allocations. A non-tail call (SaveContinuation)
// acquires a stack only when the eval stack is deeper than inlineEvalsCap;
// a shallower save inlines its values into the continuation frame and reuses
// mc's stack. Pooling avoids repeated heap allocation of the backing slice.
var stackPool = registerFreeList(pools, NewFreeList("stack", newStackPoolEntry, resetStackPoolEntry))

func newStackPoolEntry() *Stack {
	s := make(Stack, 0, stackInitialCap)
	return &s
}

func resetStackPoolEntry(s *Stack) {
	full := (*s)[:cap(*s)]
	for i := range full {
		full[i] = nil
	}
	*s = full[:0]
}

// subContextPool recycles MachineContext structs used as sub-contexts.
// Sub-contexts are created by NewSubContext for every foreign function
// that needs to call back into Scheme, and are immediately dead after
// the call returns.
var subContextPool = registerPool(pools, NewPool("sub_context",
	func() *MachineContext {
		return &MachineContext{}
	},
	func(mc *MachineContext) {
		// The eval stack is released explicitly by ReleaseSubContext /
		// ReleaseTopLevelContext (which know mc's per-thread pool) before this
		// reset runs; here we only zero the struct.
		*mc = MachineContext{}
	},
))

// continuationPool recycles MachineContinuation frames. Frames are created
// on every non-tail call (SaveContinuation) and consumed on every normal
// return (RestoreAndRelease). Only the normal-return path pools frames;
// call/cc, escape, and composable continuation paths must not pool because
// the frame may be re-invoked.
var continuationPool = registerFreeList(pools, NewFreeList("continuation", newContinuationPoolEntry, resetContinuationPoolEntry))

func newContinuationPoolEntry() *MachineContinuation {
	return &MachineContinuation{}
}

// resetContinuationPoolEntry zeros the frame. The eval stack is released
// explicitly by releaseContinuation (which knows the per-thread pool) before
// this reset runs.
func resetContinuationPoolEntry(cont *MachineContinuation) {
	*cont = MachineContinuation{}
}

// defaultBindingsCap is the pre-allocated binding capacity for fresh env
// frames from the pool. Most lambdas take 1-3 parameters; cap 4 covers
// >95% of closures without waste. Frames that need more will grow via
// make([]Binding, n) in copyForApplyInto — a one-time cost per frame
// that is retained across subsequent pool cycles via ResetForPool.
const defaultBindingsCap = 4

// envFramePool recycles EnvironmentFrame structs used in the Apply copy path.
// Frames are created on every non-tail closure call and stored in the
// continuation chain. On normal return (RestoreAndRelease, unshared path),
// the old mc.env is overwritten — the pool recycles it instead of leaving it
// for GC. Shared frames (marked by call/cc) are never pooled.
//
// Uses a mutex-guarded freelist instead of sync.Pool because sync.Pool is
// cleared on every GC cycle. In recursive Scheme workloads the GC runs
// 1000+ times per second, giving sync.Pool a <1% hit rate. A freelist
// survives GC, so after warmup (one full recursion depth) every acquire
// is a hit and copyForApplyInto reuses the retained bindings capacity.
var envFramePool = registerFreeList(pools, NewFreeList("env_frame", newEnvFramePoolEntry, resetEnvFramePoolEntry))

// newEnvFramePoolEntry / resetEnvFramePoolEntry are the env-frame freelist
// factory and reset. Extracted as named functions so the process-global
// envFramePool and each per-thread threadPools.envFrames share identical
// behavior.
func newEnvFramePoolEntry() *environment.EnvironmentFrame {
	f := &environment.EnvironmentFrame{}
	f.PreAllocateBindings(defaultBindingsCap)
	return f
}

func resetEnvFramePoolEntry(f *environment.EnvironmentFrame) {
	f.ResetForPool()
}

// threadPools holds a thread's (goroutine's) private allocation freelists.
// It is minted once at each thread root (NewMachineContext, NewThreadSubContext,
// AcquireTopLevelContext) and inherited by reference through NewSubContext, so
// every same-goroutine context shares one set and no two goroutines ever touch
// the same freelist — removing the mutex and atomic-counter contention that
// process-global pools impose on parallel threads.
//
// Safe only because continuations are thread-confined (a frame allocated by one
// thread is never released by another); see
// plans/2026-06-08-per-thread-pools-invariant.md.
//
// The three freelists hit on every non-tail closure call (env frame +
// continuation frame + eval stack) are all per-thread. A nil threadPools
// (cold/expand-time contexts without a root) falls back to the global pools.
type threadPools struct {
	envFrames     *unsyncFreeList[environment.EnvironmentFrame]
	continuations *unsyncFreeList[MachineContinuation]
	stacks        *unsyncFreeList[Stack]
}

// newThreadPools mints a fresh, unregistered set of per-thread freelists.
// Per-thread pools are deliberately NOT registered with the global PoolManager:
// they are single-goroutine, so they use the lock-free unsyncFreeList (no mutex,
// no atomic counters) and must not share its lock or aggregate counters.
func newThreadPools() *threadPools {
	return &threadPools{
		envFrames:     newUnsyncFreeList(newEnvFramePoolEntry, resetEnvFramePoolEntry),
		continuations: newUnsyncFreeList(newContinuationPoolEntry, resetContinuationPoolEntry),
		stacks:        newUnsyncFreeList(newStackPoolEntry, resetStackPoolEntry),
	}
}

// Each acquire/release dispatches between the lock-free per-thread freelist (the
// hot path, present once a context has a thread root) and the synchronized
// process-global pool (the fallback for rootless cold/expand-time contexts where
// p.pools is nil). The two pools are distinct concrete types — *unsyncFreeList
// and *FreeList — so the branch lives in each method rather than behind a shared
// accessor, keeping both calls statically dispatched.

// acquireEnvFrame returns a zeroed EnvironmentFrame from this context's pool.
func (p *MachineContext) acquireEnvFrame() *environment.EnvironmentFrame {
	if p.pools != nil {
		return p.pools.envFrames.Acquire()
	}
	return envFramePool.Acquire()
}

// releaseEnvFrame returns an EnvironmentFrame to this context's pool. Nil-safe.
// Must NOT be called on frames stored in shared continuations.
//
// EnvFramePoolReleases is counted HERE, not at the call sites. It used to be
// incremented beside each call, and OpReleaseEnvFrame — added later — did not
// get one, so the counter undercounted by exactly that opcode's executions
// (2,264 of 5,623,323 on nqueens: silent, and worse the further the reclamation
// proof reaches). Owning the count here makes a new release site countable by
// construction.
func (p *MachineContext) releaseEnvFrame(f *environment.EnvironmentFrame) {
	if f == nil {
		return
	}
	p.counters.EnvFramePoolReleases++
	if p.pools != nil {
		p.pools.envFrames.Release(f)
		return
	}
	envFramePool.Release(f)
}

// acquireStack returns a zeroed-length Stack from this context's pool.
func (p *MachineContext) acquireStack() *Stack {
	if p.pools != nil {
		return p.pools.stacks.Acquire()
	}
	return stackPool.Acquire()
}

// releaseStack returns a Stack to this context's pool. Nil-safe.
func (p *MachineContext) releaseStack(s *Stack) {
	if s == nil {
		return
	}
	if p.pools != nil {
		p.pools.stacks.Release(s)
		return
	}
	stackPool.Release(s)
}

// acquireContinuation returns a zeroed MachineContinuation from this context's pool.
func (p *MachineContext) acquireContinuation() *MachineContinuation {
	if p.pools != nil {
		return p.pools.continuations.Acquire()
	}
	return continuationPool.Acquire()
}

// releaseContinuation releases the frame's eval stack to this context's stack
// pool, then returns the frame to this context's continuation pool. Nil-safe.
// Must NOT be called on shared (call/cc-captured) frames.
func (p *MachineContext) releaseContinuation(cont *MachineContinuation) {
	if cont == nil {
		return
	}
	p.releaseStack(cont.evals)
	cont.evals = nil
	if p.pools != nil {
		p.pools.continuations.Release(cont)
		return
	}
	continuationPool.Release(cont)
}

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
	if mc.envPooled {
		mc.releaseEnvFrame(mc.env)
		mc.env = nil
	}
	mc.releaseStack(mc.evals) // to mc's per-thread pool before the reset zeros it
	mc.evals = nil
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
	mc.pools = newThreadPools() // top-level execution root: its own freelists
	mc.env = env
	mc.template = tpl
	mc.evals = mc.acquireStack()
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
	if mc.envPooled {
		mc.releaseEnvFrame(mc.env)
		mc.env = nil
	}
	mc.releaseStack(mc.evals) // to mc's per-thread pool before the reset zeros it
	mc.evals = nil
	subContextPool.Release(mc)
}

// acquireMacroContext returns a pooled MachineContext initialized for running
// a macro transformer closure. Callers must defer ReleaseSubContext(mc).
//
// It builds the context directly from the closure's template for the
// two macro expansion call sites, eliminating the intermediate
// MachineContinuation allocation a continuation-based constructor would incur.
func acquireMacroContext(ctx context.Context, cls *MachineClosure) *MachineContext {
	mc := acquireSubContext()
	mc.ctx = ctx
	// env is left nil deliberately. The sole caller applies cls immediately
	// (macro_evaluator.go), and Apply establishes p.env itself — either the
	// closure's frame reused in place or a pooled apply frame. Seeding it here
	// would be dead, and seeding it with the closure's compile-time frame would
	// be dead AND wrong for anyone who later reads it before the Apply.
	// envPooled: zero value (false) — Apply sets it to match what it chose.
	mc.env = nil
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
	releaseStack(cont.evals)
	cont.evals = nil
	continuationPool.Release(cont)
}

// acquireEnvFrame returns a zeroed EnvironmentFrame from the freelist.
func acquireEnvFrame() *environment.EnvironmentFrame {
	return envFramePool.Acquire()
}

// releaseEnvFrame zeros the EnvironmentFrame (breaking GC references) and
// returns it to the freelist. Nil-safe.
//
// Must NOT be called on frames stored in shared continuations — those frames
// may be re-invoked by call/cc and must remain live for GC.
func releaseEnvFrame(f *environment.EnvironmentFrame) {
	if f == nil {
		return
	}
	envFramePool.Release(f)
}
