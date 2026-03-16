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
	"sync"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// ---------------------------------------------------------------------------
// Stack pool
// ---------------------------------------------------------------------------

func TestAcquireStack_ReturnsEmptyStack(t *testing.T) {
	s := acquireStack()
	qt.Assert(t, s, qt.IsNotNil)
	qt.Assert(t, s.Len(), qt.Equals, 0)
	qt.Assert(t, cap(*s) > 0, qt.IsTrue)
}

func TestReleaseStack_NilIsNoop(t *testing.T) {
	// Must not panic.
	releaseStack(nil)
}

func TestStackPool_Roundtrip(t *testing.T) {
	s := acquireStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))
	qt.Assert(t, s.Len(), qt.Equals, 2)

	savedCap := cap(*s)
	releaseStack(s)

	// After release the same (or another) stack comes back empty but
	// retains its backing array capacity.
	s2 := acquireStack()
	qt.Assert(t, s2, qt.IsNotNil)
	qt.Assert(t, s2.Len(), qt.Equals, 0)
	qt.Assert(t, cap(*s2) >= savedCap, qt.IsTrue)
}

func TestReleaseStack_NilsClearsElements(t *testing.T) {
	s := acquireStack()
	s.Push(values.NewInteger(42))
	s.Push(values.NewInteger(99))
	qt.Assert(t, s.Len(), qt.Equals, 2)

	// Expand slice to full capacity to inspect backing array after release.
	releaseStack(s)

	// Re-acquire and check that the backing array has been cleared.
	s2 := acquireStack()
	full := (*s2)[:cap(*s2)]
	for i, v := range full {
		qt.Assert(t, v, qt.IsNil, qt.Commentf("element %d should be nil after release", i))
	}
}

// ---------------------------------------------------------------------------
// SubContext pool
// ---------------------------------------------------------------------------

func TestAcquireSubContext_ReturnsZeroedContext(t *testing.T) {
	mc := acquireSubContext()
	qt.Assert(t, mc, qt.IsNotNil)
	qt.Assert(t, mc.parentMC, qt.IsNil)
	qt.Assert(t, mc.evals, qt.IsNil)
	qt.Assert(t, mc.cont, qt.IsNil)
	qt.Assert(t, mc.ctx, qt.IsNil)
}

func TestReleaseSubContext_NilIsNoop(t *testing.T) {
	// Must not panic.
	ReleaseSubContext(nil)
}

func TestSubContextPool_Roundtrip(t *testing.T) {
	mc := acquireSubContext()

	// Simulate NewSubContext field assignment.
	parent := &MachineContext{}
	mc.parentMC = parent
	mc.evals = acquireStack()
	mc.evals.Push(values.NewInteger(7))

	ReleaseSubContext(mc)

	// Re-acquire: all fields must be zeroed.
	mc2 := acquireSubContext()
	qt.Assert(t, mc2, qt.IsNotNil)
	qt.Assert(t, mc2.parentMC, qt.IsNil)
	qt.Assert(t, mc2.evals, qt.IsNil)
	qt.Assert(t, mc2.cont, qt.IsNil)
	qt.Assert(t, mc2.exceptionHandler, qt.IsNil)
	qt.Assert(t, mc2.thread, qt.IsNil)
}

func TestAcquireTopLevelContext_InitializesOpcodeHits(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	env := environment.NewTopLevelEnvironment().Runtime()
	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	defer ReleaseTopLevelContext(mc)

	if opcodeHitsEnabled() {
		qt.Assert(t, mc.counters.opcodeHits != nil, qt.IsTrue)
	}
}

func TestAcquireTopLevelContext_OpcodeHitsZeroedAfterReuse(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	env := environment.NewTopLevelEnvironment().Runtime()

	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	if mc.counters.opcodeHits != nil {
		mc.counters.opcodeHits[OpPush] = 42
	}
	ReleaseTopLevelContext(mc)

	mc2 := AcquireTopLevelContext(context.Background(), tpl, env)
	defer ReleaseTopLevelContext(mc2)
	if mc2.counters.opcodeHits != nil {
		qt.Assert(t, mc2.counters.opcodeHits[OpPush], qt.Equals, uint64(0))
	}
}

func TestAcquireTopLevelContext_InitializesCallCounts(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	env := environment.NewTopLevelEnvironment().Runtime()
	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	defer ReleaseTopLevelContext(mc)

	if opcodeHitsEnabled() {
		qt.Assert(t, mc.counters.callCounts != nil, qt.IsTrue)
	}
}

func TestAcquireTopLevelContext_CallCountsZeroedAfterReuse(t *testing.T) {
	tpl := NewEmptyNativeTemplate()
	env := environment.NewTopLevelEnvironment().Runtime()

	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	if mc.counters.callCounts != nil {
		mc.counters.callCounts["+"] = 42
	}
	ReleaseTopLevelContext(mc)

	mc2 := AcquireTopLevelContext(context.Background(), tpl, env)
	defer ReleaseTopLevelContext(mc2)
	if mc2.counters.callCounts != nil {
		qt.Assert(t, mc2.counters.callCounts["+"], qt.Equals, uint64(0))
	}
}

func TestReleaseSubContext_ReleasesEvalsStack(t *testing.T) {
	mc := acquireSubContext()
	mc.evals = acquireStack()
	mc.evals.Push(values.NewInteger(1))
	mc.evals.Push(values.NewInteger(2))
	savedCap := cap(*mc.evals)

	ReleaseSubContext(mc)

	// The evals stack should have been returned to stackPool.
	// Acquire from the pool and verify we get a zeroed stack with
	// the same (or greater) capacity.
	s := acquireStack()
	qt.Assert(t, s.Len(), qt.Equals, 0)
	qt.Assert(t, cap(*s) >= savedCap, qt.IsTrue)
	full := (*s)[:cap(*s)]
	for i, v := range full {
		qt.Assert(t, v, qt.IsNil, qt.Commentf("element %d should be nil", i))
	}
	releaseStack(s)
}

func TestReleaseSubContext_IncrementsParentCounter(t *testing.T) {
	parent := &MachineContext{}
	qt.Assert(t, parent.counters.SubContextPoolReleases, qt.Equals, uint64(0))

	mc := acquireSubContext()
	mc.parentMC = parent
	ReleaseSubContext(mc)

	qt.Assert(t, parent.counters.SubContextPoolReleases, qt.Equals, uint64(1))

	// Second release increments again.
	mc2 := acquireSubContext()
	mc2.parentMC = parent
	ReleaseSubContext(mc2)

	qt.Assert(t, parent.counters.SubContextPoolReleases, qt.Equals, uint64(2))
}

func TestReleaseSubContext_NoParent_NoPanic(t *testing.T) {
	// Sub-context with nil parentMC (shouldn't happen in practice, but
	// must not panic).
	mc := acquireSubContext()
	mc.parentMC = nil
	ReleaseSubContext(mc)
}

// ---------------------------------------------------------------------------
// Macro context pool (acquireMacroContext)
// ---------------------------------------------------------------------------

func TestAcquireMacroContext_InitializesFields(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(2)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(2, 0, false)
	cls := NewClosureWithTemplate(tpl, env)

	ctx := context.Background()
	mc := acquireMacroContext(ctx, cls)
	defer ReleaseSubContext(mc)

	// Fields set from closure + context.
	qt.Assert(t, mc.ctx, qt.Equals, ctx)
	qt.Assert(t, mc.env, qt.Equals, env)
	qt.Assert(t, mc.template, qt.Equals, tpl)
	qt.Assert(t, mc.evals, qt.IsNotNil)
	qt.Assert(t, mc.evals.Len(), qt.Equals, 0)

	// Fields NOT set must be zero.
	qt.Assert(t, mc.cont, qt.IsNil)
	qt.Assert(t, mc.parentMC, qt.IsNil)
	qt.Assert(t, mc.expanderCtx, qt.IsNil)
	qt.Assert(t, mc.exceptionHandler, qt.IsNil)
	qt.Assert(t, mc.singleValue, qt.IsNil)
	qt.Assert(t, mc.multiValues, qt.IsNil)
	qt.Assert(t, mc.pc, qt.Equals, 0)
	qt.Assert(t, mc.thread, qt.IsNil)
}

func TestAcquireMacroContext_Roundtrip(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(2)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(2, 0, false)
	cls := NewClosureWithTemplate(tpl, env)

	ctx := context.Background()
	mc := acquireMacroContext(ctx, cls)

	// Simulate what a macro expansion does: set expanderCtx, run, get value.
	mc.expanderCtx = &ExpanderContext{}
	mc.singleValue = values.NewInteger(42)

	ReleaseSubContext(mc)

	// Re-acquire: all fields must be reset (new closure fields replace old).
	env2 := environment.NewEnvironmentFrameWithParent(
		environment.NewLocalEnvironment(1), topEnv,
	)
	tpl2 := NewNativeTemplate(1, 0, false)
	cls2 := NewClosureWithTemplate(tpl2, env2)

	mc2 := acquireMacroContext(ctx, cls2)
	defer ReleaseSubContext(mc2)

	qt.Assert(t, mc2.env, qt.Equals, env2)
	qt.Assert(t, mc2.template, qt.Equals, tpl2)
	qt.Assert(t, mc2.expanderCtx, qt.IsNil)
	qt.Assert(t, mc2.singleValue, qt.IsNil)
	qt.Assert(t, mc2.evals, qt.IsNotNil)
	qt.Assert(t, mc2.evals.Len(), qt.Equals, 0)
}

// ---------------------------------------------------------------------------
// Continuation pool
// ---------------------------------------------------------------------------

func TestAcquireContinuation_ReturnsZeroedFrame(t *testing.T) {
	cont := acquireContinuation()
	qt.Assert(t, cont, qt.IsNotNil)
	qt.Assert(t, cont.parent, qt.IsNil)
	qt.Assert(t, cont.env, qt.IsNil)
	qt.Assert(t, cont.template, qt.IsNil)
	qt.Assert(t, cont.evals, qt.IsNil)
	qt.Assert(t, cont.singleValue, qt.IsNil)
	qt.Assert(t, cont.multiValues, qt.IsNil)
	qt.Assert(t, cont.pc, qt.Equals, 0)
	qt.Assert(t, cont.callDepth, qt.Equals, 0)
	qt.Assert(t, cont.promptHandler, qt.IsNil)
	qt.Assert(t, cont.promptTag, qt.IsNil)
	qt.Assert(t, cont.shared, qt.IsFalse)
}

func TestReleaseContinuation_NilIsNoop(t *testing.T) {
	// Must not panic.
	releaseContinuation(nil)
}

func TestContinuationPool_Roundtrip(t *testing.T) {
	cont := acquireContinuation()
	cont.env = &environment.EnvironmentFrame{}
	cont.template = &NativeTemplate{}
	cont.pc = 42
	cont.parent = &MachineContinuation{}
	cont.evals = acquireStack()
	cont.evals.Push(values.NewInteger(1))

	releaseContinuation(cont)

	// Re-acquire: all fields must be zeroed.
	cont2 := acquireContinuation()
	qt.Assert(t, cont2, qt.IsNotNil)
	qt.Assert(t, cont2.env, qt.IsNil)
	qt.Assert(t, cont2.template, qt.IsNil)
	qt.Assert(t, cont2.pc, qt.Equals, 0)
	qt.Assert(t, cont2.parent, qt.IsNil)
	qt.Assert(t, cont2.evals, qt.IsNil)
}

func TestReleaseContinuation_ReleasesEvalsStack(t *testing.T) {
	cont := acquireContinuation()
	cont.evals = acquireStack()
	cont.evals.Push(values.NewInteger(1))
	cont.evals.Push(values.NewInteger(2))
	savedCap := cap(*cont.evals)

	releaseContinuation(cont)

	// The evals stack should have been returned to stackPool.
	s := acquireStack()
	qt.Assert(t, s.Len(), qt.Equals, 0)
	qt.Assert(t, cap(*s) >= savedCap, qt.IsTrue)
	releaseStack(s)
}

func TestReleaseContinuation_ClearsAllReferences(t *testing.T) {
	cont := acquireContinuation()
	cont.env = &environment.EnvironmentFrame{}
	cont.template = &NativeTemplate{}
	cont.parent = &MachineContinuation{}
	cont.singleValue = values.NewInteger(42)
	cont.promptHandler = &MachineClosure{}
	cont.promptTag = NewPromptTag("test")
	cont.evals = acquireStack()

	releaseContinuation(cont)

	// Re-acquire and verify all reference fields are nil.
	cont2 := acquireContinuation()
	qt.Assert(t, cont2.env, qt.IsNil)
	qt.Assert(t, cont2.template, qt.IsNil)
	qt.Assert(t, cont2.parent, qt.IsNil)
	qt.Assert(t, cont2.singleValue, qt.IsNil)
	qt.Assert(t, cont2.promptHandler, qt.IsNil)
	qt.Assert(t, cont2.promptTag, qt.IsNil)
	qt.Assert(t, cont2.evals, qt.IsNil)
}

func TestRestoreAndRelease_TransfersEvalsAndPoolsFrame(t *testing.T) {
	// Build a minimal continuation with known evals.
	cont := acquireContinuation()
	evalsStack := acquireStack()
	evalsStack.Push(values.NewInteger(10))
	evalsStack.Push(values.NewInteger(20))
	cont.evals = evalsStack
	cont.env = &environment.EnvironmentFrame{}
	cont.template = &NativeTemplate{}
	cont.pc = 7

	// Build an mc with its own evals.
	mc := &MachineContext{}
	mc.evals = acquireStack()
	mc.evals.Push(values.NewInteger(99))

	mc.RestoreAndRelease(cont)

	// mc.evals should now be the transferred stack (not a copy).
	qt.Assert(t, mc.evals, qt.Equals, evalsStack)
	qt.Assert(t, mc.evals.Len(), qt.Equals, 2)
	qt.Assert(t, mc.pc, qt.Equals, 7)

	// The continuation frame should have been pooled (we can't directly
	// observe this, but acquiring again should give us a zeroed frame).
	cont2 := acquireContinuation()
	qt.Assert(t, cont2.evals, qt.IsNil)
	qt.Assert(t, cont2.parent, qt.IsNil)
}

func TestRestoreAndRelease_IncrementsCounters(t *testing.T) {
	cont := acquireContinuation()
	cont.evals = acquireStack()
	cont.env = &environment.EnvironmentFrame{}
	cont.template = &NativeTemplate{}

	mc := &MachineContext{}
	mc.evals = acquireStack()

	qt.Assert(t, mc.counters.ContinuationsRestored, qt.Equals, uint64(0))
	qt.Assert(t, mc.counters.StackPoolReleases, qt.Equals, uint64(0))
	qt.Assert(t, mc.counters.ContinuationPoolReleases, qt.Equals, uint64(0))

	mc.RestoreAndRelease(cont)

	qt.Assert(t, mc.counters.ContinuationsRestored, qt.Equals, uint64(1))
	qt.Assert(t, mc.counters.StackPoolReleases, qt.Equals, uint64(1))
	qt.Assert(t, mc.counters.ContinuationPoolReleases, qt.Equals, uint64(1))
}

// ---------------------------------------------------------------------------
// Shared frame behavior
// ---------------------------------------------------------------------------

func TestMarkChainShared_MarksAllFrames(t *testing.T) {
	c := &MachineContinuation{}
	b := &MachineContinuation{parent: c}
	a := &MachineContinuation{parent: b}

	a.MarkChainShared()

	qt.Assert(t, a.shared, qt.IsTrue)
	qt.Assert(t, b.shared, qt.IsTrue)
	qt.Assert(t, c.shared, qt.IsTrue)
}

func TestMarkChainShared_EarlyExitOnAlreadyShared(t *testing.T) {
	c := &MachineContinuation{shared: true}
	b := &MachineContinuation{parent: c}
	a := &MachineContinuation{parent: b}

	a.MarkChainShared()

	// a and b should be marked, c was already shared.
	qt.Assert(t, a.shared, qt.IsTrue)
	qt.Assert(t, b.shared, qt.IsTrue)
	qt.Assert(t, c.shared, qt.IsTrue)
}

func TestMarkChainShared_NilIsNoop(t *testing.T) {
	// Must not panic.
	var p *MachineContinuation
	p.MarkChainShared()
}

func TestCopy_DoesNotPropagateShared(t *testing.T) {
	orig := &MachineContinuation{}
	orig.shared = true
	orig.evals = NewStack()

	cp := orig.Copy()
	qt.Assert(t, cp.shared, qt.IsFalse)
}

func TestRestoreAndRelease_SharedFrameCopiesEvals(t *testing.T) {
	// Build a shared continuation with known evals.
	cont := acquireContinuation()
	evalsStack := acquireStack()
	evalsStack.Push(values.NewInteger(10))
	evalsStack.Push(values.NewInteger(20))
	cont.evals = evalsStack
	cont.env = &environment.EnvironmentFrame{}
	cont.template = &NativeTemplate{}
	cont.pc = 7
	cont.shared = true

	// Build an mc with its own evals.
	mc := &MachineContext{}
	mc.evals = acquireStack()
	mc.evals.Push(values.NewInteger(99))

	mc.RestoreAndRelease(cont)

	// mc.evals should be a COPY (not the same pointer as cont.evals).
	qt.Assert(t, mc.evals != evalsStack, qt.IsTrue)
	qt.Assert(t, mc.evals.Len(), qt.Equals, 2)
	qt.Assert(t, mc.pc, qt.Equals, 7)

	// The original evals in cont should be preserved for re-invocation.
	qt.Assert(t, cont.evals, qt.Equals, evalsStack)
	qt.Assert(t, cont.evals.Len(), qt.Equals, 2)

	qt.Assert(t, mc.counters.SharedFrameRestores, qt.Equals, uint64(1))
	qt.Assert(t, mc.counters.ContinuationPoolReleases, qt.Equals, uint64(0))
}

func TestRestoreAndRelease_UnsharedFrameStillPools(t *testing.T) {
	// Verify the unshared path is unchanged.
	cont := acquireContinuation()
	evalsStack := acquireStack()
	evalsStack.Push(values.NewInteger(10))
	cont.evals = evalsStack
	cont.env = &environment.EnvironmentFrame{}
	cont.template = &NativeTemplate{}
	cont.pc = 3

	mc := &MachineContext{}
	mc.evals = acquireStack()

	mc.RestoreAndRelease(cont)

	// mc.evals should be the transferred stack (same pointer).
	qt.Assert(t, mc.evals, qt.Equals, evalsStack)
	qt.Assert(t, mc.counters.SharedFrameRestores, qt.Equals, uint64(0))
	qt.Assert(t, mc.counters.ContinuationPoolReleases, qt.Equals, uint64(1))
}

// ---------------------------------------------------------------------------
// Pool stats tracking
// ---------------------------------------------------------------------------

func TestStackPool_StatsTracked(t *testing.T) {
	// Read baseline stats (pools are shared across tests, so use relative checks).
	before := stackPool.Stats()

	s := acquireStack()
	s.Push(values.NewInteger(1))
	releaseStack(s)

	after := stackPool.Stats()
	qt.Assert(t, after.Acquires-before.Acquires, qt.Equals, uint64(1))
	qt.Assert(t, after.Releases-before.Releases, qt.Equals, uint64(1))
}

func TestContinuationPool_StatsTracked(t *testing.T) {
	before := continuationPool.Stats()

	cont := acquireContinuation()
	releaseContinuation(cont)

	after := continuationPool.Stats()
	qt.Assert(t, after.Acquires-before.Acquires, qt.Equals, uint64(1))
	qt.Assert(t, after.Releases-before.Releases, qt.Equals, uint64(1))
}

func TestSubContextPool_StatsTracked(t *testing.T) {
	before := subContextPool.Stats()

	mc := acquireSubContext()
	ReleaseSubContext(mc)

	after := subContextPool.Stats()
	qt.Assert(t, after.Acquires-before.Acquires, qt.Equals, uint64(1))
	qt.Assert(t, after.Releases-before.Releases, qt.Equals, uint64(1))
}

func TestPoolManager_AllStats_ReportsAllPools(t *testing.T) {
	stats := pools.AllStats()
	qt.Assert(t, len(stats) >= 4, qt.IsTrue)

	names := make(map[string]bool)
	for _, s := range stats {
		names[s.Name] = true
	}
	qt.Assert(t, names["stack"], qt.IsTrue)
	qt.Assert(t, names["sub_context"], qt.IsTrue)
	qt.Assert(t, names["continuation"], qt.IsTrue)
	qt.Assert(t, names["env_frame"], qt.IsTrue)
}

// ---------------------------------------------------------------------------
// Concurrent access
// ---------------------------------------------------------------------------

func TestStackPool_ConcurrentAccess(t *testing.T) {
	const goroutines = 16
	const iterations = 100

	var wg sync.WaitGroup
	wg.Add(goroutines)

	for range goroutines {
		go func() {
			defer wg.Done()
			for range iterations {
				s := acquireStack()
				s.Push(values.NewInteger(1))
				s.Push(values.NewInteger(2))
				qt.Assert(t, s.Len(), qt.Equals, 2)
				releaseStack(s)
			}
		}()
	}
	wg.Wait()
}

func TestSubContextPool_ConcurrentAccess(t *testing.T) {
	const goroutines = 16
	const iterations = 100

	parent := &MachineContext{}

	var wg sync.WaitGroup
	wg.Add(goroutines)

	for range goroutines {
		go func() {
			defer wg.Done()
			for range iterations {
				mc := acquireSubContext()
				mc.parentMC = parent
				mc.evals = acquireStack()
				ReleaseSubContext(mc)
			}
		}()
	}
	wg.Wait()

	// Counter won't be exactly goroutines*iterations because concurrent
	// uint64 increments without atomics can lose writes. But it should be
	// non-zero, confirming the counter path executes.
	qt.Assert(t, parent.counters.SubContextPoolReleases > 0, qt.IsTrue)
}

func TestContinuationPool_ConcurrentAccess(t *testing.T) {
	const goroutines = 16
	const iterations = 100

	var wg sync.WaitGroup
	wg.Add(goroutines)

	for range goroutines {
		go func() {
			defer wg.Done()
			for range iterations {
				cont := acquireContinuation()
				cont.evals = acquireStack()
				cont.evals.Push(values.NewInteger(1))
				cont.pc = 42
				releaseContinuation(cont)
			}
		}()
	}
	wg.Wait()
}

// ---------------------------------------------------------------------------
// Env frame pool
// ---------------------------------------------------------------------------

func TestEnvFramePool_StatsTracked(t *testing.T) {
	before := envFramePool.Stats()

	f := acquireEnvFrame()
	releaseEnvFrame(f)

	after := envFramePool.Stats()
	qt.Assert(t, after.Acquires-before.Acquires, qt.Equals, uint64(1))
	qt.Assert(t, after.Releases-before.Releases, qt.Equals, uint64(1))
}

func TestRestoreAndRelease_ReleasesPooledEnvFrame(t *testing.T) {
	// Set up a pooled env frame as mc.env (simulates Apply copy path).
	env := acquireEnvFrame()
	mc := &MachineContext{}
	mc.env = env
	mc.envPooled = true
	mc.evals = acquireStack()

	// Build a continuation to restore from.
	cont := acquireContinuation()
	cont.evals = acquireStack()
	cont.env = &environment.EnvironmentFrame{}
	cont.template = &NativeTemplate{}

	before := envFramePool.Stats()
	mc.RestoreAndRelease(cont)
	after := envFramePool.Stats()

	// The old pooled env should have been released.
	qt.Assert(t, mc.counters.EnvFramePoolReleases, qt.Equals, uint64(1))
	qt.Assert(t, after.Releases-before.Releases, qt.Equals, uint64(1))
	// Restored env is from the continuation, not pooled.
	qt.Assert(t, mc.envPooled, qt.IsFalse)
}

func TestRestoreAndRelease_SkipsNonPooledEnvFrame(t *testing.T) {
	// mc.env is NOT from the pool (simulates noCopy path).
	mc := &MachineContext{}
	mc.env = &environment.EnvironmentFrame{}
	mc.envPooled = false
	mc.evals = acquireStack()

	cont := acquireContinuation()
	cont.evals = acquireStack()
	cont.env = &environment.EnvironmentFrame{}
	cont.template = &NativeTemplate{}

	before := envFramePool.Stats()
	mc.RestoreAndRelease(cont)
	after := envFramePool.Stats()

	// No env frame should have been released.
	qt.Assert(t, mc.counters.EnvFramePoolReleases, qt.Equals, uint64(0))
	qt.Assert(t, after.Releases-before.Releases, qt.Equals, uint64(0))
}

func TestRestoreAndRelease_SharedCont_ReleasesPooledEnv(t *testing.T) {
	// Shared continuation path should still release the old pooled env.
	env := acquireEnvFrame()
	mc := &MachineContext{}
	mc.env = env
	mc.envPooled = true
	mc.evals = acquireStack()

	cont := acquireContinuation()
	cont.evals = acquireStack()
	cont.evals.Push(values.NewInteger(1))
	cont.env = &environment.EnvironmentFrame{}
	cont.template = &NativeTemplate{}
	cont.shared = true

	before := envFramePool.Stats()
	mc.RestoreAndRelease(cont)
	after := envFramePool.Stats()

	qt.Assert(t, mc.counters.EnvFramePoolReleases, qt.Equals, uint64(1))
	qt.Assert(t, after.Releases-before.Releases, qt.Equals, uint64(1))
	// Restored env from shared continuation must NOT be marked pooled.
	qt.Assert(t, mc.envPooled, qt.IsFalse)
}

func TestRestoreAndRelease_PropagatesEnvPooledFromCont(t *testing.T) {
	// The continuation was saved when mc.envPooled was true. After restoring
	// from an unshared continuation, envPooled should be propagated.
	mc := &MachineContext{}
	mc.env = &environment.EnvironmentFrame{}
	mc.envPooled = false
	mc.evals = acquireStack()

	cont := acquireContinuation()
	cont.evals = acquireStack()
	cont.env = &environment.EnvironmentFrame{}
	cont.template = &NativeTemplate{}
	cont.envPooled = true // saved when caller's env was pooled

	mc.RestoreAndRelease(cont)

	qt.Assert(t, mc.envPooled, qt.IsTrue)
}

func TestRestoreAndRelease_SameEnvIdentity_SkipsRelease(t *testing.T) {
	// When no Apply occurs between SaveContinuation and RestoreContinuation
	// (e.g., a foreign function call), oldEnv == cont.env. Releasing would
	// corrupt the live env frame that mc now points to.
	sharedEnv := acquireEnvFrame()
	mc := &MachineContext{}
	mc.env = sharedEnv
	mc.envPooled = true
	mc.evals = acquireStack()

	cont := acquireContinuation()
	cont.evals = acquireStack()
	cont.env = sharedEnv // same pointer: no Apply changed mc.env
	cont.template = &NativeTemplate{}
	cont.envPooled = true

	before := envFramePool.Stats()
	mc.RestoreAndRelease(cont)
	after := envFramePool.Stats()

	// Must NOT release because oldEnv == p.env (same frame).
	qt.Assert(t, mc.counters.EnvFramePoolReleases, qt.Equals, uint64(0))
	qt.Assert(t, after.Releases-before.Releases, qt.Equals, uint64(0))
	// mc.env should still be the shared frame, usable.
	qt.Assert(t, mc.env, qt.Equals, sharedEnv)
	qt.Assert(t, mc.envPooled, qt.IsTrue)
}
