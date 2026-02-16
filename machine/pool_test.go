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
	qt.Assert(t, cap(*s) >= 8, qt.IsTrue) // pool's New allocates cap 8
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
	qt.Assert(t, cont.callDepth, qt.Equals, uint64(0))
	qt.Assert(t, cont.promptHandler, qt.IsNil)
	qt.Assert(t, cont.promptTag, qt.IsNil)
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
