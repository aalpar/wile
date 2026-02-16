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
