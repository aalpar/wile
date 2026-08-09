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

package values_test

import (
	"context"
	"errors"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

func TestMutex_NewMutex(t *testing.T) {
	m := values.NewMutex("test-mutex")
	qt.Assert(t, m, qt.Not(qt.IsNil))
	qt.Assert(t, m.Name(), qt.Equals, "test-mutex")
	qt.Assert(t, m.ID() > 0, qt.IsTrue)
	qt.Assert(t, m.State(), qt.Equals, values.MutexUnlocked)
}

func TestMutex_DefaultName(t *testing.T) {
	m := values.NewMutex("")
	qt.Assert(t, strings.HasPrefix(m.Name(), "mutex-"), qt.IsTrue)
}

func TestMutex_Specific(t *testing.T) {
	m := values.NewMutex("test")
	qt.Assert(t, m.Specific() == nil, qt.IsTrue)

	m.SetSpecific(values.NewInteger(42))
	qt.Assert(t, m.Specific(), valuestest.SchemeEquals, values.NewInteger(42))
}

func TestMutexState_String(t *testing.T) {
	tcs := []struct {
		state values.MutexState
		str   string
	}{
		{values.MutexUnlocked, "unlocked"},
		{values.MutexLocked, "locked"},
		{values.MutexAbandoned, "abandoned"},
		{values.MutexState(99), "unknown"},
	}
	for _, tc := range tcs {
		t.Run(tc.str, func(t *testing.T) {
			qt.Assert(t, tc.state.String(), qt.Equals, tc.str)
		})
	}
}

func TestMutex_LockUnlock_NoOwner(t *testing.T) {
	m := values.NewMutex("test")

	ok, err := m.Lock(nil, nil)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, m.State(), qt.Equals, values.MutexLocked)
	qt.Assert(t, m.Owner() == nil, qt.IsTrue)

	ok = m.Unlock(nil, nil)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, m.State(), qt.Equals, values.MutexUnlocked)
}

func TestMutex_LockUnlock_WithOwner(t *testing.T) {
	m := values.NewMutex("test")
	th := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "owner")

	ok, err := m.Lock(nil, th)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, m.State(), qt.Equals, values.MutexLocked)
	qt.Assert(t, m.Owner(), qt.Equals, th)

	m.Unlock(nil, nil)
	qt.Assert(t, m.Owner() == nil, qt.IsTrue)
}

func TestMutex_StateValue(t *testing.T) {
	m := values.NewMutex("test")

	// Unlocked → 'not-owned (per SRFI-18 collapse).
	sv := m.StateValue()
	qt.Assert(t, sv, valuestest.SchemeEquals, values.NewSymbol("not-owned"))

	// Locked with nil owner → 'not-owned. This is the load-bearing SRFI-18
	// collapse the refactor relies on: MutexUnlocked and MutexLocked with
	// owner==nil both surface as the same Scheme symbol. A regression that
	// drops the owner-nil check inside the MutexLocked branch would fail
	// here rather than at a far-away user-reported bug.
	m.Lock(nil, nil)
	sv = m.StateValue()
	qt.Assert(t, sv, valuestest.SchemeEquals, values.NewSymbol("not-owned"))
	m.Unlock(nil, nil)

	// Locked with owner → owner thread.
	th := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "owner")
	m.Lock(nil, th)
	sv = m.StateValue()
	qt.Assert(t, sv.EqualTo(th), qt.IsTrue)

	// Unlock and mark abandoned.
	m.Unlock(nil, nil)
	m.Lock(nil, th)
	m.MarkAbandoned()
	sv = m.StateValue()
	qt.Assert(t, sv, valuestest.SchemeEquals, values.NewSymbol("abandoned"))
}

func TestMutex_MarkAbandoned(t *testing.T) {
	th := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "owner")

	tcs := []struct {
		name      string
		setup     func(*values.Mutex)
		wantState values.MutexState
		wantOwner bool // true if Owner() should be non-nil after MarkAbandoned
	}{
		{
			name:      "locked-with-owner → abandoned",
			setup:     func(m *values.Mutex) { m.Lock(nil, th) },
			wantState: values.MutexAbandoned,
			wantOwner: false,
		},
		{
			name:      "locked-without-owner → abandoned",
			setup:     func(m *values.Mutex) { m.Lock(nil, nil) },
			wantState: values.MutexAbandoned,
			wantOwner: false,
		},
		{
			name:      "unlocked → no-op (stays unlocked)",
			setup:     func(*values.Mutex) {},
			wantState: values.MutexUnlocked,
			wantOwner: false,
		},
		{
			name: "already-abandoned → no-op (stays abandoned)",
			setup: func(m *values.Mutex) {
				m.Lock(nil, th)
				m.MarkAbandoned()
			},
			wantState: values.MutexAbandoned,
			wantOwner: false,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			m := values.NewMutex(tc.name)
			tc.setup(m)
			m.MarkAbandoned()
			qt.Assert(t, m.State(), qt.Equals, tc.wantState)
			// Invariant: only MutexLocked admits a non-nil owner.
			ownerNonNil := m.Owner() != nil
			qt.Assert(t, ownerNonNil, qt.Equals, tc.wantOwner)
		})
	}
}

func TestMutex_LockAbandoned(t *testing.T) {
	m := values.NewMutex("test")
	th := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "owner")
	m.Lock(nil, th)
	m.MarkAbandoned()

	// Locking an abandoned mutex should succeed but return AbandonedMutexException.
	ok, err := m.Lock(nil, nil)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, err, qt.Not(qt.IsNil))

	// Use errors.As per the project's mandate (CLAUDE.md): never naked
	// type assertions on errors. errors.As walks any future wrapping.
	var abandoned *values.AbandonedMutexException
	qt.Assert(t, errors.As(err, &abandoned), qt.IsTrue)
}

// TestMutex_Lock_Contention exercises the slow-path acquisition site at
// mutex.go's unified post-wait block. The refactor collapsed three formerly
// distinct exit blocks into one; this test covers each entry path:
//   - unlock-wake: waiter blocks, holder Unlocks, waiter acquires cleanly.
//   - abandon-wake: waiter blocks, holder is terminated (MarkAbandoned),
//     waiter acquires with *AbandonedMutexException.
//   - timeout-fired: waiter blocks with a short timeout, deadline passes
//     before any wakeup, Lock returns (false, nil).
//
// Without these, the merged exit site is reachable only from the
// fast-path tests, leaving the wait loops uncovered.
func TestMutex_Lock_Contention(t *testing.T) {
	th := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "owner")

	t.Run("unlock-wake", func(t *testing.T) {
		m := values.NewMutex("contention-unlock")
		m.Lock(nil, th) // primary holder

		var wg sync.WaitGroup
		wg.Add(1)
		var waiterOk bool
		var waiterErr error
		go func() {
			defer wg.Done()
			waiterOk, waiterErr = m.Lock(nil, nil)
		}()

		// Give the waiter time to enter cond.Wait; without sleep this
		// race-occasionally beats the goroutine and Unlock observes no
		// waiters. A small sleep is the established pattern in Wile's
		// thread tests for cond-wait synchronization.
		time.Sleep(20 * time.Millisecond)
		m.Unlock(nil, nil)
		wg.Wait()

		qt.Assert(t, waiterOk, qt.IsTrue)
		qt.Assert(t, waiterErr, qt.IsNil)
		qt.Assert(t, m.State(), qt.Equals, values.MutexLocked)
	})

	t.Run("abandon-wake", func(t *testing.T) {
		m := values.NewMutex("contention-abandon")
		m.Lock(nil, th)

		var wg sync.WaitGroup
		wg.Add(1)
		var waiterOk bool
		var waiterErr error
		go func() {
			defer wg.Done()
			waiterOk, waiterErr = m.Lock(nil, nil)
		}()

		time.Sleep(20 * time.Millisecond)
		m.MarkAbandoned()
		wg.Wait()

		qt.Assert(t, waiterOk, qt.IsTrue)
		var abandoned *values.AbandonedMutexException
		qt.Assert(t, errors.As(waiterErr, &abandoned), qt.IsTrue)
		// After acquiring an abandoned mutex, state transitions back to
		// MutexLocked with the new owner (nil in this case).
		qt.Assert(t, m.State(), qt.Equals, values.MutexLocked)
	})

	t.Run("timeout-fired", func(t *testing.T) {
		m := values.NewMutex("contention-timeout")
		m.Lock(nil, th) // primary holder; never unlocks

		timeout := 30 * time.Millisecond
		start := time.Now()
		ok, err := m.Lock(&timeout, nil)
		elapsed := time.Since(start)

		qt.Assert(t, ok, qt.IsFalse) // SRFI-18: false on timeout.
		qt.Assert(t, err, qt.IsNil)  // No error on plain timeout (only AbandonedMutexException is meaningful).
		// Sanity: actually waited roughly the timeout duration (allow
		// generous slack for CI machines).
		qt.Assert(t, elapsed >= 20*time.Millisecond, qt.IsTrue)
		// State unchanged: still held by the primary thread.
		qt.Assert(t, m.State(), qt.Equals, values.MutexLocked)
		qt.Assert(t, m.Owner(), qt.Equals, th)
	})
}

func TestMutex_IsVoid(t *testing.T) {
	m := values.NewMutex("test")
	qt.Assert(t, m.IsVoid(), qt.IsFalse)

	var nilM *values.Mutex
	qt.Assert(t, nilM.IsVoid(), qt.IsTrue)
}

func TestMutex_EqualTo(t *testing.T) {
	m1 := values.NewMutex("a")
	m2 := values.NewMutex("b")
	qt.Assert(t, m1.EqualTo(m1), qt.IsTrue)
	qt.Assert(t, m1.EqualTo(m2), qt.IsFalse)
	qt.Assert(t, m1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestMutex_SchemeString(t *testing.T) {
	m := values.NewMutex("my-mutex")
	s := m.SchemeString()
	qt.Assert(t, strings.Contains(s, "mutex"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "my-mutex"), qt.IsTrue)

	var nilM *values.Mutex
	qt.Assert(t, nilM.SchemeString(), qt.Equals, "#<mutex:void>")
}

// TestUnlockCVNoLostWakeup drives the SRFI-18 unlock-and-wait rendezvous directly at
// the Go level, which is where the lost-wakeup window is narrow enough to hit
// reliably. Each iteration races a consumer (lock, predicate false, unlock-and-wait)
// against a producer (lock, set predicate, signal, unlock). The producer holds the
// mutex while signalling, so under enqueue-before-release the signal can never land
// on an empty wait set.
//
// Before the fix, Unlock released the mutex and only then called cv.Wait: a producer
// that acquired the mutex in that gap signalled nothing and the consumer parked
// forever. The per-iteration deadline turns that hang into a failure instead of a
// suite timeout.
func TestUnlockCVNoLostWakeup(t *testing.T) {
	const iterations = 20000

	for i := range iterations {
		m := values.NewMutex("m")
		cv := values.NewConditionVariable("cv")
		var ready atomic.Bool
		done := make(chan struct{})

		go func() {
			defer close(done)
			m.Lock(nil, nil)
			for !ready.Load() {
				m.Unlock(cv, nil)
				m.Lock(nil, nil)
			}
			m.Unlock(nil, nil)
		}()

		m.Lock(nil, nil)
		ready.Store(true)
		cv.Signal()
		m.Unlock(nil, nil)

		timer := time.NewTimer(5 * time.Second)
		select {
		case <-done:
			timer.Stop()
		case <-timer.C:
			t.Fatalf("iteration %d: consumer never woke — the signal was lost "+
				"because the waiter was not enqueued before the mutex was released", i)
		}
	}
}

// lockContextResult is one waiter's return from LockContext, moved off the
// waiter goroutine so the driver can put a watchdog on it.
type lockContextResult struct {
	acquired bool
	err      error
}

// runLockContextWaiter launches one waiter on the already-held m under the
// supplied ctx and timeout, and reports whether that waiter returned within the
// watchdog. Reporting rather than failing keeps both arms of the lost-wakeup
// driver on one mechanism.
func runLockContextWaiter(ctx context.Context, m *values.Mutex, timeout *time.Duration, watchdog time.Duration) (lockContextResult, bool) {
	res := make(chan lockContextResult, 1)
	go func() {
		acquired, err := m.LockContext(ctx, timeout, nil)
		res <- lockContextResult{acquired: acquired, err: err}
	}()

	timer := time.NewTimer(watchdog)
	defer timer.Stop()
	select {
	case r := <-res:
		return r, true
	case <-timer.C:
		return lockContextResult{}, false
	}
}

// TestMutexLockContextNoLostWakeup drives both of LockContext's wait mechanisms
// under one driver: the TIMED path, whose wakeup comes from a timer goroutine
// launched next to cond.Wait, and the untimed CANCELLED path, whose wakeup comes
// from waitOnCondCtx. Every trial must resolve.
//
// The timed arm is the defect: its timer goroutine's Broadcast is lost whenever
// it lands between the `go` statement and cond.Wait's notifyListAdd, and the
// waiter then parks with nothing left to wake it. The cancelled arm is the
// control — same shape, same trials, same durations, but its Broadcast is taken
// under cond.L, so it must lose zero wakeups. Without the control the driver is
// not shown to discriminate between the two mechanisms.
//
// The bound was re-measured on this base rather than inherited: at 1 ms, the
// duration the review's driver reproduced at, 12 000 trials pass. The window
// being raced is the gap between the `go` statement and notifyListAdd, so
// shortening the timer is what widens it — 3 µs reproduces (trial 10596 of
// 12 000, unfixed), and the table keeps the review's longer durations as the
// slower-machine arms.
func TestMutexLockContextNoLostWakeup(t *testing.T) {
	const trials = 12000
	// Generous: the assertion is "returned at all", not "returned promptly".
	const watchdog = 5 * time.Second

	timeouts := []time.Duration{
		3 * time.Microsecond,
		20 * time.Microsecond,
		100 * time.Microsecond,
	}

	for _, timeout := range timeouts {
		t.Run("timed/"+timeout.String(), func(t *testing.T) {
			for i := range trials {
				m := values.NewMutex("timed")
				m.Lock(nil, nil) // held for the whole trial: the waiter can only time out

				d := timeout
				r, returned := runLockContextWaiter(context.Background(), m, &d, watchdog)
				if !returned {
					t.Fatalf("trial %d: waiter never returned from a %v timed acquire — the "+
						"timer goroutine broadcast before cond.Wait enqueued on the notify list", i, timeout)
				}
				qt.Assert(t, r.acquired, qt.IsFalse)
				qt.Assert(t, r.err, qt.IsNil)

				m.Unlock(nil, nil)
			}
		})

		t.Run("cancelled/"+timeout.String(), func(t *testing.T) {
			for i := range trials {
				m := values.NewMutex("cancelled")
				m.Lock(nil, nil)

				ctx, cancel := context.WithTimeout(context.Background(), timeout)
				r, returned := runLockContextWaiter(ctx, m, nil, watchdog)
				cancel()
				if !returned {
					t.Fatalf("trial %d: waiter never returned from a %v cancelled untimed acquire", i, timeout)
				}
				qt.Assert(t, r.acquired, qt.IsFalse)
				qt.Assert(t, r.err, qt.IsNil)

				m.Unlock(nil, nil)
			}
		})
	}
}
