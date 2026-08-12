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
	"runtime"
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

	// Unlocked → 'not-abandoned. NOT 'not-owned, and the difference is the
	// whole point: SRFI-18's two unowned answers are distinct.
	//
	// This assertion used to read 'not-owned, with a comment calling the
	// sameness "the load-bearing SRFI-18 collapse the refactor relies on".
	// That belief was wrong, and it is why item 17 was filed as a
	// REPRESENTATION problem: MutexState plus the owner field always encoded
	// all four SRFI-18 answers faithfully — MutexAbandoned is not a held
	// state, both wait loops exit on it exactly as on MutexUnlocked — and the
	// collapse existed only in this renderer, which returned 'not-owned from
	// three branches and left 'not-abandoned unreachable.
	sv := m.StateValue()
	qt.Assert(t, sv, valuestest.SchemeEquals, values.NewSymbol("not-abandoned"))

	// Locked with nil owner → 'not-owned. THIS is what 'not-owned means: the
	// mutex IS held, by a caller with no Thread object — mutex-lock! takes an
	// optional owner and #f is legal, and the primordial thread has none. A
	// regression that drops the owner-nil check inside the MutexLocked branch
	// fails here rather than at a far-away user-reported bug.
	m.Lock(nil, nil)
	sv = m.StateValue()
	qt.Assert(t, sv, valuestest.SchemeEquals, values.NewSymbol("not-owned"))

	// The discrimination itself, asserted directly: held-with-no-owner and
	// not-held must never render the same. One symbol for both is exactly the
	// defect, and it is invisible to any test that checks only one of them.
	held := m.StateValue()
	m.Unlock(nil, nil)
	notHeld := m.StateValue()
	qt.Assert(t, held.EqualTo(notHeld), qt.IsFalse,
		qt.Commentf("held-unowned rendered as %v, not-held as %v", held, notHeld))

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
			name:      "locked-with-owner → abandoned, owner retained",
			setup:     func(m *values.Mutex) { m.Lock(nil, th) },
			wantState: values.MutexAbandoned,
			wantOwner: true,
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
			name: "already-abandoned → no-op (stays abandoned, owner still retained)",
			setup: func(m *values.Mutex) {
				m.Lock(nil, th)
				m.MarkAbandoned()
			},
			wantState: values.MutexAbandoned,
			wantOwner: true,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			m := values.NewMutex(tc.name)
			tc.setup(m)
			m.MarkAbandoned()
			qt.Assert(t, m.State(), qt.Equals, tc.wantState)
			// Owner is an independent axis, not a function of the state.
			// Abandonment KEEPS it — the thread that died holding the mutex is
			// the one fact worth having about it, and dropping it was the whole
			// of the loss. The nil rows are nil because they were locked with a
			// nil owner or never locked, not because abandonment cleared one.
			ownerNonNil := m.Owner() != nil
			qt.Assert(t, ownerNonNil, qt.Equals, tc.wantOwner)
			// Retained, but never rendered: SRFI-18 gives the thread answer to
			// the held-and-owned case alone.
			if tc.wantState == values.MutexAbandoned {
				qt.Assert(t, m.StateValue(), valuestest.SchemeEquals, values.NewSymbol("abandoned"))
			}
		})
	}
}

// TestMutex_UnlockClearsRetainedOwner pins the other end of retention: an owner
// that survives abandonment must not survive the next release, or a released
// mutex would keep naming a thread that has nothing to do with it.
func TestMutex_UnlockClearsRetainedOwner(t *testing.T) {
	m := values.NewMutex("retain")
	th := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "owner")

	m.Lock(nil, th)
	m.MarkAbandoned()
	qt.Assert(t, m.Owner(), qt.Equals, th)

	m.Unlock(nil, nil)
	qt.Assert(t, m.State(), qt.Equals, values.MutexUnlocked)
	qt.Assert(t, m.Owner() == nil, qt.IsTrue)
	qt.Assert(t, m.StateValue(), valuestest.SchemeEquals, values.NewSymbol("not-abandoned"))
}

// TestMutex_AbandonedAcquireReplacesRetainedOwner pins that retention does not
// outlive its usefulness the other way either: the next acquirer owns the
// mutex, and the dead thread's identity goes with the state it belonged to.
func TestMutex_AbandonedAcquireReplacesRetainedOwner(t *testing.T) {
	m := values.NewMutex("retain-acquire")
	dead := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "dead")
	live := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "live")

	m.Lock(nil, dead)
	m.MarkAbandoned()

	ok, err := m.Lock(nil, live)
	qt.Assert(t, ok, qt.IsTrue)
	var abandoned *values.AbandonedMutexException
	qt.Assert(t, errors.As(err, &abandoned), qt.IsTrue)
	qt.Assert(t, m.Owner(), qt.Equals, live)
	qt.Assert(t, m.StateValue(), qt.Equals, values.Value(live))
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

// runLockContextWaiters launches n concurrent waiters on the already-held m and
// collects the ones that return within the watchdog. A short return slice is a
// lost wakeup; reporting it rather than failing here keeps both arms of the
// lost-wakeup driver on one mechanism.
//
// n > 1 is load-bearing, not throughput. The window being raced is the gap
// between the `go` statement and cond.Wait's notifyListAdd, a few hundred
// nanoseconds, and it is only entered when the waiter is descheduled inside it.
// One waiter at a time on an idle machine is almost never descheduled there —
// measured: 36 000 sequential single-waiter trials across three durations found
// nothing on an idle box, while the same driver reproduced within 12 000 when
// something else was loading the machine. Saturating the Ps is what makes the
// interleaving reachable without depending on what else happens to be running.
func runLockContextWaiters(ctx context.Context, m *values.Mutex, n int, timeout *time.Duration, watchdog time.Duration) []lockContextResult {
	res := make(chan lockContextResult, n)
	for range n {
		go func() {
			acquired, err := m.LockContext(ctx, timeout, nil)
			res <- lockContextResult{acquired: acquired, err: err}
		}()
	}

	timer := time.NewTimer(watchdog)
	defer timer.Stop()

	q := make([]lockContextResult, 0, n)
	for len(q) < n {
		select {
		case r := <-res:
			q = append(q, r)
		case <-timer.C:
			return q
		}
	}
	return q
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
// The bound was re-measured on this base rather than inherited. At 1 ms, the
// duration the review's driver reproduced at, 12 000 single-waiter trials pass.
// Shortening the timer widens the window, and 3 µs and 20 µs both reproduced —
// but only while the machine was loaded by other work; on an idle box the same
// 36 000 trials found nothing. Concurrency is what removes that dependence: each
// trial launches waiters on every P, so the descheduling the race needs is
// produced by the driver rather than borrowed from whatever else is running.
func TestMutexLockContextNoLostWakeup(t *testing.T) {
	const trials = 1500
	// Enough to keep every P busy; trials × waiters is the acquire count.
	waiters := 8 * runtime.GOMAXPROCS(0)
	// Generous: the assertion is "returned at all", not "returned promptly".
	const watchdog = 5 * time.Second

	timeouts := []time.Duration{
		3 * time.Microsecond,
		20 * time.Microsecond,
		100 * time.Microsecond,
	}

	assertAllTimedOut := func(t *testing.T, q []lockContextResult) {
		t.Helper()
		for _, r := range q {
			qt.Assert(t, r.acquired, qt.IsFalse)
			qt.Assert(t, r.err, qt.IsNil)
		}
	}

	for _, timeout := range timeouts {
		t.Run("timed/"+timeout.String(), func(t *testing.T) {
			for i := range trials {
				m := values.NewMutex("timed")
				m.Lock(nil, nil) // held for the whole trial: the waiters can only time out

				d := timeout
				q := runLockContextWaiters(context.Background(), m, waiters, &d, watchdog)
				if len(q) != waiters {
					t.Fatalf("trial %d: %d of %d waiters never returned from a %v timed acquire — the "+
						"timer goroutine broadcast before cond.Wait enqueued on the notify list",
						i, waiters-len(q), waiters, timeout)
				}
				assertAllTimedOut(t, q)

				m.Unlock(nil, nil)
			}
		})

		t.Run("cancelled/"+timeout.String(), func(t *testing.T) {
			for i := range trials {
				m := values.NewMutex("cancelled")
				m.Lock(nil, nil)

				ctx, cancel := context.WithTimeout(context.Background(), timeout)
				q := runLockContextWaiters(ctx, m, waiters, nil, watchdog)
				cancel()
				if len(q) != waiters {
					t.Fatalf("trial %d: %d of %d waiters never returned from a %v cancelled untimed acquire",
						i, waiters-len(q), waiters, timeout)
				}
				assertAllTimedOut(t, q)

				m.Unlock(nil, nil)
			}
		})
	}
}
