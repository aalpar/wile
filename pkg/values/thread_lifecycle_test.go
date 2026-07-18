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

package values

import (
	"context"
	"errors"
	"sync"
	"testing"
	"time"
)

// TestThread_ConcurrentStartTerminate_NoDoubleClose is the permanent guard for
// the two claims that let Terminate close the done channel itself.
//
// Terminate must close done when it ends a thread that was never started —
// nothing else ever will, and Join parks on done before reading the outcome, so
// such a thread would block its joiner forever. That makes two closers, and a
// double close of the done channel is a fatal host panic no recover boundary can
// catch.
//
// The two closers are claimed to be mutually exclusive, and done is claimed to
// always close. Both rest on Start making the ThreadNew -> ThreadRunnable
// transition under p.mu before spawning, and refusing any other state:
//
//   - Start wins the lock: it spawns, and its goroutine owns the close. Terminate
//     then observes ThreadRunnable, so it does not close.
//   - Terminate wins: it observes ThreadNew, closes done, and leaves the thread
//     ThreadTerminated. Start then refuses and never spawns.
//
// This races the two on the same thread. Reaching the end of every trial without
// a host panic is the no-double-close assertion; the receive on done is the
// always-closes assertion, and would hang on a missed close rather than pass.
//
// Both racers wait on a starting gate rather than one calling inline. Called
// inline, Terminate wins nearly every trial (measured: only 5/20000 reached the
// spawn), which left the Start-wins arm — where the goroutine must own the
// close — all but untested while the loop still read as a race. The gate lifts
// that to ~190/20000. It stays lopsided because Terminate has less to do than
// Start, so the split is the scheduler's business and nothing is asserted about
// it; TestThread_TerminateAfterStart_GoroutineOwnsDone pins that arm
// deterministically, and TestThread_TerminateBeforeStart_ClosesDone the other.
func TestThread_ConcurrentStartTerminate_NoDoubleClose(t *testing.T) {
	const trials = 20000
	for range trials {
		th := NewThread(nil, "race")
		th.RunFunc = func(_ context.Context, _ Callable) (Value, error) {
			return TrueValue, nil
		}

		var gate, wg sync.WaitGroup
		gate.Add(1)
		wg.Add(2)
		go func() {
			defer wg.Done()
			gate.Wait()
			// Whichever side loses is a no-op, so the error is not the subject.
			_ = th.Start(context.Background())
		}()
		go func() {
			defer wg.Done()
			gate.Wait()
			th.Terminate()
		}()
		gate.Done()
		wg.Wait()

		select {
		case <-th.done:
		case <-time.After(10 * time.Second):
			t.Fatal("done was never closed: Start and Terminate each assumed the other owned it")
		}
	}
}

// TestThread_TerminateAfterStart_GoroutineOwnsDone is the deterministic
// Start-wins arm: once Start has transitioned the thread out of ThreadNew,
// Terminate must leave the close to the spawned goroutine. If it closed done
// here too, the goroutine's own deferred close would panic the host.
//
// The thunk blocks until released, so the goroutine is provably still running
// (and has not yet reached its deferred close) when Terminate lands.
func TestThread_TerminateAfterStart_GoroutineOwnsDone(t *testing.T) {
	release := make(chan struct{})
	running := make(chan struct{})
	th := NewThread(nil, "started")
	th.RunFunc = func(_ context.Context, _ Callable) (Value, error) {
		close(running)
		<-release
		return TrueValue, nil
	}

	err := th.Start(context.Background())
	if err != nil {
		t.Fatalf("Start: %v", err)
	}
	<-running

	th.Terminate()
	close(release)

	select {
	case <-th.done:
	case <-time.After(10 * time.Second):
		t.Fatal("done was never closed after terminating a started thread")
	}
}

// TestThread_TerminateBeforeStart_ClosesDone pins the deterministic half of the
// race above: with no contention, terminating a never-started thread closes done
// and Join reports the SRFI-18 terminated-thread exception rather than parking.
// Start must then refuse, leaving the exception intact.
func TestThread_TerminateBeforeStart_ClosesDone(t *testing.T) {
	th := NewThread(nil, "never-started")
	th.RunFunc = func(_ context.Context, _ Callable) (Value, error) {
		return TrueValue, nil
	}

	th.Terminate()

	startErr := th.Start(context.Background())
	if startErr == nil {
		t.Fatal("Start on a terminated thread should be refused; if it spawned, its goroutine would double-close done")
	}

	timeout := 10 * time.Second
	_, err := th.Join(&timeout)

	// UncaughtThreadException.Unwrap exposes the stored cause, so one errors.As
	// reaches through the wrapper a JoinTimeoutException would never carry.
	var terminated *TerminatedThreadException
	if !errors.As(err, &terminated) {
		t.Fatalf("Join: want the stored TerminatedThreadException, got %v", err)
	}
}
