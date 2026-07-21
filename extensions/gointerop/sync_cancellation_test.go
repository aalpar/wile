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

// Scheme-level guards for thread-terminate! unparking a thread blocked ACQUIRING
// a lock (rw-mutex, SRFI-18 mutex). Before values.RWMutex/Mutex acquisition went
// through the cond-based waitOnCondCtx bridge, these ops parked on Go sync
// primitives with no ctx-aware form: a terminated thread's goroutine stayed
// parked, done never closed, and thread-join! raised JoinTimeoutException.
// Reaching the terminated-thread exception proves the goroutine exited on ctx
// cancellation.
//
// A thread that HOLDS a lock is deliberately NOT covered: the held side is never
// force-released, so these tests park threads that are still WAITING to acquire.
//
// See docs/concurrency/cancellation.md.

package gointerop_test

import (
	"context"
	"errors"
	"testing"
	"time"

	extgointerop "github.com/aalpar/wile/extensions/gointerop"
	extthreads "github.com/aalpar/wile/extensions/threads"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// newThreadedEngine creates an engine with both the gointerop primitives and the
// SRFI-18 thread primitives. thread-terminate! / thread-join! live in the threads
// extension; with-timeout is a core primitive present in both.
func newThreadedEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extgointerop.Extension),
		wile.WithExtension(extthreads.Extension),
	)
	qt.New(t).Assert(err, qt.IsNil)
	t.Cleanup(func() {
		_ = engine.Close()
	})
	return engine
}

// TestTerminateUnparksBlockedSyncPrimitive is the leak guard: a thread parked
// acquiring a blocking lock is reaped by thread-terminate! rather than leaking its
// goroutine and timing out the join. Each case parks a thread whose acquire cannot
// succeed (the lock is held by the main thread), then terminates and joins it.
//
// The rendezvous is a lock-free `atomic` flag polled with thread-yield!, not a
// channel: channels were removed from the Scheme surface, and the worker signals
// before it parks. If terminate wins the race anyway, the worker's parked acquire
// sees an already-cancelled ctx and wakes immediately — same exit, no flake.
func TestTerminateUnparksBlockedSyncPrimitive(t *testing.T) {
	tcs := []struct {
		name  string
		ctor  string // constructs the shared `sync` binding
		setup string // runs on the main thread before the worker parks
		park  string // the blocking op in the worker's tail position
	}{
		{
			name:  "rw-mutex-write-lock! held by main",
			ctor:  `(make-rw-mutex)`,
			setup: `(rw-mutex-write-lock! sync)`,
			park:  `(rw-mutex-write-lock! sync)`,
		},
		{
			name:  "rw-mutex-read-lock! blocked by held write lock",
			ctor:  `(make-rw-mutex)`,
			setup: `(rw-mutex-write-lock! sync)`,
			park:  `(rw-mutex-read-lock! sync)`,
		},
		{
			name:  "mutex-lock! held by main (SRFI-18 untimed path)",
			ctor:  `(make-mutex)`,
			setup: `(mutex-lock! sync)`,
			park:  `(mutex-lock! sync)`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			engine := newThreadedEngine(t)

			code := `
(define ready (make-atomic #f))
(define sync ` + tc.ctor + `)
` + tc.setup + `
(define th (make-thread (lambda ()
  (atomic-store! ready 'here)
  ` + tc.park + `)))
(thread-start! th)
(let loop ()
  (if (eq? (atomic-load ready) 'here)
      #t
      (begin (thread-yield!) (loop))))
(thread-terminate! th)
(thread-join! th 5)
`
			expr, err := engine.Parse(context.Background(), "(begin "+code+" )")
			qt.Assert(t, err, qt.IsNil)
			_, err = engine.Eval(context.Background(), expr)

			qt.Assert(t, err, qt.IsNotNil)
			var terminated *values.TerminatedThreadException
			qt.Assert(t, errors.As(err, &terminated), qt.IsTrue,
				qt.Commentf("want the SRFI-18 terminated-thread exception; a "+
					"JoinTimeoutException here means the goroutine leaked on a "+
					"Go sync primitive with no cancellable form. got: %v", err))
		})
	}
}

// TestEmbedderDeadlineRWMutexRaisesCancelled is the wait-side guard for the source
// with no other protection: a rw-mutex-write-lock! parked under an embedder
// deadline raises the distinct ErrOperationCancelled. The single thread takes the
// write lock, then blocks re-taking it until the deadline cancels ctx.
func TestEmbedderDeadlineRWMutexRaisesCancelled(t *testing.T) {
	engine := newThreadedEngine(t)

	ctx, cancel := context.WithTimeout(context.Background(), 50*time.Millisecond)
	defer cancel()

	expr, err := engine.Parse(ctx,
		"(begin (define m (make-rw-mutex)) (rw-mutex-write-lock! m) (rw-mutex-write-lock! m) )")
	qt.Assert(t, err, qt.IsNil)

	_, err = engine.Eval(ctx, expr)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrOperationCancelled), qt.IsTrue,
		qt.Commentf("want the distinct cancellation sentinel; got %v", err))
}

// TestWithTimeoutInterruptsParkedRWMutex guards the ErrTimerExpired carve-out for
// the lock family: a rw-mutex-write-lock! parked inside a with-timeout runs the
// handler (returns its value) rather than the ErrOperationCancelled escaping.
func TestWithTimeoutInterruptsParkedRWMutex(t *testing.T) {
	engine := newThreadedEngine(t)

	result := eval(t, engine, `
(define m (make-rw-mutex))
(rw-mutex-write-lock! m)
(with-timeout 50
  (lambda (k) 'timed-out)
  (lambda () (rw-mutex-write-lock! m)))
`)

	qt.Assert(t, result.Internal(), valuestest.SchemeEquals, values.NewSymbol("timed-out"))
}

// TestTerminateUnparksCVWait is the cv-path analogue of
// TestTerminateUnparksBlockedSyncPrimitive: a thread parked in an UNTIMED
// (mutex-unlock! m cv) is reaped by thread-terminate! instead of leaking its
// goroutine. Before ConditionVariable wait grew a ctx arm, the parked <-ch had no
// cancellation edge: thread-terminate! cancelled the thread ctx but nothing closed
// the channel, so thread-join! raised JoinTimeoutException. Reaching the
// terminated-thread exception proves the goroutine exited on ctx cancellation.
func TestTerminateUnparksCVWait(t *testing.T) {
	engine := newThreadedEngine(t)

	code := `
(define ready (make-atomic #f))
(define m (make-mutex))
(define cv (make-condition-variable))
(define th (make-thread (lambda ()
  (mutex-lock! m)
  (atomic-store! ready 'here)
  (mutex-unlock! m cv))))
(thread-start! th)
(let loop ()
  (if (eq? (atomic-load ready) 'here)
      #t
      (begin (thread-yield!) (loop))))
(thread-terminate! th)
(thread-join! th 5)
`
	expr, err := engine.Parse(context.Background(), "(begin "+code+" )")
	qt.Assert(t, err, qt.IsNil)
	_, err = engine.Eval(context.Background(), expr)

	qt.Assert(t, err, qt.IsNotNil)
	var terminated *values.TerminatedThreadException
	qt.Assert(t, errors.As(err, &terminated), qt.IsTrue,
		qt.Commentf("want the SRFI-18 terminated-thread exception; a "+
			"JoinTimeoutException here means the goroutine leaked parked on the "+
			"condition variable with no cancellable form. got: %v", err))
}

// TestUnlockCVDeliversSignalAcrossThreads guards the SRFI-18 condition-wait
// rendezvous: a consumer that releases the mutex and waits on the cv is woken by a
// producer that (holding the same mutex) sets the predicate and signals. The
// consumer uses the standard predicate re-check loop with an UNTIMED wait, so a lost
// wakeup would hang here (caught by the test timeout). Because the waiter is now
// enqueued before the mutex is released, the signal the producer sends while blocked
// acquiring the mutex cannot be lost.
//
// The predicate is an atomic box, not a top-level `set!`: top-level bindings are
// immutable by default, so the mutation has to live in a mutable cell. The mutex
// still guards the predicate transition — the box is the storage, not the
// synchronization.
func TestUnlockCVDeliversSignalAcrossThreads(t *testing.T) {
	engine := newThreadedEngine(t)

	result := eval(t, engine, `
(define m (make-mutex))
(define cv (make-condition-variable))
(define ready (make-atomic #f))
(define consumer (make-thread (lambda ()
  (mutex-lock! m)
  (let loop ()
    (if (atomic-load ready)
        (begin (mutex-unlock! m) 'received)
        (begin (mutex-unlock! m cv) (mutex-lock! m) (loop)))))))
(define producer (make-thread (lambda ()
  (mutex-lock! m)
  (atomic-store! ready #t)
  (condition-variable-signal! cv)
  (mutex-unlock! m))))
(thread-start! consumer)
(thread-start! producer)
(thread-join! consumer)
`)
	qt.Assert(t, result.Internal(), valuestest.SchemeEquals, values.NewSymbol("received"))
}
