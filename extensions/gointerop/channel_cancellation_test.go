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

// Scheme-level guards for the channel/VM cancellation coupling documented in
// docs/concurrency/channel-cancellation.md.
//
// pkg/values/channel_lifecycle_test.go proves the Go-level contract: a blocking
// Send/Receive returns on a raw context.WithCancel. It cannot reach the two
// integrations that make the Option A "cancelled is surfaced as closed" policy
// safe, because both live above the values package — the with-timeout timer
// interrupt and the SRFI-18 thread ctx. These tests drive both from Scheme.

package gointerop_test

import (
	"context"
	"errors"
	"testing"

	extgointerop "github.com/aalpar/wile/extensions/gointerop"
	extthreads "github.com/aalpar/wile/extensions/threads"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// newThreadedEngine creates an engine with both the gointerop channel
// primitives and the SRFI-18 thread primitives. The package-level newEngine
// loads gointerop only; thread-terminate! / thread-join! live in the threads
// extension, and with-timeout is a core primitive present in both.
func newThreadedEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extgointerop.Extension),
		wile.WithExtension(extthreads.Extension),
	)
	qt.New(t).Assert(err, qt.IsNil)
	return engine
}

// TestWithTimeoutInterruptsParkedReceive is the regression guard for the eager
// ErrTimerExpired recheck in callForeignCached (pkg/machine/call_foreign_cached.go).
//
// A with-timeout whose thunk parks in channel-receive cancels mc.Context() with
// cause ErrTimerExpired. Channel.Receive selects on ctx.Done() and returns
// RecvCancelled, which PrimChannelReceive launders into Void (Option A) — the
// same value a legitimately closed-and-drained channel produces. Nothing in the
// channel or primitive layer distinguishes the two. What makes the composition
// correct is non-local: callForeignCached rechecks ctx.Done() after every
// foreign return and, on ErrTimerExpired, raises ErrTimerInterrupt *before* the
// laundered Void is consumed, so the timeout handler runs.
//
// Asserting the handler's value (and not Void) is therefore the only test in the
// repo that fails if that recheck is removed or narrowed to exclude this path.
func TestWithTimeoutInterruptsParkedReceive(t *testing.T) {
	engine := newThreadedEngine(t)

	// The channel is empty and has no sender, so the receive parks until the
	// 50ms timer fires. The handler's value becomes the with-timeout result.
	result := eval(t, engine, `
(define ch (make-channel))
(with-timeout 50
  (lambda (k) 'timed-out)
  (lambda () (channel-receive ch)))
`)

	qt.Assert(t, result.Internal(), valuestest.SchemeEquals, values.NewSymbol("timed-out"))
}

// TestTerminateUnparksBlockedThread is the guard for the ctx goroutine-leak
// (T1.3) and for the SRFI-18 outcome of the thread it unparks.
//
// thread-join! IS the goroutine-exit handshake, not merely an outcome check:
// Thread.Join blocks on the thread's done channel, which is closed by a defer
// registered first in Thread.Start and therefore run last. Before the
// done-channel lifecycle, the parked receive ignored ctx, the goroutine never
// returned, done never closed, and this join would raise JoinTimeoutException
// after its 5s timeout. Reaching any join outcome proves the goroutine exited.
//
// Both cases park identically and differ only in whether a VM op follows the
// receive — the distinction that decides which mechanism ends the thread. In
// tail position nothing follows, so the VM's top-of-loop ctx check never runs
// and the thunk returns the cancelled receive's laundered Void; with ops
// following, the ctx check unwinds the thread within contextCheckMask (≈1024)
// ops. Neither mechanism may be visible from Scheme: a terminated thread's
// outcome is the SRFI-18 terminated-thread exception either way, so both cases
// assert the same thing. The tail case is the one that regressed — the thunk's
// normal return overwrote the exception Terminate had stored, and the joiner
// saw a terminated thread report success.
func TestTerminateUnparksBlockedThread(t *testing.T) {
	tcs := []struct {
		name string
		body string
	}{
		{
			name: "receive in tail position of the thunk",
			body: `(channel-receive parked)`,
		},
		{
			name: "ops follow the receive",
			body: `(let ((v (channel-receive parked)))
			         (let loop ((i 0))
			           (if (< i 100000) (loop (+ i 1)) 'unreachable)))`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			engine := newThreadedEngine(t)

			// ready rendezvouses with the thread so terminate lands after it has
			// begun the parked receive. If terminate were to win the race anyway,
			// the receive's ctx.Done() arm is already ready and it returns
			// RecvCancelled immediately — same exit, so the test does not flake.
			// parked is empty with no sender, so the receive blocks.
			code := `
(define ready (make-channel))
(define parked (make-channel))
(define th (make-thread (lambda ()
  (channel-send! ready 'here)
  ` + tc.body + `)))
(thread-start! th)
(channel-receive ready)
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
					"JoinTimeoutException here means the goroutine leaked, any "+
					"other outcome means Terminate's exception was overwritten. got: %v", err))
		})
	}
}
