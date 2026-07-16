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

// SRFI-18 regression guards for what a terminated thread reports.
//
// SRFI-18 gives thread-terminate! an outcome, not just an effect: it stores a
// terminated-thread exception in the thread's end-exception field, which
// thread-join! then raises. Wile stored that exception and then discarded it —
// Thread.Start's goroutine unconditionally overwrote the outcome when its thunk
// returned, so a terminated thread reported whatever its unwind produced. The
// existing terminate coverage could not see this: prim_threads_test.go's
// "terminate thread" case never starts the thread, and prim_thread_test.go's
// TestThreadTerminate never joins — both assert the #t literal they wrote.
//
// These tests assert the identity of the raised exception. Asserting only that
// *some* exception is raised would not have caught the bug: the overwritten
// outcome was the thunk's own ctx error (context.Canceled), which raises too.

package threads_test

import (
	"context"
	"errors"
	"testing"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestThreadTerminateStoresEndException pins SRFI-18: joining a thread that was
// terminated while running raises the terminated-thread exception itself, not
// whatever error or value the cancelled thunk happened to produce.
//
// The thread parks in thread-sleep! rather than a channel op, so this guards the
// SRFI-18 contract independently of the channel-cancellation coupling that
// exposed the bug (docs/concurrency/channel-cancellation.md). The two park
// differently on purpose: thread-sleep! surfaces ctx cancellation as an error,
// while a cancelled channel-receive launders it into an ordinary value. The
// outcome must be the same terminated-thread exception either way.
//
// The 10s sleep cannot elapse within the test, so the thread is still running
// when terminate lands. If terminate were to beat the goroutine to its first
// instruction the outcome is unchanged — terminate records the exception before
// the goroutine records anything, and the first writer wins — so this does not
// flake on scheduling.
func TestThreadTerminateStoresEndException(t *testing.T) {
	engine := newEngine(t)

	expr, err := engine.Parse(context.Background(), `
(let ((th (make-thread (lambda () (thread-sleep! 10) 'thunk-finished))))
  (thread-start! th)
  (thread-terminate! th)
  (thread-join! th 5))`)
	qt.Assert(t, err, qt.IsNil)

	_, err = engine.Eval(context.Background(), expr)

	qt.Assert(t, err, qt.IsNotNil)
	var terminated *values.TerminatedThreadException
	qt.Assert(t, errors.As(err, &terminated), qt.IsTrue,
		qt.Commentf("want SRFI-18's terminated-thread exception; a JoinTimeoutException "+
			"means the goroutine never exited, and any other exception means the "+
			"thunk's unwind overwrote the outcome Terminate stored. got: %v", err))
}

// TestThreadTerminateEndExceptionIsCatchable pins that joining a terminated
// thread raises into the joining thread's dynamic environment, where a guard can
// catch it, rather than escaping as a host error. PrimThreadJoin re-raises via
// RaiseInPlace specifically to make this true, and nothing else asserts it.
//
// This is a contract guard, not a regression guard for the outcome-overwrite
// bug: the overwritten outcome (the thunk's context.Canceled) is catchable too,
// so this test passes either way. Only the exception's identity separates them —
// TestThreadTerminateStoresEndException is what fails when the outcome is lost.
func TestThreadTerminateEndExceptionIsCatchable(t *testing.T) {
	engine := newEngine(t)

	result := eval(t, engine, `
(let ((th (make-thread (lambda () (thread-sleep! 10)))))
  (thread-start! th)
  (thread-terminate! th)
  (guard (e (#t 'terminated))
    (thread-join! th 5)))`)

	qt.Assert(t, result.Internal(), valuestest.SchemeEquals, values.NewSymbol("terminated"))
}

// TestThreadTerminateLeavesCompletedThreadAlone is the mirror guard, and the
// reason the fix is "first writer wins" rather than "terminate always wins".
//
// SRFI-18 scopes thread-terminate!'s effect to a thread that is "not already
// terminated"; a thread that ran to completion has an outcome already, and
// terminating it afterwards must not replace that result with an exception.
// Joining first forces completion, so this is not a race.
func TestThreadTerminateLeavesCompletedThreadAlone(t *testing.T) {
	engine := newEngine(t)

	result := eval(t, engine, `
(let ((th (make-thread (lambda () 42))))
  (thread-start! th)
  (thread-join! th 5)
  (thread-terminate! th)
  (thread-join! th 5))`)

	qt.Assert(t, result.Internal(), valuestest.SchemeEquals, values.NewInteger(42))
}
