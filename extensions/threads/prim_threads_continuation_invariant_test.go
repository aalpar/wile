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

package threads_test

import (
	"context"
	"errors"
	"testing"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/werr"

	qt "github.com/frankban/quicktest"
)

// TestCrossThreadContinuationIsAllocatorInvariant pins thread-confinement of
// continuations to the specific werr.ErrCrossThreadContinuation sentinel, for
// BOTH continuation flavors that carry their own thread check:
//
//   - captured (escape) continuations from call/cc       — applyCapturedContinuation
//   - composable continuations from call-with-composable  — applyComposableContinuation
//
// WHY THIS IS LOAD-BEARING (do not relax without reading
// plans/2026-06-08-per-thread-pools-invariant.md):
//
// The per-thread frame-pool design anchors allocation pools at each thread's
// root MachineContext, with NO mutex, precisely because a frame allocated by
// thread A's execution is only ever released by thread A. The only way thread B
// could release an A-allocated frame is by *executing* a continuation captured
// in A — which these checks forbid. If this rejection is ever weakened to allow
// cross-thread continuation invocation (e.g. for a work-stealing scheduler),
// the per-thread pool design becomes unsafe and must be reworked first.
//
// TestCrossThreadContinuationRejection already covers the call/cc behavior with
// a generic "is an error" assertion. This test is stronger on purpose: it asserts
// the *exact* sentinel, so a regression that swaps the rejection for some other
// incidental failure (or removes it and fails for an unrelated reason) is caught
// rather than masked.
func TestCrossThreadContinuationIsAllocatorInvariant(t *testing.T) {
	engine := newEngineWithExceptions(t)
	tcs := []struct {
		name string
		code string
	}{
		{
			// call/cc captures an escape (captured) continuation in t1;
			// invoking it from the primordial thread must be rejected.
			name: "captured continuation (call/cc) rejected across threads",
			code: `(let* ((k #f)
			               (t1 (make-thread
			                     (lambda ()
			                       (call/cc (lambda (cont) (set! k cont) 'captured))))))
			          (thread-start! t1)
			          (thread-join! t1)
			          (k 42))`,
		},
		{
			// call-with-composable-continuation captures a composable
			// continuation in t1; invoking it from the primordial thread must
			// be rejected by the separate check in applyComposableContinuation.
			name: "composable continuation rejected across threads",
			code: `(let* ((k #f)
			               (tag (make-continuation-prompt-tag))
			               (t1 (make-thread
			                     (lambda ()
			                       (call-with-continuation-prompt
			                         (lambda ()
			                           (call-with-composable-continuation
			                             (lambda (cont) (set! k cont) 'captured)
			                             tag))
			                         tag
			                         (lambda (v) v))))))
			          (thread-start! t1)
			          (thread-join! t1)
			          (k 42))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectSentinel(t, engine, tc.code, werr.ErrCrossThreadContinuation)
		})
	}
}

// evalExpectSentinel evaluates code expected to fail, and asserts the error
// wraps the given sentinel (errors.Is), not merely that some error occurred.
func evalExpectSentinel(t *testing.T, engine *wile.Engine, code string, sentinel error) {
	t.Helper()
	c := qt.New(t)
	expr, err := engine.Parse(context.Background(), code)
	c.Assert(err, qt.IsNil)
	_, err = engine.Eval(context.Background(), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, sentinel), qt.IsTrue,
		qt.Commentf("expected error wrapping %v, got: %v", sentinel, err))
}
