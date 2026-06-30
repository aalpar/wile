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
	"testing"
	"time"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// TestConcurrentSameMacroExpansionHygiene closes the residual gap named in
// plans/2026-06-29-concurrent-library-load-crosscheck-followups.local.md.
// Prior -race coverage proved the syntax-rules transformer is race-free and that
// hygiene holds sequentially, plus a matcher-level concurrent clone test
// (match.TestSyntaxMatcher_CloneForMatchConcurrent) proved no cross-talk between
// clones. What was missing: a single test that concurrently expands the SAME
// hygienic macro through the FULL engine and asserts the OUTPUT's hygiene.
//
// A syntax-rules macro is compiled once at define-syntax time; its matcher is
// shared on the macro binding and reused for every expansion — operation_syntax_
// rules_transform.go runs clause.Matcher.CloneForMatch() per expansion, the fix
// (C) under test. Macro use sites expand at COMPILE time, so to force concurrent
// expansion of that one shared binding each thread evals a freshly built form
// (compiled on its own goroutine) that uses my-or.
//
// my-or introduces a binding t; the caller also binds t. With (my-or #f t) the
// first operand is false, so the result is the CALLER's t (= the thread's i):
//   - correct hygiene + no cross-talk -> i
//   - broken hygiene (macro's t captures the caller's t) -> #f
//   - shared capture stack across clones -> another thread's i
//
// Asserting results == inputs therefore catches all three failure modes at once.
// thread-join! is mapped in order, so the result list is deterministic even
// though the expansions ran concurrently (every thread is started before any
// join). Run under -race (the threads package is on the -race CI job).
//
// The interaction environment is captured ONCE on the main thread and shared,
// not fetched per thread: (interaction-environment) lazily writes the root
// namespace's Name on first call, so calling it concurrently is an unrelated
// data race in that primitive — hoisting it keeps this test pointed at the
// macro-expansion path under test.
func TestConcurrentSameMacroExpansionHygiene(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithLibraryPaths(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	const src = `
(define-syntax my-or
  (syntax-rules ()
    ((_ a b) (let ((t a)) (if t t b)))))

(define env (interaction-environment))

(define (expansion-thread i)
  (make-thread
    (lambda ()
      (eval (list 'let (list (list 't i)) (list 'my-or #f 't))
            env))))

(let* ((inputs '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
       (threads (map expansion-thread inputs)))
  (for-each thread-start! threads)
  (map thread-join! threads))`

	type evalResult struct {
		val wile.Value
		err error
	}
	done := make(chan evalResult, 1)
	go func() {
		val, evalErr := eng.EvalMultiple(ctx, src)
		done <- evalResult{val, evalErr}
	}()
	select {
	case r := <-done:
		c.Assert(r.err, qt.IsNil)
		// Each thread must echo its own caller-bound t — no #f (hygiene break)
		// and no cross-thread value (shared capture state).
		c.Assert(r.val.SchemeString(), qt.Equals,
			"(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)")
	case <-time.After(30 * time.Second):
		t.Fatal("concurrent same-macro expansion did not complete in 30s")
	}
}
