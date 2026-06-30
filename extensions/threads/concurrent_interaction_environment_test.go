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

// TestConcurrentInteractionEnvironment is the regression guard for the
// lazy-init data race in PrimInteractionEnvironment: a "read-only" query that
// labeled the shared root namespace's Name on first call, so concurrent
// (interaction-environment) calls from SRFI-18 threads each read "" and wrote
// it. The fix names the engine's top level eagerly at init (engine.go) and makes
// the primitive a pure read.
//
// Each thread calls (interaction-environment) and reads its name; with the fix
// every thread sees "interaction-environment" and there is no write to race on.
// The assertion counts correct results (8) so a regression to the lazy write —
// only race-free if the eager name is also kept — trips -race here. Run under
// -race (the threads package is on the -race CI job).
func TestConcurrentInteractionEnvironment(t *testing.T) {
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
(define (probe)
  (make-thread
    (lambda ()
      (namespace-name (interaction-environment)))))

(let ((threads (map (lambda (i) (probe)) '(1 2 3 4 5 6 7 8))))
  (for-each thread-start! threads)
  (let ((names (map thread-join! threads)))
    (apply + (map (lambda (n) (if (equal? n "interaction-environment") 1 0)) names))))`

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
		// All eight threads must read the eagerly-set name; no #f, no race.
		c.Assert(r.val.SchemeString(), qt.Equals, "8")
	case <-time.After(30 * time.Second):
		t.Fatal("concurrent interaction-environment probe did not complete in 30s")
	}
}
