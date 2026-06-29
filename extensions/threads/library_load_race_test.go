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

// TestConcurrentLibraryLoadFromThreads is the end-to-end companion to the
// registry-level TestLibraryRegistryConcurrentLoad (CC1): SRFI-18 threads that
// share the engine's library registry each load a distinct, not-preloaded
// library via (environment …) concurrently. Before LibraryRegistry was
// synchronized this aborted the process with "concurrent map writes" (a Go map
// is unsafe for concurrent writes even to different keys). Run under -race
// (the threads package is on the -race CI job) to surface the data race even
// when no crash happens.
//
// The authoritative proof of the synchronization fix is the registry unit test
// run under -race; this is the realistic integration smoke that exercises the
// same maps through the full (environment …) → LoadLibrary → Register path.
func TestConcurrentLibraryLoadFromThreads(t *testing.T) {
	// Progress so far (plans/2026-06-25-apply-frame-cow-race.local.md):
	//   1. apply-frame copy-on-write race (keysShared) — FIXED, proven by
	//      environment.TestCopyForApplyInto_ConcurrentSourceRaceFree.
	//   2. false-circular-dependency on concurrent shared-dependency load — FIXED
	//      by the per-name load latch (LibraryRegistry.LookupClaimOrWait), with
	//      genuine cycles caught via the ctx-borne load chain (loadChainContains).
	//      Proven by TestLibraryRegistryLookupClaimOrWait /
	//      TestLibraryRegistryFinishLoadingClosesLatch under -race.
	//
	// Un-skipping now surfaces TWO further engine-global shared-state hazards that
	// the latch does not address (Q2's "find the next one" — each a separate task):
	//   A. registry.ApplyDocs concurrent map write (apply.go:300): every library
	//      load runs applyBaseEnvironment→ApplyDocs, mutating bnd.EnsureMeta().Doc
	//      on bindings that resolve up to the SHARED sealed base — concurrent
	//      writes to the same base binding. A real data race under -race.
	//   B. shared LoadPathStack: include resolves relative to the stack top, but
	//      all threads push/pop one stack on the root namespace, so a thread
	//      resolves its (include …) against another thread's directory → file not
	//      found. A logic race (no -race report), needs a per-load-chain stack.
	// Both are tracked in the plan's "Still OPEN" section. Keep skipped until they
	// land; the latch fix itself is proven by the registry unit tests above.
	t.Skip("blocked by engine-global shared-state under concurrent load: (A) registry.ApplyDocs map write on the shared base; (B) shared LoadPathStack breaks per-thread include resolution. The per-name latch is fixed and proven by pkg/machine/compilation TestLibraryRegistryLookupClaimOrWait under -race.")

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

	// Four distinct SRFI libraries, none auto-loaded at startup, each depending on
	// the shared (scheme base). Each thread loads one, so all four claim the
	// registry concurrently AND contend on (scheme base): exactly one thread loads
	// it; the rest wait on its latch rather than failing with a false cycle.
	src := `(let ((t1 (make-thread (lambda () (environment '(srfi 1)))))
	              (t2 (make-thread (lambda () (environment '(srfi 13)))))
	              (t3 (make-thread (lambda () (environment '(srfi 14)))))
	              (t4 (make-thread (lambda () (environment '(srfi 132))))))
	          (thread-start! t1) (thread-start! t2)
	          (thread-start! t3) (thread-start! t4)
	          (thread-join! t1) (thread-join! t2)
	          (thread-join! t3) (thread-join! t4)
	          'done)`

	// Watchdog: a per-name-latch regression that deadlocks would otherwise hang
	// the whole package until the global -timeout. Fail fast with a clear message.
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
		c.Assert(r.val.SchemeString(), qt.Equals, "done")
	case <-time.After(30 * time.Second):
		t.Fatal("concurrent library load did not complete in 30s — possible per-name latch deadlock")
	}
}
