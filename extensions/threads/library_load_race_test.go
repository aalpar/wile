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
	// FIXED on this branch (the two hazards the original skip named, Q2's first two):
	//   A. registry.ApplyDocs concurrent write on the shared sealed base — gated off
	//      for flat library frames in applyBaseEnvironment (engine.go), mirroring the
	//      registerSchemeDocstrings guard. The same idempotent-shared-write class in
	//      libEnv.SetLibraryRegistry (library_loader.go, compile_library_forms.go) is
	//      now guarded on namespace identity.
	//   B. shared LoadPathStack — library loading and (include …) now carry a
	//      per-load-chain LoadStack on ctx (sourceload.WithLoadStack), read by the
	//      FS/OS resolvers, so concurrent threads resolve includes against their own
	//      directory. ctx does not cross the SRFI-18 goroutine boundary.
	//
	// STILL OPEN (Q2's next sibling, a different subsystem): concurrent macro
	// expansion races shared syntax-rules transformer state. A clause's
	// *match.SyntaxMatcher is compiled once at macro-definition time and stored on
	// the shared macro binding, but the underlying Matcher carries per-invocation
	// MUTABLE state (captureStack, syntaxStack, tailCountCache) and SyntaxMatcher
	// sets/clears bindingChecker per call (syntax_adapter.go:167). Library
	// compilation pervasively expands macros (let / named-let bodies), so two threads
	// compiling libraries concurrently corrupt the shared matcher. The fix separates
	// the immutable compiled pattern from per-invocation matching state (e.g. clone
	// the matcher per invocation) — a delicate macro-subsystem change on the hottest
	// compile path, with single-thread perf implications. Tracked in
	// plans/2026-06-25-apply-frame-cow-race.local.md ("Still OPEN").
	t.Skip("blocked by concurrent macro-expansion races on shared syntax-rules matcher state (match.SyntaxMatcher; syntax_adapter.go:167). The two originally-named hazards (registry.ApplyDocs shared-base write; shared LoadPathStack include resolution) are FIXED on this branch and proven race-free up to the macro-expansion frontier under -race.")

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
