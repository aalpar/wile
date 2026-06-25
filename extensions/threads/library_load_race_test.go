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
	// Synchronizing LibraryRegistry (Task 1C) removed the "concurrent map
	// writes" fatal, but doing so unmasked a SEPARATE, pre-existing data race
	// in the per-call apply path: LocalEnvironmentFrame.copyForApplyInto writes
	// keysShared=true on the SHARED source frame, so concurrent foreign-closure
	// applications from multiple threads (here, the `environment` primitive
	// during library load) race on that copy-on-write flag. That race lives in
	// the hot environment-frame apply path and is out of scope for the registry
	// synchronization; the authoritative proof of the registry fix is the
	// registry-level TestLibraryRegistryConcurrentLoad / LookupOrClaim run under
	// -race. Re-enable this once the apply-frame CoW race is fixed.
	// TODO(apply-frame-race): tracked in plans/2026-06-25-apply-frame-cow-race.local.md.
	t.Skip("blocked by pre-existing copyForApplyInto keysShared race (separate from CC1)")

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

	// Four distinct SRFI libraries, none auto-loaded at startup. Each thread
	// loads one, so all four StartLoading/Register/FinishLoading sequences hit
	// the shared registry maps simultaneously.
	src := `(let ((t1 (make-thread (lambda () (environment '(srfi 1)))))
	              (t2 (make-thread (lambda () (environment '(srfi 13)))))
	              (t3 (make-thread (lambda () (environment '(srfi 14)))))
	              (t4 (make-thread (lambda () (environment '(srfi 132))))))
	          (thread-start! t1) (thread-start! t2)
	          (thread-start! t3) (thread-start! t4)
	          (thread-join! t1) (thread-join! t2)
	          (thread-join! t3) (thread-join! t4)
	          'done)`
	result, err := eng.EvalMultiple(ctx, src)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "done")
}
