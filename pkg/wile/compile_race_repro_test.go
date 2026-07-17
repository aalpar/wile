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

package wile

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/stdlib"
)

// TestCompileDefineMetaRace guards the fix for a CONFIRMED data race (T1.5,
// 2026-07-17, fixed by the atomic-meta migration): two SRFI-18 threads eval'ing a
// define of the SAME top-level name raced on Binding.meta. It runs in the normal
// suite; the teeth are in the -race build (make test-race):
//
//	go test ./pkg/wile/ -run TestCompileDefineMetaRace -race
//
// THE SHAPE (why this specific test). Every CONTAINER on the compile path was
// already locked — GlobalEnvironmentFrame.mu protects the keys map and the
// bindings slice — but once a *Binding was handed out, meta was a plain pointer and
// Stable/Imported plain fields. Binding.value got an atomicCell because someone
// traced that reader; meta never did, because it was assumed compile-time-only —
// true, and irrelevant, since the race is compile-vs-compile, reachable once
// SRFI-18 threads could each call eval. The fix moves a global binding's meta into
// the atomicCell too, published copy-on-write via Binding.UpdateMeta.
//
// WHY A SISTER GUARD DID NOT CATCH IT. TestCompilePathIsSafeUnderConcurrentThreads
// mints DISTINCT names per thread, so no two threads ever touch the same *Binding.
// Sharing the NAME is the whole repro; that is the one variable it holds constant.
func TestCompileDefineMetaRace(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink),
		WithSourceFS(stdlib.FS), WithLibraryPaths("."))
	if err != nil {
		t.Fatalf("engine: %v", err)
	}
	defer func() {
		_ = eng.Close()
	}()

	// Eight threads, each re-defining the one shared name. The define path drops
	// import provenance and writes Scopes/Source/Stable through Binding.UpdateMeta,
	// which publishes atomically rather than mutating a shared *BindingMeta in place.
	_, err = eng.EvalMultiple(ctx, `
(define e (environment '(scheme base)))
(define (spawn i)
  (make-thread
    (lambda ()
      (let loop ((k 0))
        (if (< k 200)
            (begin
              (eval (list 'define 'shared k) e)
              (loop (+ k 1)))
            'done)))))
(define ts (map spawn '(0 1 2 3 4 5 6 7)))
(for-each thread-start! ts)
(for-each thread-join! ts)
'ok`)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
}
