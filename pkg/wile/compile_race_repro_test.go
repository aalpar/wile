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
	"os"
	"testing"

	"github.com/aalpar/wile/pkg/stdlib"
)

// TestCompileDefineMetaRace is a GATED REPRO of a CONFIRMED data race (T1.5,
// 2026-07-17). It is skipped unless WILE_RACE_REPRO=1, so CI stays green while the
// finding is open:
//
//	WILE_RACE_REPRO=1 go test ./pkg/wile/ -run TestCompileDefineMetaRace -race
//
// When the race is fixed, delete the gate and invert this into a permanent guard.
//
// THE FINDING. Two SRFI-18 threads eval'ing a define of the SAME top-level name race
// on Binding.meta. This violates the ownership line (the compiler is engine
// machinery; a user never synchronizes it), so it is a defect, not a documented
// user responsibility like sharing a vector.
//
// Confirmed stack:
//
//	environment.(*Binding).IsImported()                binding.go:285
//	compilation.(*CompileTimeContinuation).declareDefineBinding()  compile_define.go:103
//	compilation.CompileValidatedDefine()              compile_define.go:35
//	compilation.ExpandAndCompile()                    expand_and_compile.go:54
//
// THE SHAPE. Every CONTAINER on the compile path is correctly locked; the gap is
// uniform and structural. GlobalEnvironmentFrame.mu protects the keys map and the
// bindings slice — but once a *Binding is handed out, meta is a plain pointer and
// Stable/Imported are plain fields. Binding.value got an atomicCell precisely
// because someone traced that reader; meta never did, because it was assumed
// compile-time-only. That assumption is stated at binding.go:24 ("never read during
// VM execution") and it is TRUE — and irrelevant. The race is compile-vs-compile,
// not compile-vs-run, and concurrent compiles became reachable when SRFI-18 threads
// could each call eval.
//
// WHY THE PASSING GUARD DID NOT CATCH IT. TestCompilePathIsSafeUnderConcurrentThreads
// mints DISTINCT names per thread, so no two threads ever touch the same *Binding.
// It is green, its teeth are verified, and it is blind to exactly this. Sharing the
// NAME is the whole repro; that is the one variable it holds constant.
func TestCompileDefineMetaRace(t *testing.T) {
	if os.Getenv("WILE_RACE_REPRO") != "1" {
		t.Skip("gated race repro; set WILE_RACE_REPRO=1 and run with -race")
	}
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink),
		WithSourceFS(stdlib.FS), WithLibraryPaths("."))
	if err != nil {
		t.Fatalf("engine: %v", err)
	}
	defer func() {
		_ = eng.Close()
	}()

	// Eight threads, each re-defining the one shared name. The define path runs
	// binding.EnsureMeta().Imported = false and then writes Scopes/Source/Stable,
	// none of it under the frame lock.
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
	t.Log("no race reported — if this holds under -race, the finding may be fixed")
}
