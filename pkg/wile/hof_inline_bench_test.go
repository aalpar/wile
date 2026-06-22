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

// Callback-specialization Strategy A — P0 RED baseline probe.
// THROWAWAY (delete after P5 per plans/2026-06-22-callback-specialization-A-impl.local.md).
//
// Confirms the stdlib `for-each` currently LEAKS ~2 env-frame allocs/element. Its
// single-list case-lambda clause IS a tail named-let loop:
//
//	(let loop ((lst lst))
//	  (if (null? lst) (if #f #f) (begin (f (car lst)) (loop (cdr lst)))))
//
// — but bodyCalleesAllCaptureSafe (self_tail.go:57) is POISONED by the `(f ...)`
// param call (a param is an arbitrary runtime value that could call/cc), so the loop
// does NOT reclaim. `cb` is a capture-safe global, so Strategy A — inlining for-each
// at this call site with `cb` substituted — must drop this to ~0 allocs/element (the
// Q3 drive-r result, ~29 allocs total).

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/stdlib"
)

const forEachLeakSetup = `(begin
(define (build n acc) (if (= n 0) acc (build (- n 1) (cons n acc))))
(define lst (build 20000 '()))
(define (cb x) x)
)`

// BenchmarkForEachLeakBaseline measures the unspecialized stdlib for-each. Expected
// NOW: ~40000 allocs/op (2 env-frame allocs * 20000 elements). Target after P3: ~29.
func BenchmarkForEachLeakBaseline(b *testing.B) {
	ctx := context.Background()
	opts := []EngineOption{WithProfile(KitchenSink), WithSourceFS(stdlib.FS), WithLibraryPaths()}
	eng, err := NewEngine(ctx, opts...)
	if err != nil {
		b.Fatal(err)
	}
	_, err = eng.EvalMultiple(ctx, forEachLeakSetup)
	if err != nil {
		b.Fatal(err)
	}
	compiled, err := eng.Compile(ctx, eng.MustParse(ctx, "(for-each cb lst)"))
	if err != nil {
		b.Fatal(err)
	}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, runErr := eng.Run(ctx, compiled)
		if runErr != nil {
			b.Fatal(runErr)
		}
	}
}
