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

// A hot loop whose driver is self-tail (frame reused in place) and which calls a
// tiny NON-tail helper each iteration. The helper is the dominant env-frame pool
// consumer: every iteration does one acquireEnvFrame on the call and one
// releaseEnvFrame on the helper's normal return (RestoreAndRelease). CPU-profile
// this to read the cumulative fraction of runtime spent in the pool round-trip
// (FreeList.Acquire/Release/ResetForPool/InitApplyFrame) — the ceiling on what a
// "local call frame elide" could recover over today's pooled path.
//
//	(id x)          — identity helper: minimal body, so call overhead (incl. the
//	                  pool round-trip) dominates → an UPPER bound on the fraction.
//	run loops N times, calling id non-tail each iteration.
//
// The loop count stays inside the cached-integer window [-32768, 32767] so
// (- n 1) never boxes a fresh Integer — otherwise GC churn from integer boxing
// (a SEPARATE allocation source) swamps the env-frame pool signal we are measuring.
const benchNonTailHelper = "(begin " +
	"(define (id x) x)\n" +
	"(define (run n) (if (= n 0) 0 (begin (id n) (run (- n 1)))))\n" +
	")"

func benchNonTailHelperLoop(b *testing.B, code string) {
	b.Helper()
	ctx := context.Background()
	// Immutable top level is the production default; measure the optimized path.
	opts := []EngineOption{WithProfile(KitchenSink), WithSourceFS(stdlib.FS), WithLibraryPaths()}
	eng, err := NewEngine(ctx, opts...)
	if err != nil {
		b.Fatal(err)
	}
	_, err = eng.EvalMultiple(ctx, benchNonTailHelper)
	if err != nil {
		b.Fatal(err)
	}
	compiled, err := eng.Compile(ctx, eng.MustParse(ctx, code))
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

// BenchmarkNonTailHelperLoop drives 30000 non-tail helper calls per op, all
// within the cached-integer window so the only churn is the env-frame pool.
func BenchmarkNonTailHelperLoop(b *testing.B) {
	benchNonTailHelperLoop(b, "(run 30000)")
}
