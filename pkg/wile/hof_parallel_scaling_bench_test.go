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
	"fmt"
	"testing"

	"github.com/aalpar/wile/pkg/stdlib"
)

// Phase 5 of callback specialization (Strategy A): the real-stdlib parallel
// confirmation of the hand-written Q3 measurement
// (plans/2026-06-22-callback-specialization-A-impl.local.md;
// 2026-06-22-callback-specialization-design.local.md § Q3 results).
//
// Q3 measured, on a hand-written drive-r loop, that inline-reclaim lifts P-core
// effective parallelism from the Leaky ~2.7x plateau to ~3.1-3.3x and that the
// per-run advantage grows with thread count. These benchmarks reproduce that on
// the REAL stdlib fold now that it inline-reclaims (P3 + P6).
//
// Design: WEAK scaling (mirrors extensions/threads/scaling_bench_test.go). Every
// thread runs the identical fold-heavy kernel, so total work scales with the
// thread count; ns/op is wall-clock for the whole parallel section running P
// copies. eff-speedup = P / (ns/op(P) / ns/op(1)); ideal is P.
//
// The two arms differ ONLY in whether fold inline-reclaims its per-iteration env
// frame — the kons callback and all per-element work are identical:
//
//   - Reclaim: (fold kons 0 data) — kons is a capture-safe Stable global symbol,
//     so the call inlines fold's single-list loop and the loop self-tail-reclaims
//     (callback specialization). Zero env-frame allocs/element.
//   - Leaky: (fold (car (list kons)) 0 data) — the callback is computed, so
//     CallbackIsCaptureSafe fails and the real, capturable fold runs, calling its
//     kons PARAMETER and leaking ~2 env frames/element (the Q3 Leaky shape). The
//     one-time (list kons) per fold call is constant and negligible vs the loop.
//
// Read: Leaky leaks env frames -> they cannot return to the per-thread pool ->
// the pool refills from Go's shared allocator -> contention caps effective
// parallelism (the [[vm-no-cpu-parallelism]] ~2.5-2.7x plateau). Reclaim keeps
// the pool full (far less Go allocation at every P), lifting the P-core ceiling.
//
// Sweep: P-cores only (this machine: 12 P + 4 E). P=16 would span E-cores and is
// not a clean weak-scaling point (the batch is dragged by the 4 slow cores), so
// it is excluded; add it back only for an explicit E-core experiment.
var hofScalingThreadCounts = []int{1, 2, 4, 8, 12}

// hofKernelSetup builds the fold-heavy weak-scaling driver. data is a 20000-
// element list; kons is a pure, bounded, capture-safe reducer (acc stays in
// [0,250] so it never boxes, isolating the env-frame allocation). foldCall is the
// arm-specific fold invocation. The kernel runs passCount fold passes per thread.
func hofKernelSetup(foldCall string, passCount int) string {
	return fmt.Sprintf(`(begin
(import (srfi 1))
(define data (iota 20000))
(define (kons x acc) (modulo (+ acc (modulo (* x x) 1009)) 251))
(define (kernel)
  (let loop ((r 0))
    (if (= r %d) #t (begin %s (loop (+ r 1))))))
(define (spawn-run p)
  (let loop ((i 0) (ts '()))
    (if (= i p)
        (for-each thread-join! ts)
        (loop (+ i 1) (cons (thread-start! (make-thread kernel)) ts))))))`,
		passCount, foldCall)
}

// benchmarkHOFScaling sweeps thread counts for one fold arm. Engine construction,
// setup eval, and spawn-run compilation happen outside the timed loop; only the
// parallel section ((spawn-run p)) is measured. A fresh engine per sub-benchmark
// isolates thread-pool state. The immutable top-level default is required for the
// Reclaim arm: it makes the global kons Stable so the callback proof admits it.
func benchmarkHOFScaling(b *testing.B, foldCall string) {
	const passCount = 8
	for _, p := range hofScalingThreadCounts {
		b.Run(fmt.Sprintf("threads=%d", p), func(b *testing.B) {
			ctx := context.Background()
			engine, err := NewEngine(ctx,
				WithProfile(KitchenSink), WithSourceFS(stdlib.FS), WithLibraryPaths())
			if err != nil {
				b.Fatalf("NewEngine: %v", err)
			}
			_, err = engine.EvalMultiple(ctx, hofKernelSetup(foldCall, passCount))
			if err != nil {
				b.Fatalf("setup eval: %v", err)
			}
			expr, err := engine.Parse(ctx, fmt.Sprintf("(spawn-run %d)", p))
			if err != nil {
				b.Fatalf("parse spawn-run: %v", err)
			}
			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				_, err := engine.Eval(ctx, expr)
				if err != nil {
					b.Fatalf("eval spawn-run: %v", err)
				}
			}
		})
	}
}

// BenchmarkHOFParallelReclaim is the post-A arm: a capture-safe symbol kons, so
// fold inlines and the loop reclaims. Effective parallelism should lift above the
// Leaky plateau and its per-run advantage should grow with thread count.
func BenchmarkHOFParallelReclaim(b *testing.B) {
	benchmarkHOFScaling(b, "(fold kons 0 data)")
}

// BenchmarkHOFParallelLeaky is the baseline arm: a computed callback, so the real
// capturable fold runs and leaks ~2 env frames/element. Its effective parallelism
// should plateau at the [[vm-no-cpu-parallelism]] ceiling.
func BenchmarkHOFParallelLeaky(b *testing.B) {
	benchmarkHOFScaling(b, "(fold (car (list kons)) 0 data)")
}
