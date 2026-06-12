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

// CI-stable parallel scaling benchmarks for attributing the residual ceiling on
// SRFI-18 thread scaling (see plans/2026-06-10-pool-parallel-scaling.local.md).
//
// Per-thread allocation pools (machine/pool.go) converted threads from a slowdown
// into a real speedup, but scaling is sublinear: 8 threads buys ~2.5x and 16 does
// not beat 8. Something shared still serializes the cores. These benchmarks pin
// down what, reproducibly, instead of via a one-off CLI run.
//
// Design: WEAK scaling. Every thread runs the identical kernel, so total work
// scales with the thread count. ns/op is therefore wall-clock for the whole
// parallel section running P copies of one fixed kernel.
//
//	ns/op flat as P grows  -> linear scaling (P cores absorb P copies)
//	ns/op rising with P    -> something shared serializes (GC, a global mutex)
//
// The slowdown factor ns/op(P) / ns/op(1) is the read; ideal is 1.0.
//
// Two kernels isolate the two live hypotheses for the plateau:
//
//   - Compute (fib): non-tail recursion. Hits the per-thread env-frame and
//     continuation pools heavily but NEVER subContextPool. If this kernel
//     plateaus, the cause is shared (GC / allocation pressure), not a pool mutex,
//     because its hot pools are already per-thread.
//   - Control (dynamic-wind): routes through NewSubContext on every iteration,
//     drawing the MachineContext struct from the process-global subContextPool
//     (machine/pool.go). If THIS kernel plateaus harder than Compute, the extra
//     gap is subContextPool contention — the one pool.go residual still global.
//
// Attribution recipe (run one sub-benchmark, snapshot the profile):
//
//	go test -run x -bench 'ParallelScalingCompute/threads=16' \
//	    -cpuprofile cpu.out -mutexprofile mutex.out -blockprofile block.out \
//	    ./extensions/threads/
//	go tool pprof -top mutex.out   # is any pool mutex hot?
//	go tool pprof -top cpu.out     # how much is runtime.gcBgMarkWorker / mallocgc?
//
// A flat mutex profile + heavy GC in the CPU profile confirms the plan's
// "GC, not pool.go" hypothesis and triggers the Phase 0 decision gate.

import (
	"context"
	"fmt"
	"testing"

	extthreads "github.com/aalpar/wile/extensions/threads"
	"github.com/aalpar/wile/pkg/wile"
)

// scalingThreadCounts is the sweep. Includes 1 as the single-thread baseline that
// every higher count is measured against.
var scalingThreadCounts = []int{1, 2, 4, 8, 16}

// computeKernelSetup defines a pure-compute kernel (fib) and a weak-scaling driver.
// fib is non-tail, so it exercises the per-thread env-frame and continuation pools
// on every call but never creates a sub-context.
const computeKernelSetup = `
(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(define (kernel) (fib 24))
(define (spawn-run p)
  (let loop ((i 0) (ts '()))
    (if (= i p)
        (for-each thread-join! ts)
        (loop (+ i 1) (cons (thread-start! (make-thread kernel)) ts)))))
`

// controlKernelSetup defines a control-heavy kernel: each iteration enters a
// dynamic-wind, which routes through NewSubContext and so draws from the
// process-global subContextPool. Same driver as compute.
const controlKernelSetup = `
(define (dw-loop n acc)
  (if (= n 0) acc
      (dw-loop (- n 1)
        (dynamic-wind (lambda () #f) (lambda () (+ acc 1)) (lambda () #f)))))
(define (kernel) (dw-loop 50000 0))
(define (spawn-run p)
  (let loop ((i 0) (ts '()))
    (if (= i p)
        (for-each thread-join! ts)
        (loop (+ i 1) (cons (thread-start! (make-thread kernel)) ts)))))
`

// benchmarkScaling sweeps thread counts for one kernel. Engine construction and
// kernel compilation happen outside the timed loop; only the parallel section is
// measured. A fresh engine per sub-benchmark keeps thread-pool state isolated.
func benchmarkScaling(b *testing.B, setup string) {
	for _, p := range scalingThreadCounts {
		b.Run(fmt.Sprintf("threads=%d", p), func(b *testing.B) {
			ctx := context.Background()
			engine, err := wile.NewEngine(ctx, wile.WithExtension(extthreads.Extension))
			if err != nil {
				b.Fatalf("NewEngine: %v", err)
			}
			_, err = engine.EvalMultiple(ctx, setup)
			if err != nil {
				b.Fatalf("setup eval: %v", err)
			}
			expr, err := engine.Parse(ctx, fmt.Sprintf("(spawn-run %d)", p))
			if err != nil {
				b.Fatalf("parse spawn-run: %v", err)
			}
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

// BenchmarkParallelScalingCompute measures weak scaling of a pure-compute kernel
// (fib). Plateau here fingerprints GC / allocation pressure, not a pool mutex.
func BenchmarkParallelScalingCompute(b *testing.B) {
	benchmarkScaling(b, computeKernelSetup)
}

// BenchmarkParallelScalingControl measures weak scaling of a sub-context-heavy
// kernel (dynamic-wind). A plateau worse than Compute's isolates subContextPool
// contention as the extra cost.
func BenchmarkParallelScalingControl(b *testing.B) {
	benchmarkScaling(b, controlKernelSetup)
}
