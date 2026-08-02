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
// into a real speedup. What still serializes the cores is the open question these
// benchmarks pin down reproducibly, instead of via a one-off CLI run.
//
// Last measured 2026-08-01, Apple M4 Max (12 P + 4 E), GOMAXPROCS=16,
// -benchtime=1s. Effective speedup is P / (ns/op(P) / ns/op(1)); ideal is P.
//
//	P     Compute   Control
//	1       1.00x     1.00x
//	2       1.93x     1.58x
//	4       3.62x     2.24x
//	8       6.98x     2.30x
//	16      8.82x     2.43x
//
// The two-kernel discrimination has fired. Compute holds near-linear scaling
// through 8 and still gains at 16; Control is flat from 8 on. The older shared
// reading ("~2.5x at 8, 16 no better than 8", plans/2026-06-10-pool-parallel-
// scaling.local.md) now describes Control alone. Not attributed: the 2026-06-10
// run was different hardware AND predates unsyncFreeList (PR #777), which took
// the residual atomics out of the per-thread pools.
//
// Attribution, profiled 2026-08-01 via the recipe below: Control's plateau is the
// Go runtime's heap SPAN ALLOCATOR. Not subContextPool, and not the collector.
//
//	mutex   100% runtime._LostContendedRuntimeLock + runtime.unlock, i.e. a
//	        runtime-internal lock. No Wile mutex and no subContextPool appear at
//	        all; the Wile frames present are callers into the allocator.
//	cpu     mheap.allocSpan 23% and sweep->mheap.freeSpan 20%, both under the
//	        mheap lock; sysUsed->madvise 22%, darwin page RE-commit; lock2 39%,
//	        two thirds of that spinning in osyield. MachineContext.Run is 19%.
//	        Mark plus stop-the-world together are under 5%.
//
// Confirmed by a knob that changes the environment but not the work. GOGC, as
// effective speedup at threads=16: 100 -> 2.49x, 400 -> 3.98x, 1600 -> 5.20x,
// while threads=1 moves ~6% and then stops. A setting that does nothing for one
// thread and doubles sixteen is shared contention by definition.
//
// Mechanism: Control churns ~2.7 GB/s. The heap grows, the scavenger returns
// pages, the allocator needs them straight back, and each recommit on darwin is
// a madvise syscall; 16 threads then serialize on the mheap lock. GOGC is the
// embedder's knob. The lever Wile owns is allocation volume. Per-wind budget as
// it stood on 2026-08-01, 12.7 objects and 654 bytes (memprofilerate=1, exact):
//
//	240 B  3 x 80 B frames from OpMakeClosure  | cut, MachineClosure pair split
//	240 B  env-frame pool miss (80 B + 160 B slab)
//	 64 B  DynamicWindFrame                    | cut, value-slice WindingStack
//	 56 B  box/unbox of the thunk result       | cut, single-value fast path
//	 48 B  3 closure objects (now 72 B: 3 x 24)
//
// Three cuts taken the same day (-benchtime 2s -count 6, benchstat, each against
// a baseline measured adjacent to it), leaving 5.7 objects / 318 bytes:
//
//	                     allocs  bytes    P=1               P=16
//	box/unbox fast path   -23.6%  -8.6%  -5.67% p=0.002   -5.68% p=0.041
//	winding value slice    -7.9%  -9.8%   ~     p=0.699   -3.42% p=0.002
//	+ closure pair split  -55.2% -51.4% -22.85% p=0.002  -31.67% p=0.002
//
// Read the two columns, not the totals. Removing LOCAL work (mallocgc calls)
// helps both arms alike: box/unbox moved P=1 and P=16 by the same 5.7% and left
// effective speedup at 2.63x. Removing BYTES from the shared span allocator
// while adding local work back (a 64-byte pointer-bearing copy under a write
// barrier: gcWriteBarrier and wbBufFlush1 go 2.5% -> 8.4% of CPU, mallocgc 5.0%
// -> 3.4%) cancels at P=1 and survives only where that allocator is contended.
// The closure cut does both at once, which is why it is the first one to move
// the plateau: 2.645x -> 2.988x. What remains is the 240 B pool miss, which is
// inherent (OpMakeClosure sets envPooled = false because the closure captures
// mc.env as its parent, so that frame can never be recycled).
//
// Replicate before believing. This benchmark spreads ~2% at P=1 and ~8% at P=16
// run to run, so a single-sample delta under 10% is noise: the winding cut first
// read as +14% at P=1 and is in fact p=0.699. Use -count 6 and benchstat, and
// re-baseline adjacent to the arm under test rather than reusing an older run.
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
// Two kernels, written to isolate the two then-live hypotheses for the plateau:
//
//   - Compute (fib): non-tail recursion. Hits the per-thread env-frame and
//     continuation pools heavily but NEVER subContextPool. If this kernel
//     plateaus, the cause is shared (GC / allocation pressure), not a pool mutex,
//     because its hot pools are already per-thread.
//   - Control (dynamic-wind): believed when written to route through
//     NewSubContext on every iteration, drawing the MachineContext struct from
//     the process-global subContextPool (machine/pool.go). It does not, and never
//     did. dynamic-wind in operator position compiles to inline bytecode
//     (CompileValidatedDynamicWind: PushWind / PeekK / SaveContinuation / Apply /
//     PopWind) and closure application stays in context, so the count is zero
//     sub-contexts per iteration and one per thread. What the kernel does do is
//     allocate ~3000x more objects per op than Compute (635k vs 209).
//
// The inference originally drawn from that pair ("Control plateaus harder, so
// the gap is subContextPool contention") is REFUTED twice over: by the profile
// above, and by the correction in the bullet. The pool was not a confounded
// variable, it was an absent one. Allocation volume is what the
// pair actually isolates, and that is still worth having. Keep the kernels,
// distrust that inference.
//
// Attribution recipe (run one sub-benchmark, snapshot the profile). Control is
// the arm that still plateaus, so it is the one worth profiling:
//
//	go test -run x -bench 'ParallelScalingControl/threads=16' \
//	    -cpuprofile cpu.out -mutexprofile mutex.out -blockprofile block.out \
//	    ./extensions/threads/
//	go tool pprof -top mutex.out   # any Wile mutex, or only _LostContendedRuntimeLock?
//	go tool pprof -top -cum cpu.out # mheap.allocSpan / freeSpan / sysUsed share
//
// Look at the span layer, not at gcBgMarkWorker: as of 2026-08-01 marking is a
// rounding error and the cost is allocSpan/freeSpan/madvise. Rerun after any
// change to allocation volume, pooling, or dynamic-wind's frame layout.

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

// controlKernelSetup defines an allocation-heavy kernel: each iteration enters a
// dynamic-wind, costing ~12.7 objects (three lambdas at two objects each, a
// winding frame, box/unbox, an env-frame pool miss). Same driver as compute.
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

// BenchmarkParallelScalingControl measures weak scaling of an allocation-heavy
// kernel (dynamic-wind). A plateau worse than Compute's isolates allocation
// volume as the extra cost; on the sub-context claim it replaced, see the header.
func BenchmarkParallelScalingControl(b *testing.B) {
	benchmarkScaling(b, controlKernelSetup)
}
