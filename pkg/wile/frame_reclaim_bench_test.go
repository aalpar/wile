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

// Phase 6 measurement for escape-gated frame reclamation. The optimization is
// gated by WithImmutableTopLevel (it stamps top-level defines + ambient base
// primitives Stable) — which is now the engine DEFAULT (options.go). The flag-off
// arm must therefore EXPLICITLY opt out via WithMutableTopLevel(); omitting it
// leaves both arms optimized and the benchmark measures nothing (the regression
// TestFrameReclaimBenchArmsDiffer guards exactly that). Each shape is benchmarked
// flag-on (optimized) vs flag-off (mutable top level, the unoptimized baseline).
// -benchmem reports the allocation win; ns/op reports the net effect of (fewer
// frame allocations / less GC) minus (the fib tail-+ fusion loss under A).
//
// All recursion/loop magnitudes stay inside the cached-integer window
// [-32768, 32767] so integer boxing is identical across arms and does not pollute
// the per-op allocation delta.
//
//	fib(23)=28657      — OpReleaseEnvFrame (tail call to the foreign +)
//	tak(18,12,6)       — OpSelfTailCall (tail self-call) + non-tail recursion
//	tail loop 30000    — OpSelfTailCall (pure self-tail loop)

const benchFib = "(begin (define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))\n)"
const benchTak = "(begin (define (tak x y z) (if (not (< y x)) z (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))))\n)"
const benchLoop = "(begin (define (loop i n) (if (>= i n) i (loop (+ i 1) n)))\n)"

// newReclaimEngine builds the engine for one benchmark/test arm. The mutable arm
// EXPLICITLY opts out via WithMutableTopLevel — required because immutable top
// level is the engine default (options.go); omitting the opt-out would leave the
// "off" arm optimized and collapse the A/B delta to zero. Shared by benchReclaim
// and reclaimRunAllocs so the opt-out lives in exactly one place.
func newReclaimEngine(tb testing.TB, immutable bool, setup string) (*Engine, context.Context) {
	tb.Helper()
	ctx := context.Background()
	opts := []EngineOption{WithProfile(KitchenSink), WithSourceFS(stdlib.FS), WithLibraryPaths()}
	if immutable {
		opts = append(opts, WithImmutableTopLevel())
	} else {
		// Immutable top level is the DEFAULT (options.go), so the flag-off arm MUST
		// opt out explicitly — otherwise both arms run the optimized path and the A/B
		// delta collapses to a false ~0, making every lever's measure step inert.
		opts = append(opts, WithMutableTopLevel())
	}
	eng, err := NewEngine(ctx, opts...)
	if err != nil {
		tb.Fatal(err)
	}
	if setup != "" {
		_, err = eng.EvalMultiple(ctx, setup)
		if err != nil {
			tb.Fatal(err)
		}
	}
	return eng, ctx
}

func benchReclaim(b *testing.B, immutable bool, setup, code string) {
	b.Helper()
	eng, ctx := newReclaimEngine(b, immutable, setup)
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

func BenchmarkFib23_FlagOff(b *testing.B) { benchReclaim(b, false, benchFib, "(fib 23)") }
func BenchmarkFib23_FlagOn(b *testing.B)  { benchReclaim(b, true, benchFib, "(fib 23)") }

func BenchmarkTak_FlagOff(b *testing.B) { benchReclaim(b, false, benchTak, "(tak 18 12 6)") }
func BenchmarkTak_FlagOn(b *testing.B)  { benchReclaim(b, true, benchTak, "(tak 18 12 6)") }

func BenchmarkTailLoop_FlagOff(b *testing.B) { benchReclaim(b, false, benchLoop, "(loop 0 30000)") }
func BenchmarkTailLoop_FlagOn(b *testing.B)  { benchReclaim(b, true, benchLoop, "(loop 0 30000)") }

// TestFrameReclaimBenchArmsDiffer pins the benchmark's control arm so its prior bug
// cannot silently return. After immutable top level became the engine default,
// benchReclaim's "off" arm — which only conditionally ADDED WithImmutableTopLevel
// and never opted out — ran optimized too, so FlagOff ≡ FlagOn and the A/B
// benchmark measured nothing. The mutable arm MUST allocate far more than the
// optimized arm; 10× is a deliberately loose floor (the real fib ratio is ~1000×),
// so this asserts "the control arm is unoptimized" without being magnitude-brittle.
func TestFrameReclaimBenchArmsDiffer(t *testing.T) {
	off := reclaimRunAllocs(t, false, benchFib, "(fib 20)")
	on := reclaimRunAllocs(t, true, benchFib, "(fib 20)")
	if off < on*10 {
		t.Errorf("benchmark control arm is inert: FlagOff=%.0f allocs/run, FlagOn=%.0f allocs/run; "+
			"want FlagOff >= 10*FlagOn (the mutable arm must run unoptimized)", off, on)
	}
}

// reclaimRunAllocs returns the average heap allocations of one Engine.Run of code
// under the given optimization arm — the test-time analogue of benchReclaim's loop.
func reclaimRunAllocs(t *testing.T, immutable bool, setup, code string) float64 {
	t.Helper()
	eng, ctx := newReclaimEngine(t, immutable, setup)
	compiled, err := eng.Compile(ctx, eng.MustParse(ctx, code))
	if err != nil {
		t.Fatal(err)
	}
	return testing.AllocsPerRun(3, func() {
		_, runErr := eng.Run(ctx, compiled)
		if runErr != nil {
			t.Fatal(runErr)
		}
	})
}
