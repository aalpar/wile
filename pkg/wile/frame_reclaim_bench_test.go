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
// primitives Stable), so each shape is benchmarked flag-on (optimized) vs
// flag-off (the default, inert path — also the regression baseline for the new
// opcodes). -benchmem reports the allocation win; ns/op reports the net effect of
// (fewer frame allocations / less GC) minus (the fib tail-+ fusion loss under A).
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

func benchReclaim(b *testing.B, immutable bool, setup, code string) {
	b.Helper()
	ctx := context.Background()
	opts := []EngineOption{WithProfile(KitchenSink), WithSourceFS(stdlib.FS), WithLibraryPaths()}
	if immutable {
		opts = append(opts, WithImmutableTopLevel())
	}
	eng, err := NewEngine(ctx, opts...)
	if err != nil {
		b.Fatal(err)
	}
	if setup != "" {
		_, err = eng.EvalMultiple(ctx, setup)
		if err != nil {
			b.Fatal(err)
		}
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

func BenchmarkFib23_FlagOff(b *testing.B) { benchReclaim(b, false, benchFib, "(fib 23)") }
func BenchmarkFib23_FlagOn(b *testing.B)  { benchReclaim(b, true, benchFib, "(fib 23)") }

func BenchmarkTak_FlagOff(b *testing.B) { benchReclaim(b, false, benchTak, "(tak 18 12 6)") }
func BenchmarkTak_FlagOn(b *testing.B)  { benchReclaim(b, true, benchTak, "(tak 18 12 6)") }

func BenchmarkTailLoop_FlagOff(b *testing.B) { benchReclaim(b, false, benchLoop, "(loop 0 30000)") }
func BenchmarkTailLoop_FlagOn(b *testing.B)  { benchReclaim(b, true, benchLoop, "(loop 0 30000)") }
