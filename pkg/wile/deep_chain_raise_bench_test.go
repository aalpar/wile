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

// Gate for the eager error-context mark snapshot (exception_raise.go): every raise
// now walks the live continuation chain up to DefaultPromptTag to snapshot marks,
// which is O(chain-depth) — unlike the depth-bounded CaptureStackTrace. This bench
// stresses exactly that worst case: a raise at the BOTTOM of a deep non-tail chain,
// with the guard handler at the TOP, so the snapshot walk spans the full depth.
//
//	(deep n) recurses non-tail — each (+ 1 (deep …)) leaves a live frame on mc.cont,
//	         so at n=0 the chain from the raise site up to the top prompt is D frames
//	         deep. That is precisely the walk CollectContinuationMarks performs.
//	(run d)  wraps the whole thing in a guard whose clause matches, so the raise is
//	         caught (no secondary escalation) and the op is a clean raise+catch.
//
// Depths stay inside the cached-integer window [-32768, 32767] so (- n 1) never
// boxes a fresh Integer — GC churn from integer boxing would swamp the walk signal.
// Sub-benchmarks sweep depth so sec/op-vs-depth reveals the per-frame walk cost; if
// it is a flat offset dwarfed by raise/guard machinery, the eager snapshot is free
// in practice.
const benchDeepChainRaise = "(begin " +
	"(define (deep n) (if (= n 0) (raise 'x) (+ 1 (deep (- n 1)))))\n" +
	"(define (run d) (guard (e (#t #t)) (deep d)))\n" +
	")"

func benchDeepChainRaiseLoop(b *testing.B, code string) {
	b.Helper()
	ctx := context.Background()
	// Immutable top level is the production default; measure the optimized path.
	opts := []EngineOption{WithProfile(KitchenSink), WithSourceFS(stdlib.FS), WithLibraryPaths()}
	eng, err := NewEngine(ctx, opts...)
	if err != nil {
		b.Fatal(err)
	}
	_, err = eng.EvalMultiple(ctx, benchDeepChainRaise)
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

// BenchmarkDeepChainRaise sweeps the chain depth at which the raise fires. Each op
// is one raise caught by a guard D frames above; the marks-snapshot walk covers all
// D frames. Compare sec/op across depths to read the per-frame walk cost.
func BenchmarkDeepChainRaise(b *testing.B) {
	for _, depth := range []int{1, 100, 1000, 5000} {
		b.Run(fmt.Sprintf("depth=%d", depth), func(b *testing.B) {
			benchDeepChainRaiseLoop(b, fmt.Sprintf("(run %d)", depth))
		})
	}
}
