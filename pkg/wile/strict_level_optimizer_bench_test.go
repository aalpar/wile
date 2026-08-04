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

// Level 2 removes the optimizer's usual evidence for primitive stability:
// WithStableBasePrimitives stamps AMBIENT primitives Stable, and at level 2 there
// are none. The design's claim is that nothing is lost, because IsStable already
// treats Imported as standing evidence ("imported primitives are already
// immutable") and stampImportedInlineHOF covers the inline-HOF path — so level 2
// shifts the evidence SOURCE from ambient registration to import rather than
// losing it. That claim was unverified when written; these arms verify it.
//
// Both arms run identical code over identically-spelled bindings. They differ only
// in where +, -, <= and map came from:
//
//	Level0  ambient, stamped Stable at registration
//	Level2  imported by (import (scheme base)), stamped Stable at import
//
// fib is the arithmetic/self-call arm; mapSum adds the inline-HOF path, which
// travels by a different stamp (stampImportedInlineHOF) and would degrade
// independently of the arithmetic one.
//
// Steady-state execution only: the preamble import runs during setup, so the
// ~9.4 ms per-import tax is deliberately NOT in the measured region. That tax is
// real and documented on WithoutAmbientBindings; it is a startup cost, and mixing
// it in here would swamp the per-op signal these arms exist to read.
//
//	go test ./pkg/wile/ -run '^$' -bench 'StrictLevelOpt' -benchmem -count=8
//
// Measured 2026-08-04, macOS/arm64 (M4 Max), count=8, benchstat: no significant
// time difference on either arm (fib 2.718m → 2.712m, p=0.798; map-sum 4.900µ →
// 4.944µ, p=0.225; geomean +0.35%), and B/op and allocs/op are IDENTICAL —
// 10.44Ki/81 and 6.520Ki/71, all samples equal. The allocation identity is the
// load-bearing half: losing a Stable stamp would move the shape (PullApply in
// place of a promoted op, a non-inlined HOF), which shows up in allocs/op and
// does not hide inside timing noise. The ns/op deltas alone would not settle it —
// per memory/interleaved-ab-required-for-vm-microdeltas.md a same-binary
// sequential comparison cannot resolve sub-1.5% VM deltas.

const benchOptFib = `(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))`

const benchOptMapSum = `(define (map-sum xs) (apply + (map (lambda (x) (* x 2)) xs)))
(define xs (list 1 2 3 4 5 6 7 8 9 10))`

// newStrictLevelEngine builds one arm. The level-2 arm must import (scheme base)
// or nothing in the body resolves at all; the level-0 arm imports it too, so the
// arms differ ONLY in whether the ambient copy was there first. Skipping the
// import on the level-0 arm would leave the two running against different binding
// objects for reasons unrelated to the stamp under test.
func newStrictLevelEngine(b *testing.B, noAmbient bool, setup string) (*Engine, context.Context) {
	b.Helper()
	ctx := context.Background()
	opts := []EngineOption{WithProfile(Small), WithSourceFS(stdlib.FS), WithLibraryPaths()}
	if noAmbient {
		opts = append(opts, WithoutAmbientBindings())
	}
	eng, err := NewEngine(ctx, opts...)
	if err != nil {
		b.Fatal(err)
	}
	_, err = eng.EvalMultiple(ctx, "(import (scheme base))\n"+setup)
	if err != nil {
		b.Fatal(err)
	}
	return eng, ctx
}

func benchStrictLevel(b *testing.B, noAmbient bool, setup, code string) {
	b.Helper()
	eng, ctx := newStrictLevelEngine(b, noAmbient, setup)
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

func BenchmarkStrictLevelOptFib_Level0(b *testing.B) {
	benchStrictLevel(b, false, benchOptFib, "(fib 20)")
}

func BenchmarkStrictLevelOptFib_Level2(b *testing.B) {
	benchStrictLevel(b, true, benchOptFib, "(fib 20)")
}

func BenchmarkStrictLevelOptMapSum_Level0(b *testing.B) {
	benchStrictLevel(b, false, benchOptMapSum, "(map-sum xs)")
}

func BenchmarkStrictLevelOptMapSum_Level2(b *testing.B) {
	benchStrictLevel(b, true, benchOptMapSum, "(map-sum xs)")
}
