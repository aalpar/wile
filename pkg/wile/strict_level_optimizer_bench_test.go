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
// losing it. That claim was unverified when written; these arms verify its
// Imported⇒IsStable half. They do NOT verify the stampImportedInlineHOF half —
// see the mapSum paragraph, which found that half is not reached from here and
// does not hold for the sealed-base HOFs.
//
// Both arms run identical code over identically-spelled bindings. They differ only
// in where +, -, <= and map came from:
//
//	Level0  ambient, stamped Stable at registration
//	Level2  imported by (import (scheme base)), stamped Stable at import
//
// fib is the arithmetic/self-call arm: its <=, - and + become promoted ops
// (NumLe, Sub, AddTail) only while their bindings are stamped Stable, so a lost
// stamp moves it back onto the generic apply path.
//
// mapSum was written to add the inline-HOF path on the theory that it travels by
// a different stamp (stampImportedInlineHOF). Measurement says it does not reach
// that path at all: stampImportedInlineHOF stamps only the IMPORT-GATED HOFs
// (fold, fold-right), while map is a sealed-base HOF stamped by StampInlineHOFs,
// and the preamble import rebinds map to a fresh imported binding that neither
// seam stamps. So map's loop is left un-inlined in BOTH arms — verified by
// disassembly, which shows PushCachedBinding + PullApply rather than the inlined
// null?/car/cdr loop. What this arm actually measures is that the generic apply
// path and the resolution of + and map are unchanged; the inline-HOF stamp is
// NOT under test here, and reaching it would need fold or fold-right from
// (srfi 1). Kept because it is a real second shape, not because it probes what
// its name suggests.
//
// Steady-state execution only: the preamble import runs during setup, so the
// ~9.4 ms per-import tax is deliberately NOT in the measured region. That tax is
// real and documented on WithoutAmbientBindings; it is a startup cost, and mixing
// it in here would swamp the per-op signal these arms exist to read.
//
// Collect the two arms INTERLEAVED, not as two blocks of eight. Go runs a -count=8
// benchmark as eight consecutive repeats of each function, so every Level0 sample
// predates every Level2 sample and any drift in machine load lands entirely on the
// level-2 arm. Measured both ways below: the blocked form reported a spurious
// +2.67%/+11.64%, the interleaved form on the same binary and the same machine
// reported no difference. Build the comparison by alternating single runs:
//
//	go test -c -o /tmp/wile.test ./pkg/wile/
//	for i in $(seq 8); do /tmp/wile.test -test.run '^$' \
//	    -test.bench StrictLevelOpt -test.benchmem -test.count=1; done
//
// Measured 2026-08-04, macOS/arm64 (M4 Max), n=8 interleaved, benchstat:
//
//   - sec/op: no significant difference on either arm (fib 2.769m → 2.762m,
//     p=0.505; map-sum 5.220µ → 5.387µ, p=0.574; geomean +1.46%). Inconclusive by
//     construction, not merely by p-value: per
//     memory/interleaved-ab-required-for-vm-microdeltas.md a same-binary
//     comparison cannot resolve sub-1.5% VM deltas, and this geomean is inside
//     that band.
//   - allocs/op: EXACTLY equal in every sample of both arms, 81 on fib and 71 on
//     map-sum (benchstat footnotes both rows "all samples are equal").
//   - B/op: equal on map-sum in every sample (6.520Ki). On fib it is equal only
//     after benchstat's rounding to 10.44Ki; the raw samples jitter WITHIN a
//     single arm, 10693–10711 B/op, and which arm jitters varies between runs.
//     So fib's B/op agreement is not sample-level identity and carries no weight.
//
// allocs/op is therefore the load-bearing metric, and it is the right one: losing
// a Stable stamp changes the emitted shape (PullApply in place of a promoted op),
// which shows up as a whole-number change in allocation COUNT and does not hide
// inside timing noise, whereas B/op is perturbed by allocation SIZE effects that
// vary run to run at 81 allocs held constant.
//
// TestStrictLevelOptSameShape (strict_level_optimizer_test.go) asserts the same
// property directly off the disassembly, which is what CI actually runs; these
// benchmarks additionally show that equal shape costs equal time.

const benchOptFib = `(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))`

const benchOptMapSum = `(define (map-sum xs) (apply + (map (lambda (x) (* x 2)) xs)))
(define xs (list 1 2 3 4 5 6 7 8 9 10))`

// newStrictLevelEngine builds one arm. The level-2 arm must import (scheme base)
// or nothing in the body resolves at all; the level-0 arm imports it too, so the
// arms differ ONLY in whether the ambient copy was there first. Skipping the
// import on the level-0 arm would leave the two running against different binding
// objects for reasons unrelated to the stamp under test.
// It takes testing.TB so the shape test in strict_level_optimizer_test.go builds
// its arms through this exact function: a test that constructed its engines
// separately would not be pinning the claim these benchmarks make.
func newStrictLevelEngine(tb testing.TB, noAmbient bool, setup string) (*Engine, context.Context) {
	tb.Helper()
	ctx := context.Background()
	opts := []EngineOption{WithProfile(Small), WithSourceFS(stdlib.FS), WithLibraryPaths()}
	if noAmbient {
		opts = append(opts, WithoutAmbientBindings())
	}
	eng, err := NewEngine(ctx, opts...)
	if err != nil {
		tb.Fatal(err)
	}
	_, err = eng.EvalMultiple(ctx, "(import (scheme base))\n"+setup)
	if err != nil {
		tb.Fatal(err)
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
