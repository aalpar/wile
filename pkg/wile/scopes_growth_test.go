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

package wile_test

import (
	"context"
	"fmt"
	"runtime"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// The growth ratchet for the scope-set representation.
//
// # What it measures, and why a ratio rather than a number
//
// Compiling n nested lexical forms used to allocate O(n^3) words: the expander
// performs O(n^2) scope-set adds over sets averaging O(n) members, and every add
// COPIED the set. A ratio between two problem sizes is the only stable way to
// assert that: absolute allocation depends on the machine and on everything else
// the process has done, while alloc(2n)/alloc(n) is a property of the algorithm.
// Quadratic growth is a doubling ratio of 4; cubic is 8.
//
// # Measured, not projected
//
//	                   at d2927bea   after the flip   Racket 9.3
//	probe A (let)          7.00           3.98           ~2.8
//	probe B (shrink)       6.81           3.96           4.69
//
// Allocation at n=750 fell from 1345.4 to 190.6 MB on probe A (-85.8%) and from
// 2092.7 to 358.7 MB on probe B (-82.9%). Both ratios reproduce to +/-0.001
// across runs; this is an allocation measure, not a timer, so it is deterministic.
//
// # Why the threshold is 4.5
//
// It is oracle-derived. Racket 9.3 on IDENTICAL probe text, measured with
// (current-memory-use 'cumulative) around (compile stx) in a fresh
// make-base-namespace, gives doubling ratios of 1.98/2.75/2.78/3.01 on nested let
// and 4.22/4.30/4.69/4.45 on shrink. So 4.5 is enormous slack on probe A and,
// at the 375->750 doubling this test measures, a bar Racket itself FAILS on
// probe B (4.69). Wile now reads 3.96 there — better than the oracle rather than
// merely under an arbitrary number, which is the claim worth having.
//
// # The exponent is still climbing, and that is expected
//
// Sweeping n=94/188/375/750/1500 at d2927bea gave local exponents 2.44, 2.66,
// 2.81, 2.88 — converging on 3.0, so the pre-flip growth was CUBIC and 7.00 was a
// property of the 375->750 doubling specifically, not a constant. Removing the
// per-add copy collapses exactly one factor and leaves O(n^2) behind, which is
// what 3.98 is. A future change that attacks the remaining quadratic term should
// expect this threshold to come down with it.
//
// # If this test fails
//
// It is measuring allocation during Compile only, through the public API. A
// regression here means some path re-introduced per-touch copying of a scope set.
// Look first at values.SourceContext.WithScope (source_context.go), which carried
// 85% of probe A's allocation before the flip, and at anything that materializes
// a scope set into a slice — the type deliberately offers no Slice() for exactly
// this reason.
const growthRatioCeiling = 4.5

// nestedLetProbe builds probe A. The n=3 spelling is committed verbatim so the
// probe cannot drift from the numbers recorded above:
//
//	(let ((v1 0)) (let ((v2 0)) (let ((v3 0)) (list v1 v3))))
func nestedLetProbe(n int) string {
	var b strings.Builder
	for i := 1; i <= n; i++ {
		fmt.Fprintf(&b, "(let ((v%d 0)) ", i)
	}
	fmt.Fprintf(&b, "(list v1 v%d)", n)
	b.WriteString(strings.Repeat(")", n))
	return b.String()
}

// shrinkProbeSetup is probe B's macro, verbatim.
const shrinkProbeSetup = `(define-syntax shrink (syntax-rules () ((_) 0) ((_ a b ...) (shrink b ...))))`

// shrinkProbe builds probe B's body. The n=3 spelling is `(shrink s0 s1 s2)`.
func shrinkProbe(n int) string {
	var b strings.Builder
	b.WriteString("(shrink")
	for i := range n {
		fmt.Fprintf(&b, " s%d", i)
	}
	b.WriteString(")")
	return b.String()
}

// compileAllocMB returns the MB allocated by Compile alone, through the public
// engine API. Engine construction and parsing are deliberately outside the
// measured window: they are O(1) in n and would add a constant to both ends of
// the ratio, flattening it.
func compileAllocMB(t *testing.T, setup, src string) float64 {
	t.Helper()
	c := qt.New(t)
	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	c.Assert(err, qt.IsNil)
	if setup != "" {
		_, err = engine.EvalMultiple(ctx, setup)
		c.Assert(err, qt.IsNil)
	}
	expr, err := engine.Parse(ctx, src)
	c.Assert(err, qt.IsNil)

	runtime.GC()
	var before, after runtime.MemStats
	runtime.ReadMemStats(&before)
	_, err = engine.Compile(ctx, expr)
	runtime.ReadMemStats(&after)
	c.Assert(err, qt.IsNil)

	return float64(after.TotalAlloc-before.TotalAlloc) / (1024 * 1024)
}

// assertGrowthRatio measures the 375->750 doubling and asserts the ceiling.
// Those two sizes are not arbitrary: every recorded figure for these probes was
// taken at that doubling, and because the growth is not a clean power the ratio
// is size-dependent (7.00 at 375->750 became 7.38 at 750->1500 before the flip).
// Changing n here invalidates the comparison to both the pre-flip numbers and the
// Racket column.
func assertGrowthRatio(t *testing.T, name, setup string, probe func(int) string) {
	t.Helper()
	c := qt.New(t)

	const small, large = 375, 750
	lo := compileAllocMB(t, setup, probe(small))
	hi := compileAllocMB(t, setup, probe(large))

	c.Assert(lo > 0, qt.IsTrue, qt.Commentf("%s: n=%d allocated nothing measurable", name, small))
	ratio := hi / lo
	t.Logf("%s: alloc(%d)=%.2f MB alloc(%d)=%.2f MB ratio=%.3f (ceiling %.1f)",
		name, small, lo, large, hi, ratio, growthRatioCeiling)

	c.Assert(ratio <= growthRatioCeiling, qt.IsTrue,
		qt.Commentf("%s: doubling ratio %.3f exceeds %.1f — scope-set growth regressed toward cubic; see this file's header",
			name, ratio, growthRatioCeiling))
}

// TestNestedLetCompileGrowthIsQuadratic is the ratchet on ordinary deeply-nested
// Scheme, with no macro anywhere in the program. It read 7.00 at d2927bea.
//
// The name states the TARGET, not the defect: the growth it was installed
// against was cubic. See the header.
func TestNestedLetCompileGrowthIsQuadratic(t *testing.T) {
	assertGrowthRatio(t, "nested-let", "", nestedLetProbe)
}

// TestMacroRecursionGrowthIsQuadratic is the ratchet on a recursive syntax-rules
// macro, which is where the intro-scope flip dominates. It read 6.81 at
// d2927bea, and Racket 9.3 reads 4.69 on the same doubling.
func TestMacroRecursionGrowthIsQuadratic(t *testing.T) {
	assertGrowthRatio(t, "shrink", shrinkProbeSetup, shrinkProbe)
}
