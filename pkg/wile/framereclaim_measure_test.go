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

// Phase-2 measurement harness for escape-gated tail-frame reclamation
// (plans/2026-06-11-escape-gated-frame-allocation.local.md §"Phase 2"). It runs
// the Phase-1 classifier over every canonical Gabriel benchmark under both
// immutability tiers and weights each top-level function's reclaimability verdict
// by its dynamic call count (≈ frame allocations). The output prices the hard
// top-level-immutability analysis (Phase 7) BEFORE building it:
//
//	recovered_local     = Σ calls(reclaimable under tier a) / Σ calls(top-level defines)
//	recovered_toplevel  = Σ calls(reclaimable under tier a∪b) / Σ calls(top-level defines)
//
// The decision gate is the GAP recovered_toplevel − recovered_local: large ⇒
// tier (b) is worth building; small ⇒ stop at tier (a) and this plan collapses
// into the narrower self-tail plan.
//
// It is gated behind WILE_FRAME_RECLAIM_MEASURE because it RUNS the benchmarks
// at full iteration counts (seconds), which has no place in `go test ./...`. The
// production code it exercises (validate.ClassifyFrameReclaim, the tier-b builder,
// machine.SetCallCounting/CallCounts) is covered by fast unit tests; this harness
// is the deliberate, opt-in measurement.

import (
	"context"
	"fmt"
	"os"
	"sort"
	"strings"
	"testing"

	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/validate"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/machine/compilation"
	"github.com/aalpar/wile/stdlib"
)

// canonicalGabriel mirrors examples/benchmarks/run-canonical.sh. All sixteen are
// import-free R7RS-small, so a KitchenSink engine runs them without library
// resolution. ctak (call/cc) and cpstak (escaping closures) are negative controls
// the classifier must abstain on under BOTH tiers.
var canonicalGabriel = []string{
	"tak", "takl", "ctak", "cpstak", "fib", "triangl", "sum", "sumfp",
	"diviter", "divrec", "deriv", "ackermann", "sieve", "nqueens", "primes", "peval",
}

const importPrelude = "(import (scheme base) (scheme inexact) (scheme write) (scheme time))"

func newBenchmarkEngine(ctx context.Context) (*Engine, error) {
	eng, err := NewEngine(ctx,
		WithProfile(KitchenSink),
		WithSourceFS(stdlib.FS),
		WithLibraryPaths(),
	)
	if err != nil {
		return nil, err
	}
	_, err = eng.EvalMultiple(ctx, importPrelude)
	if err != nil {
		return nil, err
	}
	return eng, nil
}

// frameReclaimRow is one benchmark's measured result.
type frameReclaimRow struct {
	name       string
	nodes      int    // top-level function defines the classifier reasons about
	localOK    uint64 // Σ calls to defines reclaimable under tier (a)
	toplevelOK uint64 // Σ calls to defines reclaimable under tier (a∪b)
	nodeCalls  uint64 // Σ calls to all top-level defines (the denominator)
	envsCopied uint64 // total frame acquisitions (context, all closures)
}

func TestFrameReclaimMeasure(t *testing.T) {
	if os.Getenv("WILE_FRAME_RECLAIM_MEASURE") == "" {
		t.Skip("set WILE_FRAME_RECLAIM_MEASURE=1 to run the Phase-2 frame-reclamation measurement (runs benchmarks at full scale)")
	}

	ctx := context.Background()

	var rows []frameReclaimRow
	var sumLocal, sumTop, sumNode uint64

	for _, name := range canonicalGabriel {
		row, err := measureBenchmark(ctx, t, name)
		if err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		rows = append(rows, row)
		sumLocal += row.localOK
		sumTop += row.toplevelOK
		sumNode += row.nodeCalls
	}

	t.Logf("\n%s", formatFrameReclaimTable(rows, sumLocal, sumTop, sumNode))
}

// measureBenchmark classifies and runs one benchmark, returning its weighted row.
func measureBenchmark(ctx context.Context, t *testing.T, name string) (frameReclaimRow, error) {
	t.Helper()

	src, err := os.ReadFile(fmt.Sprintf("../../examples/benchmarks/%s.scm", name))
	if err != nil {
		return frameReclaimRow{}, err
	}
	// Wrap the whole file as one expression so mutually-recursive top-level
	// defines are visible to each other (R7RS body semantics) and the dynamic
	// run accumulates one program's counters. A space after "begin" keeps source
	// line numbers aligned for any diagnostics.
	wrapped := "(begin " + string(src) + "\n)"

	verdictA, verdictB, err := classifyBenchmark(ctx, wrapped)
	if err != nil {
		return frameReclaimRow{}, fmt.Errorf("classify: %w", err)
	}

	counts, envsCopied, err := runForCounts(ctx, wrapped)
	if err != nil {
		return frameReclaimRow{}, fmt.Errorf("run: %w", err)
	}

	row := frameReclaimRow{name: name, nodes: len(verdictA), envsCopied: envsCopied}
	for fnName := range verdictA {
		c := counts[fnName]
		row.nodeCalls += c
		if verdictA[fnName] {
			row.localOK += c
		}
		if verdictB[fnName] {
			row.toplevelOK += c
		}
	}
	return row, nil
}

// classifyBenchmark parses, expands, and validates the wrapped source against a
// fresh KitchenSink namespace, then classifies its top-level defines under both
// tiers. A separate engine from the dynamic run avoids any expansion side effects
// leaking into the measured run.
func classifyBenchmark(ctx context.Context, wrapped string) (map[string]bool, map[string]bool, error) {
	eng, err := newBenchmarkEngine(ctx)
	if err != nil {
		return nil, nil, err
	}
	env := eng.Environment()

	pr := parser.NewParser(env, true, strings.NewReader(wrapped))
	stx, err := pr.ReadSyntax(ctx)
	if err != nil {
		return nil, nil, fmt.Errorf("parse: %w", err)
	}

	expander := compilation.NewExpanderTimeContinuation(ctx, env, machine.NewVMMacroEvaluator())
	expanded, err := expander.ExpandExpression(stx)
	if err != nil {
		return nil, nil, fmt.Errorf("expand: %w", err)
	}

	result := validate.ValidateExpression(ctx, env, expanded)
	if !result.Ok() {
		return nil, nil, fmt.Errorf("validate: %s", result.Error())
	}

	unit := []validate.ValidatedExpr{result.Expr}
	verdictA := validate.ClassifyFrameReclaim(unit, env, validate.TierLocal)
	verdictB := validate.ClassifyFrameReclaim(unit, env, validate.TierTopLevel)
	return verdictA, verdictB, nil
}

// runForCounts runs the wrapped benchmark with per-callee counting enabled and
// returns the call-count map plus total frame acquisitions.
func runForCounts(ctx context.Context, wrapped string) (map[string]uint64, uint64, error) {
	machine.SetCallCounting(true)
	defer machine.SetCallCounting(false)

	eng, err := newBenchmarkEngine(ctx)
	if err != nil {
		return nil, 0, err
	}
	expr, err := eng.Parse(ctx, wrapped)
	if err != nil {
		return nil, 0, fmt.Errorf("parse: %w", err)
	}
	_, err = eng.Eval(ctx, expr)
	if err != nil {
		return nil, 0, fmt.Errorf("eval: %w", err)
	}
	counters := eng.LastCounters()
	return counters.CallCounts(), counters.EnvsCopied, nil
}

// formatFrameReclaimTable renders the per-benchmark rows and the aggregate gate.
func formatFrameReclaimTable(rows []frameReclaimRow, sumLocal, sumTop, sumNode uint64) string {
	sort.Slice(rows, func(i, j int) bool {
		return rows[i].name < rows[j].name
	})

	var b strings.Builder
	b.WriteString("Escape-gated frame reclamation — Phase 2 measurement (canonical Gabriel)\n")
	b.WriteString("weight = dynamic calls to each top-level define (≈ frame allocations)\n\n")
	fmt.Fprintf(&b, "%-12s %6s %12s %12s %12s %10s\n",
		"benchmark", "#defs", "node-calls", "recov_local", "recov_top", "gap")
	b.WriteString(strings.Repeat("-", 70) + "\n")
	for _, r := range rows {
		fmt.Fprintf(&b, "%-12s %6d %12d %11s %11s %9s\n",
			r.name, r.nodes, r.nodeCalls,
			pct(r.localOK, r.nodeCalls), pct(r.toplevelOK, r.nodeCalls),
			pct(r.toplevelOK-r.localOK, r.nodeCalls))
	}
	b.WriteString(strings.Repeat("-", 70) + "\n")
	fmt.Fprintf(&b, "%-12s %6s %12d %11s %11s %9s\n",
		"AGGREGATE", "", sumNode,
		pct(sumLocal, sumNode), pct(sumTop, sumNode), pct(sumTop-sumLocal, sumNode))
	b.WriteString("\nDecision gate: a large aggregate gap ⇒ build tier (b) / Phase 7;\n")
	b.WriteString("a small gap ⇒ stop at tier (a) (this plan collapses to the self-tail plan).\n")
	return b.String()
}

// pct formats num/den as a percentage, guarding den==0.
func pct(num, den uint64) string {
	if den == 0 {
		return "   n/a"
	}
	return fmt.Sprintf("%5.1f%%", float64(num)/float64(den)*100)
}
