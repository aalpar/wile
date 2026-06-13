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
// Measurement caveat (surfaced per row): LastCounters does NOT aggregate
// sub-context counters (engine.go), and NewSubContext does not allocate a
// callCounts map, so named calls executed inside a sub-context (apply, map,
// for-each, eval) are NOT counted. A row with sub-contexts > 0 therefore reports
// LOWER-BOUND weights. The benchmarks that drive the verdict (fib/tak/takl/
// ackermann/divrec) are direct self-recursion through Apply (sub-contexts == 0),
// so the headline gap is unaffected; the subctx column makes any biased row
// visible rather than silently mixed into the aggregate.
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

	"github.com/aalpar/wile/environment"
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

// importPrelude makes the standard primitives the hot benchmark functions call
// (+, -, *, less-than, =, car, cdr) IMMUTABLE by importing them. KitchenSink
// binds these ambiently in the base namespace where they are mutable (not
// imported), so without this the classifier cannot trust ANY primitive and
// recovers 0 percent by construction, independent of tier. Importing
// (scheme base) rebinds them as imported/immutable, the prerequisite for
// measuring the actual top-level-define tier-a vs tier-b gap.
const importPrelude = "(import (scheme base) (scheme inexact) (scheme write) (scheme time))"

// newBenchmarkEngine builds an engine with KitchenSink plus the embedded stdlib
// so importing (scheme base) resolves, then runs the prelude. When immutable is
// set, WithImmutableTopLevel() is enabled so the compiler stamps the producer's
// Stable bit on defined-once, never-set! top-level defines — the prerequisite
// for the classifier's tier-(b) (recovered_toplevel) verdict. With it off, no
// same-unit define is Stable and the classifier recovers only tier-(a).
func newBenchmarkEngine(ctx context.Context, immutable bool) (*Engine, error) {
	opts := []EngineOption{
		WithProfile(KitchenSink),
		WithSourceFS(stdlib.FS),
		WithLibraryPaths(),
	}
	if immutable {
		opts = append(opts, WithImmutableTopLevel())
	}
	eng, err := NewEngine(ctx, opts...)
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
	name        string
	nodes       int    // top-level function defines the classifier reasons about
	localOK     uint64 // Σ calls to defines reclaimable under tier (a)
	toplevelOK  uint64 // Σ calls to defines reclaimable under tier (a∪b)
	nodeCalls   uint64 // Σ calls to all top-level defines (the denominator)
	envsCopied  uint64 // total frame acquisitions (context, all closures)
	subContexts uint64 // sub-contexts created — when >0, node-call weights undercount
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

	// recovered_local: compile WITHOUT WithImmutableTopLevel — no same-unit
	// define is Stable, so the classifier recovers only tier-(a).
	verdictA, err := classifyCompiled(ctx, wrapped, false)
	if err != nil {
		return frameReclaimRow{}, fmt.Errorf("classify (flag off): %w", err)
	}
	// recovered_toplevel: compile WITH the flag — defined-once, never-set!
	// top-level defines carry Stable, so same-unit edges are immutable (tier-b).
	verdictB, err := classifyCompiled(ctx, wrapped, true)
	if err != nil {
		return frameReclaimRow{}, fmt.Errorf("classify (flag on): %w", err)
	}

	counters, err := runForCounts(ctx, wrapped)
	if err != nil {
		return frameReclaimRow{}, fmt.Errorf("run: %w", err)
	}
	counts := counters.CallCounts()

	row := frameReclaimRow{
		name:        name,
		nodes:       len(verdictA),
		envsCopied:  counters.EnvsCopied,
		subContexts: counters.SubContextsCreated,
	}
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

	// Guard the silent phantom: classifier found defines but the join produced
	// zero matching calls. With no sub-contexts there is no undercounting excuse,
	// so this means the verdict keys (Sym.Key) and counter keys (tpl.Name)
	// diverged — the whole row would print n/a and be read as "no recovery". Fail
	// loudly. With sub-contexts present it is the documented undercount, not a
	// key mismatch; warn instead (the subctx column flags the row).
	if row.nodes > 0 && row.nodeCalls == 0 {
		if row.subContexts == 0 {
			return frameReclaimRow{}, fmt.Errorf("%s: %d top-level defines but zero matched calls and no sub-contexts — verdict/counter key formats diverged (Sym.Key vs tpl.Name); join is degenerate", name, row.nodes)
		}
		t.Logf("WARNING %s: zero matched node-calls but %d sub-contexts — defines likely ran in sub-contexts (uncounted); row is a lower bound", name, row.subContexts)
	}
	return row, nil
}

// classifyCompiled expands the wrapped source ONCE against a fresh KitchenSink
// namespace, builds the validated tree from that expansion, compiles the SAME
// expanded syntax to stamp the producer's Stable bit on every same-unit define,
// then classifies the tree against the stamped env. immutable selects
// WithImmutableTopLevel: off ⇒ no define is Stable (tier-a / recovered_local);
// on ⇒ defined-once, never-set! defines are Stable (tier-b / recovered_toplevel).
//
// The single shared expansion is load-bearing: the validated call-site symbols
// and the compiler's stamped binding scopes derive from the same syntax, so
// GetBinding(callSite, scopes).IsStable() resolves to the binding the compiler
// stamped (plan OQ-1 — split-key resolution consistency). A separate engine from
// the dynamic run avoids any expansion side effects leaking into the measured run.
func classifyCompiled(ctx context.Context, wrapped string, immutable bool) (map[string]bool, error) {
	eng, err := newBenchmarkEngine(ctx, immutable)
	if err != nil {
		return nil, err
	}
	env := eng.Environment()

	pr := parser.NewParser(env, true, strings.NewReader(wrapped))
	stx, err := pr.ReadSyntax(ctx)
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}

	evaluator := machine.NewVMMacroEvaluator()
	expander := compilation.NewExpanderTimeContinuation(ctx, env, evaluator)
	expanded, err := expander.ExpandExpression(stx)
	if err != nil {
		return nil, fmt.Errorf("expand: %w", err)
	}

	// Validated tree (for the classifier) from the single expansion.
	result := validate.ValidateExpression(ctx, env, expanded)
	if !result.Ok() {
		return nil, fmt.Errorf("validate: %s", result.Error())
	}
	unit := []validate.ValidatedExpr{result.Expr}

	// Compile the SAME expanded syntax to stamp Stable on each same-unit define.
	// This is the producer side of the seam: declareDefineBinding sets m.Stable =
	// v.StableInUnit when ImmutableTopLevel is on. CompileExpression mirrors
	// compilation.ExpandAndCompile minus Optimize() (irrelevant to binding stamps).
	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := compilation.NewCompileTimeCallContext(ctx, false)
	compiler := compilation.NewCompileTimeContinuation(tpl, env, evaluator)
	err = compiler.CompileExpression(cctx, expanded)
	if err != nil {
		return nil, fmt.Errorf("compile: %w", err)
	}

	// Positive control (plan Task 3 CROSSCHECK, errors lens — CRITICAL): the stamp
	// must have landed exactly as the flag dictates. Without this, a broken stamp
	// path leaves every binding non-Stable, IsStable() is uniformly false, and
	// recovered_toplevel reads ~1.6% — a number that looks like a measurement but
	// is an artifact of unstamped bindings, with no error raised. Fail loudly.
	err = assertStampLanded(env, result.Expr, immutable)
	if err != nil {
		return nil, err
	}

	return validate.ClassifyFrameReclaim(unit, env), nil
}

// assertStampLanded is the positive control: the first top-level function
// define's binding must report IsStable() == immutable in the compiled env. In
// the flag-on env it confirms the producer stamped Stable (StableInUnit holds
// for a defined-once, never-set! benchmark define); in the flag-off env it
// confirms nothing was stamped. A mismatch means the stamp path or the flag
// plumbing regressed and the verdict is not a real measurement.
func assertStampLanded(env *environment.EnvironmentFrame, expr validate.ValidatedExpr, immutable bool) error {
	defs := topLevelFunctionDefines(expr)
	if len(defs) == 0 {
		return fmt.Errorf("positive control: no top-level function define found in unit")
	}
	// Range over EVERY top-level function define the classifier reads, not just
	// the first: a stamp path that stamped the head but mis-stamped a later
	// define would otherwise pass the control while that define's verdict is an
	// artifact. The producer stamps Stable ONLY when StableInUnit holds
	// (defined-once ∧ never-set!, errors.go finalizeStability), so the expected
	// bit is immutable ∧ def.StableInUnit — NOT immutable alone. This stays
	// correct if a future benchmark's define is legitimately non-Stable (set! or
	// twice-defined): the producer declines to stamp it and the control accepts.
	for _, def := range defs {
		name := def.Name()
		b := env.GetBinding(name.Sym, name.Scopes())
		if b == nil {
			return fmt.Errorf("positive control: define %q has no binding after compile — stamp path broke", name.Sym.Key)
		}
		want := immutable && def.StableInUnit
		if b.IsStable() != want {
			return fmt.Errorf("positive control: define %q IsStable()=%v, want %v (WithImmutableTopLevel=%v, StableInUnit=%v) — stamp/flag mismatch; recovered verdict would be an artifact", name.Sym.Key, b.IsStable(), want, immutable, def.StableInUnit)
		}
	}
	return nil
}

// topLevelFunctionDefines returns every top-level function define in expr,
// flattening a top-level (begin ...) one level (R7RS body semantics — the
// harness always wraps the program in a begin). The classifier reasons about
// exactly this set, so the positive control must cover all of it.
func topLevelFunctionDefines(expr validate.ValidatedExpr) []*validate.ValidatedDefine {
	var body []validate.ValidatedExpr
	begin, ok := expr.(*validate.ValidatedBegin)
	if ok {
		body = begin.Body()
	} else {
		body = []validate.ValidatedExpr{expr}
	}
	var defs []*validate.ValidatedDefine
	for _, e := range body {
		d, ok := e.(*validate.ValidatedDefine)
		if ok && d.IsFunction {
			defs = append(defs, d)
		}
	}
	return defs
}

// runForCounts runs the wrapped benchmark with per-callee counting enabled and
// returns the resulting VM counters snapshot.
func runForCounts(ctx context.Context, wrapped string) (machine.VMCounters, error) {
	machine.SetCallCounting(true)
	defer machine.SetCallCounting(false)

	eng, err := newBenchmarkEngine(ctx, false)
	if err != nil {
		return machine.VMCounters{}, err
	}
	expr, err := eng.Parse(ctx, wrapped)
	if err != nil {
		return machine.VMCounters{}, fmt.Errorf("parse: %w", err)
	}
	_, err = eng.Eval(ctx, expr)
	if err != nil {
		return machine.VMCounters{}, fmt.Errorf("eval: %w", err)
	}
	return eng.LastCounters(), nil
}

// formatFrameReclaimTable renders the per-benchmark rows and the aggregate gate.
func formatFrameReclaimTable(rows []frameReclaimRow, sumLocal, sumTop, sumNode uint64) string {
	sort.Slice(rows, func(i, j int) bool {
		return rows[i].name < rows[j].name
	})

	var b strings.Builder
	b.WriteString("Escape-gated frame reclamation — Phase 2 measurement (canonical Gabriel)\n")
	b.WriteString("weight = dynamic calls to each top-level define (≈ frame allocations)\n\n")
	fmt.Fprintf(&b, "%-12s %6s %12s %8s %12s %12s %10s\n",
		"benchmark", "#defs", "node-calls", "subctx", "recov_local", "recov_top", "gap")
	b.WriteString(strings.Repeat("-", 78) + "\n")
	biased := false
	for _, r := range rows {
		flag := ""
		if r.subContexts > 0 {
			flag = "*"
			biased = true
		}
		fmt.Fprintf(&b, "%-12s %6d %12d %7d%1s %11s %11s %9s\n",
			r.name, r.nodes, r.nodeCalls, r.subContexts, flag,
			pct(r.localOK, r.nodeCalls), pct(r.toplevelOK, r.nodeCalls),
			pct(r.toplevelOK-r.localOK, r.nodeCalls))
	}
	b.WriteString(strings.Repeat("-", 78) + "\n")
	fmt.Fprintf(&b, "%-12s %6s %12d %8s %11s %11s %9s\n",
		"AGGREGATE", "", sumNode, "",
		pct(sumLocal, sumNode), pct(sumTop, sumNode), pct(sumTop-sumLocal, sumNode))
	if biased {
		b.WriteString("\n* sub-contexts > 0: named calls in apply/map/for-each/eval are NOT counted\n")
		b.WriteString("  (LastCounters does not aggregate sub-contexts); these rows are lower bounds.\n")
	}
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
