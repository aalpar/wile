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
	"maps"
	"slices"
	"testing"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/machine/compilation"

	qt "github.com/frankban/quicktest"
)

// Enforced invariant: the peephole optimizer preserves every coverage-relevant
// source span.
//
// Coverage (coverage.Entries) aggregates execution by *unique SourceContext
// across all PCs*, so a source span stays reported as long as at least one
// instruction still references it. The peephole optimizer collapses instruction
// sequences into fused/promoted superinstructions, and a collapse assigns the
// fused instruction a single source ref — dropping the refs of the other
// absorbed instructions (see EditPlan.rewriteCode, machine/edit_plan.go).
//
// In principle that could drop a span entirely. In practice it does not: the
// compiler tags a fusable group with redundant source spans (e.g. the call-form
// span sits on SaveContinuation, the callee push, AND PullApply), so an absorbed
// span always survives on a sibling. Empirical verification (2026-06-26) over a
// varied corpus found 430 cross-source collapses and ZERO span losses; the whole
// machine+wile+coverage suites under instrumentation found zero.
//
// This test pins that property as an invariant rather than an accident. It
// compiles each form to a template via the engine's own ExpandAndCompile seam
// (pre-Optimize), snapshots the coverage-relevant spans across the whole
// template tree, runs Optimize(), and asserts no span lost its last referencing
// PC. If this ever fails, the peephole has started dropping coverage data and
// the fix in plans/2026-06-26-editplan-preserve-collapsed-sources.local.md
// (EditPlan extraSources) becomes justified.
func TestOptimize_PreservesCoverageSpans(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink))
	c.Assert(err, qt.IsNil)

	// Fusion-heavy shapes: multi-line calls (operator/operands on distinct
	// lines), deep nesting, let / named-let loops, cond, higher-order, and
	// recursion. These exercise fuseLoadPush, fusePullApply,
	// fuseCallForeignCached/Generic, and promoted compound args.
	forms := map[string]string{
		"add-multiline":  "(define (f a b)\n  (+ a\n     b))",
		"nested-car-cdr": "(define (g x)\n  (car\n    (cdr x)))",
		"vector-ref":     "(define (h v i)\n  (vector-ref\n    v\n    i))",
		"let-arith":      "(let ((x 1)\n      (y 2))\n  (* x\n     y))",
		"cond-mix":       "(define (k n)\n  (if (< n 0)\n      (- n)\n      (+ n 1)))",
		"deep-call":      "(define (m a b c)\n  (cons a\n        (cons b\n              (cons c '()))))",
		"fib":            "(define (fib n)\n  (if (< n 2)\n      n\n      (+ (fib (- n 1))\n         (fib (- n 2)))))",
		"sum":            "(define (sum lst)\n  (if (null? lst)\n      0\n      (+ (car lst)\n         (sum (cdr lst)))))",
		"map2":           "(define (map2 f a b)\n  (if (null? a)\n      '()\n      (cons (f (car a) (car b))\n            (map2 f (cdr a) (cdr b)))))",
		"let-loop":       "(let loop ((i 0) (acc 0))\n  (if (>= i 10)\n      acc\n      (loop (+ i 1)\n            (+ acc i))))",
		"tree-sum":       "(define (tree-sum t)\n  (cond ((null? t) 0)\n        ((pair? t)\n         (+ (tree-sum (car t))\n            (tree-sum (cdr t))))\n        (else t)))",
		"compose":        "(define (compose f g)\n  (lambda (x)\n    (f (g x))))",
	}

	names := slices.Sorted(maps.Keys(forms))

	for _, name := range names {
		tpl, cerr := compileUnoptimized(ctx, eng, forms[name], name+".scm")
		c.Assert(cerr, qt.IsNil, qt.Commentf("[%s] compile", name))

		before := coverageSpans(tpl)
		tpl.Optimize()
		after := coverageSpans(tpl)

		for span := range before {
			c.Check(after[span], qt.IsTrue,
				qt.Commentf("[%s] Optimize dropped coverage span %s (no surviving PC references it)", name, span))
		}
	}
}

// compileUnoptimized runs the engine's expand+compile pipeline WITHOUT the
// trailing Optimize() (mirrors expandAndCompileOptimized minus the optimize
// step), so the caller can observe source attribution before and after the
// peephole pass.
func compileUnoptimized(ctx context.Context, eng *Engine, code, source string) (*machine.NativeTemplate, error) {
	expr, err := eng.ParseWithSource(ctx, code, source)
	if err != nil {
		return nil, err
	}
	return compilation.ExpandAndCompile(ctx, eng.env, expr.stx, nil, eng.inlineThreshold, eng.maxExpandDepth)
}

// coverageSpans returns the set of coverage-relevant source spans referenced by
// a template and all its sub-templates (closure bodies live in the literal pool
// as *NativeTemplate). The filter matches coverage.Entries: nil sources and
// sources with no filename are not coverage-relevant.
func coverageSpans(tpl *machine.NativeTemplate) map[string]bool {
	q := make(map[string]bool)
	collectCoverageSpans(tpl, q)
	return q
}

func collectCoverageSpans(tpl *machine.NativeTemplate, q map[string]bool) {
	for pc := 0; pc < tpl.CodeLen(); pc++ {
		src := tpl.SourceAt(pc)
		if src == nil || src.File == "" {
			continue
		}
		key := fmt.Sprintf("%s:%d:%d-%d:%d", src.File,
			src.Start.Line(), src.Start.Column(), src.End.Line(), src.End.Column())
		q[key] = true
	}
	for _, lit := range tpl.Literals() {
		sub, ok := lit.(*machine.NativeTemplate)
		if ok {
			collectCoverageSpans(sub, q)
		}
	}
}
