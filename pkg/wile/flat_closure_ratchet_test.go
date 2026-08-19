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

// Deopt ratchets for the flat-closure conversion arc.
//
// Every change in that arc fails toward deopt: a wrong free-variable set, a
// missed identity narrowing, or a widened refusal all end at "compile the
// conservative thing". The conservative thing still evaluates to the right
// value, so a TOTAL deopt passes the entire value-assertion suite silently.
// Checked-in counts are the only instrument that notices.
//
// WHY THIS FILE IS IN pkg/wile AND NOT pkg/machine, which is where the plan put
// it. Nothing arms at all under the internal test helpers. OpSelfTailCall needs
// bodyCalleesAllCaptureSafe to clear the loop's callees, which needs the ambient
// primitives stamped CaptureSafe+Stable — and that stamp is applied by
// registry.WithStableBasePrimitives, which pkg/wile's NewEngine appends for the
// immutable-top-level default (engine.go). bootstrap.NewNamespaceFrame
// deliberately does NOT: it is the policy-free building block, so
// testhelpers.NewFullRuntimeEnv yields a MUTABLE top level with unstamped
// primitives, and a ratchet built on it measures a constant 0. Measured: 0 armed
// sites through testhelpers, 3 through the Engine, over the identical corpus.
//
// The engine's *CompiledCode carries the template, and this file is package
// wile (not wile_test) so it can read it — the same access opcode_fusion_test.go
// takes for the same reason.

package wile

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/machine"
)

// armedSelfTailSitesBaseline is the number of OpSelfTailCall instructions
// flatClosureRatchetCorpus compiles to at the flat-closure arc's phase-0
// baseline.
//
// Measured, not chosen. Do not update it to make a test pass: state which phase
// moved it, and in which direction the plan says that phase may move it.
const armedSelfTailSitesBaseline = 3

// boxedFreeSlotsBaseline is the number of free-vector slots
// flatClosureRatchetCorpus boxes.
//
// Over-boxing is CORRECT — a boxed slot evaluates to the same value — so a T2
// letrec carve-out that regressed to T1, or a captured-read-only variable that
// started boxing, costs only speed and passes every value assertion in the
// suite. This number is what notices. It must not move without a stated reason.
const boxedFreeSlotsBaseline = 0

// flatClosureRatchetCorpus is the fixed loop-and-closure corpus the arc's counts
// are taken over. One expression per entry, because Parse enforces exactly one.
//
// The shapes are chosen for what the arc changes, not for coverage: the
// escaping-closure clause of bodyIsSelfTailReusable is what phase 8 drops, so
// the corpus must contain loops that differ ONLY in whether their body builds a
// closure. Entries 1 and 2 are that pair.
var flatClosureRatchetCorpus = []struct {
	name string
	code string
	want string
}{
	{
		// The control: a named let with no closure in its body. Armed today,
		// and no phase of this arc may disarm it.
		name: "named let, no closure",
		code: `(let loop ((i 0) (acc 0))
		         (if (= i 10) acc (loop (+ i 1) (+ acc i))))`,
		want: "45",
	},
	{
		// The phase-8 subject: identical loop shape, but the body builds a
		// closure per iteration. Refused today by bodyIsSelfTailReusable's
		// escaping-closure clause; admitted once flat closures make that
		// clause's answer permanently "no".
		name: "named let building a closure per iteration",
		code: `(let loop ((i 0) (acc '()))
		         (if (= i 3) (map (lambda (f) (f)) acc)
		             (loop (+ i 1) (cons (lambda () i) acc))))`,
		want: "(2 1 0)",
	},
	{
		// letrec self-reference: design §5.3.1's T2 tier, and unification's
		// inner-loop shape.
		name: "letrec self call",
		code: `(letrec ((f (lambda (n) (if (= n 0) 1 (f (- n 1)))))) (f 5))`,
		want: "1",
	},
	{
		// T3: mutual recursion has no depth-0 self call at all, so it is never
		// armed. Present as a negative control — a count that rises because
		// THIS started arming would be a different change from the one phase 8
		// intends.
		name: "mutual recursion",
		code: `(letrec ((ev? (lambda (n) (if (= n 0) #t (od? (- n 1)))))
		               (od? (lambda (n) (if (= n 0) #f (ev? (- n 1))))))
		         (ev? 4))`,
		want: "#t",
	},
	{
		// A variadic loop. Clause (1) of bodyIsSelfTailReusable refuses a rest
		// parameter outright — a flat parallel store cannot rebind a rest slot —
		// so this is a second negative control, and the one that would notice if
		// the arc ever widened that clause by accident.
		name: "variadic loop",
		code: `(letrec ((f (lambda (n . rest) (if (= n 0) rest (f (- n 1) n)))))
		         (f 3))`,
		want: "(1)",
	},
	{
		// A loop whose parameter is set! in the body. Phase 2 boxes a free
		// variable only if it is ALSO captured; here it is not, so this entry is
		// the "assigned but not captured" control for the boxed-slot count.
		name: "loop with a set! parameter",
		code: `(letrec ((f (lambda (n) (if (= n 0) n (begin (set! n (- n 1)) (f n))))))
		         (f 4))`,
		want: "0",
	},
	{
		// The capture shape the arc exists for: a closure over an enclosing
		// parameter, returned. Contributes no armed site; it is here so the
		// boxed-slot count has a captured-but-not-assigned entry.
		name: "closure over an enclosing parameter",
		code: `(((lambda (a) (lambda (b) (+ a b)))
		         1)
		       2)`,
		want: "3",
	},
}

// compileRatchetCorpus compiles every corpus entry through the public Engine —
// default options, so the top level is immutable and the base primitives carry
// their Stable stamp — and returns the compiled templates.
func compileRatchetCorpus(t *testing.T) []*machine.NativeTemplate {
	t.Helper()
	ctx := context.Background()
	q := make([]*machine.NativeTemplate, 0, len(flatClosureRatchetCorpus))
	for _, tc := range flatClosureRatchetCorpus {
		engine, err := NewEngine(ctx)
		if err != nil {
			t.Fatalf("%s: new engine: %v", tc.name, err)
		}
		expr, err := engine.Parse(ctx, tc.code)
		if err != nil {
			t.Fatalf("%s: parse: %v", tc.name, err)
		}
		compiled, err := engine.Compile(ctx, expr)
		if err != nil {
			t.Fatalf("%s: compile: %v", tc.name, err)
		}
		q = append(q, compiled.template)
	}
	return q
}

// countOpInTemplateTree counts instructions with opcode op in tpl and in every
// sub-template reachable through its literal pool. seen keeps a shared template
// from being counted twice.
func countOpInTemplateTree(
	tpl *machine.NativeTemplate,
	op machine.OpCode,
	seen map[*machine.NativeTemplate]bool,
) int {
	if tpl == nil || seen[tpl] {
		return 0
	}
	seen[tpl] = true
	q := 0
	for _, instr := range tpl.Code() {
		if instr.Op == op {
			q++
		}
	}
	for _, lit := range tpl.Literals() {
		sub, ok := lit.(*machine.NativeTemplate)
		if ok {
			q += countOpInTemplateTree(sub, op, seen)
		}
	}
	return q
}

// TestFlatClosureArmedSiteCount pins how many compiled sites arm OpSelfTailCall
// over flatClosureRatchetCorpus.
//
// Phase 5 (fresh box per rebind) must leave this UNCHANGED — it fixes the
// rebind, it does not widen the gate. Phase 8 (drop the escaping clause from
// bodyIsSelfTailReusable) must move it strictly UP; a count that did not move
// means the clause was not the binding constraint.
func TestFlatClosureArmedSiteCount(t *testing.T) {
	tpls := compileRatchetCorpus(t)
	got := 0
	for i, tpl := range tpls {
		n := countOpInTemplateTree(tpl, machine.OpSelfTailCall, map[*machine.NativeTemplate]bool{})
		t.Logf("%-44s armed=%d", flatClosureRatchetCorpus[i].name, n)
		got += n
	}
	// Vacuity guard. A harness compiling against a mutable top level, or against
	// unstamped primitives, reports zero — and would then "pass" every phase.
	if got == 0 {
		t.Fatalf("no site in the corpus arms OpSelfTailCall — the harness is not " +
			"compiling the way production does, not a real regression")
	}
	if got != armedSelfTailSitesBaseline {
		t.Errorf("armed OpSelfTailCall sites = %d, want %d; if this was intended, "+
			"update armedSelfTailSitesBaseline AND say which phase moved it",
			got, armedSelfTailSitesBaseline)
	}
}

// countBoxedFreeSlotsInTree sums the boxed free-vector slots of tpl and every
// sub-template reachable through its literal pool.
func countBoxedFreeSlotsInTree(
	tpl *machine.NativeTemplate,
	seen map[*machine.NativeTemplate]bool,
) int {
	if tpl == nil || seen[tpl] {
		return 0
	}
	seen[tpl] = true
	q := 0
	for _, b := range tpl.FreeBoxed() {
		if b {
			q++
		}
	}
	for _, lit := range tpl.Literals() {
		sub, ok := lit.(*machine.NativeTemplate)
		if ok {
			q += countBoxedFreeSlotsInTree(sub, seen)
		}
	}
	return q
}

// TestFlatClosureBoxedSlotCount pins how many free slots flatClosureRatchetCorpus
// boxes. See boxedFreeSlotsBaseline for why a number has to be checked in.
//
// The corpus's zero is meaningful rather than vacuous: the corpus contains both
// halves of the predicate separately — "loop with a set! parameter" is assigned
// and not captured, "closure over an enclosing parameter" is captured and not
// assigned — so a predicate that collapsed to either conjunct alone would move
// this off zero. TestBoxingSet in pkg/machine/compilation carries the
// both-conjuncts fixtures.
func TestFlatClosureBoxedSlotCount(t *testing.T) {
	tpls := compileRatchetCorpus(t)
	got := 0
	for i, tpl := range tpls {
		n := countBoxedFreeSlotsInTree(tpl, map[*machine.NativeTemplate]bool{})
		t.Logf("%-44s boxed=%d", flatClosureRatchetCorpus[i].name, n)
		got += n
	}
	if got != boxedFreeSlotsBaseline {
		t.Errorf("boxed free slots = %d, want %d; over-boxing is correct and "+
			"therefore silent, so state which phase moved this and why",
			got, boxedFreeSlotsBaseline)
	}
}

// TestFlatClosureBoxingPredicateHasTeeth is the vacuity guard for the zero
// above: a source that genuinely needs a box must produce one. Without it,
// deleting markBoxedFreeVars outright would leave every ratchet green.
func TestFlatClosureBoxingPredicateHasTeeth(t *testing.T) {
	ctx := context.Background()
	engine, err := NewEngine(ctx)
	if err != nil {
		t.Fatalf("new engine: %v", err)
	}
	// `x` is captured by two closures AND assigned through one of them, so it
	// must be shared rather than copied.
	const code = `(lambda (x) (cons (lambda () (set! x 1)) (lambda () x)))`
	expr, err := engine.Parse(ctx, code)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	compiled, err := engine.Compile(ctx, expr)
	if err != nil {
		t.Fatalf("compile: %v", err)
	}
	got := countBoxedFreeSlotsInTree(compiled.template, map[*machine.NativeTemplate]bool{})
	if got == 0 {
		t.Fatalf("a captured-and-assigned variable produced no boxed slot — the "+
			"boxing predicate is not running, so %s's zero proves nothing",
			"TestFlatClosureBoxedSlotCount")
	}
}

// TestFlatClosureRatchetCorpusStillEvaluates keeps the corpus honest. A ratchet
// over sources that stopped compiling — or that quietly changed meaning — pins
// nothing, so every entry's value is asserted here and an edit to a corpus entry
// has to state what the entry now computes.
func TestFlatClosureRatchetCorpusStillEvaluates(t *testing.T) {
	ctx := context.Background()
	for _, tc := range flatClosureRatchetCorpus {
		t.Run(tc.name, func(t *testing.T) {
			engine, err := NewEngine(ctx)
			if err != nil {
				t.Fatalf("new engine: %v", err)
			}
			got, err := engine.EvalMultiple(ctx, tc.code)
			if err != nil {
				t.Fatalf("eval: %v", err)
			}
			if got.SchemeString() != tc.want {
				t.Errorf("evaluates to %s, want %s", got.SchemeString(), tc.want)
			}
		})
	}
}
