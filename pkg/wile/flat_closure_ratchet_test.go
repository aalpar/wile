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
//
// Moved 3 -> 4 in phase 8, which drops the escaping-closure clause from
// bodyIsSelfTailReusable. The mover is "closure per iteration, map OUTSIDE the
// loop", and its sibling "map INSIDE the loop" staying at 0 is what says the
// widening did not also reach past bodyCalleesAllCaptureSafe.
const armedSelfTailSitesBaseline = 4

// boxedFreeSlotsBaseline is the number of free-vector slots
// flatClosureRatchetCorpus boxes.
//
// Over-boxing is CORRECT — a boxed slot evaluates to the same value — so a T2
// letrec carve-out that regressed to T1, or a captured-read-only variable that
// started boxing, costs only speed and passes every value assertion in the
// suite. This number is what notices. It must not move without a stated reason.
//
// Moved 0 -> 2 in phase 4b, and the reason is the letrec* initialization-order
// hazard, not mutation: once free variables are COPIED, `ev?` and `od?` in the
// mutual-recursion entry are each captured by the other's init before their own
// store has run, so a value copy would freeze #!void. Both are T3, which the
// tiers box. The two named-let / letrec-self-call entries stay at 0 — they are
// T2, where OpMakeClosure's self back-patch supplies the value without a cell.
const boxedFreeSlotsBaseline = 2

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
		// A closure-building loop that is refused by the OTHER clause, and is
		// here to keep the two reasons apart. Its exit branch calls map, which
		// is stamped InvokesProcedure and therefore not capture-safe, so
		// bodyCalleesAllCaptureSafe refuses it whatever the escaping-closure
		// clause says. Measured: still armed=0 with that clause deleted.
		//
		// It was written as the phase-8 subject and could not serve as one.
		// The entry below is the subject; this one stays as the control that
		// distinguishes "phase 8 widened the gate" from "phase 8 widened it
		// past the callee check too", which it must not.
		name: "closure per iteration, map INSIDE the loop",
		code: `(let loop ((i 0) (acc '()))
		         (if (= i 3) (map (lambda (f) (f)) acc)
		             (loop (+ i 1) (cons (lambda () i) acc))))`,
		want: "(2 1 0)",
	},
	{
		// THE PHASE-8 SUBJECT. Same loop, but the closures are applied after it
		// returns, so the body's callees are =, +, cons and the self call —
		// all capture-safe. The only thing refusing it is the escaping-closure
		// clause of bodyIsSelfTailReusable, which is what phase 8 drops.
		name: "closure per iteration, map OUTSIDE the loop",
		code: `(map (lambda (f) (f))
		         (let loop ((i 0) (acc '()))
		           (if (= i 3) acc
		               (loop (+ i 1) (cons (lambda () i) acc)))))`,
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

// TestFlatClosureDoesNotPinItsCreatorsFrame is the arc's headline property,
// asserted directly rather than inferred from an allocation slope.
//
// A closure's static link must be a structural ROOT — parent == nil — so the
// activation frames it was created under are not reachable from it at all, and
// its free variables are values in a vector instead. Everything else in this
// arc exists to make that safe.
func TestFlatClosureDoesNotPinItsCreatorsFrame(t *testing.T) {
	ctx := context.Background()
	engine, err := NewEngine(ctx)
	if err != nil {
		t.Fatalf("new engine: %v", err)
	}
	// Two nesting levels, so the returned closure captures a variable of a frame
	// that is neither its own nor its immediate creator's.
	got, err := engine.EvalMultiple(ctx, `(((lambda (a) (lambda (b) (lambda () (+ a b)))) 1) 2)`)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	cls, ok := unwrapValue(got).(*machine.MachineClosure)
	if !ok {
		t.Fatalf("result is %T, want a *machine.MachineClosure", unwrapValue(got))
	}
	link := cls.Link()
	if link == nil {
		t.Fatal("closure recorded no static link")
	}
	if link.Parent() != nil {
		t.Errorf("static link has a lexical parent — the closure still pins the " +
			"frames it was created under, which is the one thing this arc exists " +
			"to stop")
	}
	if len(cls.Free()) != 2 {
		t.Errorf("free vector has %d entries, want 2 (a and b): the variables have "+
			"to travel SOMEWHERE once the link no longer reaches them", len(cls.Free()))
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

// letFrameCorpus is the fixture set for the let-slot merging census
// (memory/2026-08-19-let-slot-merging-impl.local.md). Distinct from
// flatClosureRatchetCorpus because the shapes it must discriminate are different:
// where the corpus above varies whether a loop body builds a closure, this one
// varies where a `let` sits relative to a frame that still exists at run time.
var letFrameCorpus = []struct {
	name string
	code string
	want string
}{
	{
		// Merges: the let sits directly in a lambda body, tail position, so it
		// emits no OpPopEnv today either.
		name: "tail let in a lambda body",
		code: `((lambda (n) (let ((a (+ n 1)) (b (+ n 2))) (+ a b))) 10)`,
		want: "23",
	},
	{
		// Merges. Non-tail, so it is one of the 8% that has an OpPopEnv today.
		name: "non-tail let in a lambda body",
		code: `((lambda (n) (+ (let ((a (+ n 1))) a) n)) 10)`,
		want: "21",
	},
	{
		// Merges twice. Nested lets are the schelog:unify shape.
		name: "nested lets in a lambda body",
		code: `((lambda (n) (let ((a (+ n 1))) (let ((b (+ a 1))) (let ((c (+ b 1))) c)))) 10)`,
		want: "13",
	},
	{
		// Merges. A let whose binder shadows a parameter is the case that
		// forbids appending binders to the enclosing frame directly — see §1 of
		// the plan. The value pins that the two slots stay distinct.
		name: "let shadowing a parameter",
		code: `((lambda (x) (+ (let ((x 1)) x) x)) 10)`,
		want: "11",
	},
	{
		// Does NOT merge: no enclosing frame to merge into. The OUTERMOST
		// top-level let is the one push this whole corpus still costs.
		name: "top-level let",
		code: `(let ((a 1) (b 2)) (+ a b))`,
		want: "3",
	},
	{
		// Merges twice, into the outermost let's own frame. A pushing let is a
		// shape for whatever nests inside it, so depth at the top level collapses
		// the same way it does inside a procedure — this is the shape that used to
		// be 81 of the 134 depth != 0 emit sites over this suite.
		//
		// The frame is three slots wide even though its OpPushEnv is emitted when
		// only one is known, which is what the operand patch in CompileValidatedLet
		// exists for. A wrong width faults on an out-of-range slot rather than
		// reading the wrong one, so the value arm alone would catch it — but only
		// this census catches the merge silently not happening.
		name: "nested lets at the top level",
		code: `(let ((a 1)) (let ((b (+ a 1))) (let ((c (+ b 1))) (+ a b c))))`,
		want: "6",
	},
	{
		// Merges. let* at the top level is the case whose binders do not exist
		// when OpPushEnv is emitted AND whose nested slots arrive later still, so
		// it exercises both sources of late growth against one operand.
		name: "top-level let* over a nested let",
		code: `(let* ((a 1) (b (+ a 1))) (let ((c (+ b 1))) (+ a b c)))`,
		want: "6",
	},
	{
		// Merges, and the merged slot outlives the let: a closure created inside
		// the nested top-level let is pushed a free variable that now lives in the
		// OUTER let's frame at depth 0. That push is the second population — 50 of
		// the 134 sites — and it is only correct if the closure's free vector is
		// filled from the patched frame.
		name: "closure over a merged slot at the top level",
		code: `(let ((a 1)) (let ((b (+ a 1))) ((lambda () (+ a b)))))`,
		want: "3",
	},
	{
		// Merges, and the let-bound value is captured by an escaping closure.
		// Flat closures copy, so slot reuse is invisible to it — that property is
		// what removes this change's soundness gate.
		name: "closure over a let binding",
		code: `(((lambda (n) (let ((a (+ n 1))) (lambda () a))) 10))`,
		want: "11",
	},
	{
		// Merges. let* adds its binders incrementally at compile time, so its
		// slot count is only correct after the loop that creates them.
		name: "let* in a lambda body",
		code: `((lambda (n) (let* ((a (+ n 1)) (b (+ a 1))) (+ a b))) 10)`,
		want: "23",
	},
	{
		// Merges. A named let is a letrec whose init is a lambda: the letrec
		// binding merges, and the loop's own parameter frame is a real one.
		name: "named let in a lambda body",
		code: `((lambda (n) (let loop ((i 0) (acc 0)) (if (= i n) acc (loop (+ i 1) (+ acc i))))) 5)`,
		want: "10",
	},
}

// pushEnvSitesBaseline / popEnvSitesBaseline are the OpPushEnv and OpPopEnv
// instruction counts letFrameCorpus compiles to.
//
// Measured, not chosen. This is a CENSUS, not a gate ratchet: let-slot merging is
// an unconditional rewrite, so there is no armed/disarmed count to pin. What it
// catches is the merge silently not happening — every entry falling back to the
// push path still evaluates to the same value, so no assertion in the suite would
// otherwise notice.
//
// Moved 11 -> 1 and 3 -> 1 by phase 2 of the let-slot merging plan. Moved 1 -> 4
// by extending merging to the top level, which added FOUR fixtures each keeping
// exactly one push: a pushing let is now a shape, so the outermost top-level let
// still pushes and everything nested inside it merges into that frame.
//
// The number to watch is therefore push-sites PER top-level-let fixture, which is
// 1. If a nested top-level let ever pushes again this rises to 6, and the value
// arm stays green — which is the whole reason the census exists.
const (
	pushEnvSitesBaseline = 4
	popEnvSitesBaseline  = 4
)

// compileLetFrameCorpus compiles every letFrameCorpus entry through the public
// Engine and returns the compiled templates.
func compileLetFrameCorpus(t *testing.T) []*machine.NativeTemplate {
	t.Helper()
	ctx := context.Background()
	q := make([]*machine.NativeTemplate, 0, len(letFrameCorpus))
	for _, tc := range letFrameCorpus {
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

// TestLetFrameMergeCensus pins how many env-frame pushes and pops letFrameCorpus
// compiles to. Phase 2 of the let-slot merging plan must move both strictly DOWN.
func TestLetFrameMergeCensus(t *testing.T) {
	tpls := compileLetFrameCorpus(t)
	pushes, pops := 0, 0
	for i, tpl := range tpls {
		np := countOpInTemplateTree(tpl, machine.OpPushEnv, map[*machine.NativeTemplate]bool{})
		nq := countOpInTemplateTree(tpl, machine.OpPopEnv, map[*machine.NativeTemplate]bool{})
		t.Logf("%-44s pushenv=%d popenv=%d", letFrameCorpus[i].name, np, nq)
		pushes += np
		pops += nq
	}
	if pushes != pushEnvSitesBaseline {
		t.Errorf("OpPushEnv sites = %d, want %d; if this was intended, update "+
			"pushEnvSitesBaseline AND say which phase moved it",
			pushes, pushEnvSitesBaseline)
	}
	if pops != popEnvSitesBaseline {
		t.Errorf("OpPopEnv sites = %d, want %d; if this was intended, update "+
			"popEnvSitesBaseline AND say which phase moved it",
			pops, popEnvSitesBaseline)
	}
}

// TestLetFrameCorpusValues is the value arm of the census: every shape it counts
// pushes for must still evaluate correctly. A census alone cannot tell a merge
// from a miscompile.
func TestLetFrameCorpusValues(t *testing.T) {
	ctx := context.Background()
	for _, tc := range letFrameCorpus {
		engine, err := NewEngine(ctx)
		if err != nil {
			t.Fatalf("%s: new engine: %v", tc.name, err)
		}
		got, err := engine.EvalMultiple(ctx, tc.code)
		if err != nil {
			t.Fatalf("%s: eval: %v", tc.name, err)
		}
		if got.SchemeString() != tc.want {
			t.Errorf("%s = %s, want %s", tc.name, got.SchemeString(), tc.want)
		}
	}
}

// mergedSlotsBaseline is how many binding slots letFrameCorpus's merged `let`s
// add to their enclosing shapes — the width the parameter frame grew by, and
// therefore the per-apply copy the merge would cost if copyForApplyInto did not
// reslice past it.
//
// Measured, not chosen. It is the copy-side twin of pushEnvSitesBaseline: that
// one says the frames are gone, this one says their slots are free. Fork F4 of
// memory/2026-08-14-oppushenv-frame-pooling-scope.local.md decided the exemption;
// dropping it leaves every value assertion in this file passing while
// BindingsCopied/EnvsCopied climbs (measured +6.4% on schelog-zebra).
//
// 13 exempt against 9 copied: over this corpus the merged slots are the MAJORITY
// of shape width, which is why the exemption is worth a field on the frame.
const mergedSlotsBaseline = 13

// shapeWidthAndCopyCount sums, over a template tree, each shape's slot count and
// the leading prefix copyForApplyInto actually transfers. Their difference is
// the merged-`let` population.
func shapeWidthAndCopyCount(
	tpl *machine.NativeTemplate,
	seen map[*machine.NativeTemplate]bool,
) (int, int) {
	if tpl == nil || seen[tpl] {
		return 0, 0
	}
	seen[tpl] = true
	width, copied := 0, 0
	shape := tpl.Shape()
	if shape != nil {
		lenv := shape.LocalEnvironment()
		if lenv != nil {
			width += len(lenv.Bindings())
			copied += lenv.CopyCount()
		}
	}
	for _, lit := range tpl.Literals() {
		sub, ok := lit.(*machine.NativeTemplate)
		if ok {
			w, c := shapeWidthAndCopyCount(sub, seen)
			width += w
			copied += c
		}
	}
	return width, copied
}

// TestLetFrameMergeSlotsAreExemptFromApplyCopy pins that merged `let` slots
// widen the shape without widening the per-apply copy.
func TestLetFrameMergeSlotsAreExemptFromApplyCopy(t *testing.T) {
	tpls := compileLetFrameCorpus(t)
	width, copied := 0, 0
	for i, tpl := range tpls {
		w, c := shapeWidthAndCopyCount(tpl, map[*machine.NativeTemplate]bool{})
		t.Logf("%-44s width=%d copied=%d exempt=%d", letFrameCorpus[i].name, w, c, w-c)
		width += w
		copied += c
	}
	if width-copied != mergedSlotsBaseline {
		t.Errorf("exempt slots = %d (width %d, copied %d), want %d; if this was "+
			"intended, update mergedSlotsBaseline AND say which phase moved it",
			width-copied, width, copied, mergedSlotsBaseline)
	}
	if copied == 0 {
		t.Errorf("copied = 0: the exemption swallowed the parameters too")
	}
}

// TestMergedLetSlotReadsVoidAfterRecycling is the correctness arm of the
// exemption. An exempt slot is never written by the apply copy, so whatever the
// pooled frame's backing array holds IS what a read before the `let`'s own store
// sees. A letrec forward reference is that read.
//
// The frame this exercises has been recycled: `dirty` writes the same slot
// indices first, so a reset that merely ZEROED would leave a nil values.Value
// and the probe would fail with "expected 1 value, got 0" rather than #!void.
func TestMergedLetSlotReadsVoidAfterRecycling(t *testing.T) {
	ctx := context.Background()
	engine, err := NewEngine(ctx)
	if err != nil {
		t.Fatalf("new engine: %v", err)
	}
	got, err := engine.EvalMultiple(ctx, `
(define (dirty n)
  (let ((x n) (y (* n 2)) (z (* n 3)))
    (+ x y z)))
(define (probe)
  (letrec ((a b) (b 1))
    (list a b)))
(define (churn i)
  (if (= i 0) 'done (churn (- i 1))))
(dirty 5)
(churn 50)
(map dirty '(1 2 3 4 5))
(probe)
`)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	if got.SchemeString() != "(#<void> 1)" {
		t.Errorf("probe = %s, want (#<void> 1)", got.SchemeString())
	}
}
