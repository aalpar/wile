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

// Phase distinctness: the behavioural gate for Flatt binding model Stage A.
//
// Plan: plans/2026-09-08-flatt-binding-model-a-impl.local.md, Task 2; design
// section 6.1. Written BEFORE any mechanism, on purpose: a change of this shape
// fails toward the old behaviour, and the old behaviour is green. If bulk rows
// were installed but never won a resolution, every other test in the suite
// would still pass.
//
// Two labels, and the difference is the whole point (memory:
// plan-pin-tests-must-fail-on-master):
//
//   - RED pins FAILED on master. They carried a t.Skip naming Task 7 while the
//     mechanism was being built, so the branch stayed mergeable and the tests
//     stayed visible; Task 7 deleted the skips and they are live now. Each one's
//     RED state was measured with the skip stripped, in an isolated worktree,
//     and is recorded in its comment — that measurement is the evidence the pin
//     records a CHANGE rather than an answer, and it cannot be re-taken once the
//     mechanism has landed.
//   - GUARDs PASS on both sides. They are NOT skipped — a skipped guard would
//     run for the first time only after Task 7 landed, which is exactly when it
//     was supposed to be protecting something.
//
// The store-level twin of this file's property lives in
// phase_distinctness_twin_test.go; the coordinate-level form it mirrors is
// pkg/environment/phase_distinctness_test.go.

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"
	"testing/fstest"

	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// phaseDistinctnessEngine builds the engine every pin in this file uses.
//
// KitchenSink is required, not incidental: the zero-value Tiny profile cannot
// load (scheme base) at all, so a pin that imports would fail for the wrong
// reason. The source FS and library path are what make (import ...) resolvable;
// they are inert for the pins that do not import, and are passed uniformly
// anyway so that an import line is the only thing that ever varies between a
// pin and its twin.
func phaseDistinctnessEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(wile.StdLibFS),
		wile.WithLibraryPaths("lib"),
	)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

// evalPhaseDistinctness runs one program and returns its printed value.
func evalPhaseDistinctness(t *testing.T, src string) (string, error) {
	t.Helper()
	eng := phaseDistinctnessEngine(t)
	v, err := eng.EvalMultiple(context.Background(), src)
	if err != nil {
		return "", err
	}
	return v.SchemeString(), nil
}

// phase1TransformerCadrSrc is the canonical D4 program: a procedural
// transformer whose body calls a name the base defines in Scheme.
//
// The Bound twin below is this same text with one import line prepended, so the
// two pins differ by exactly that line.
const phase1TransformerCadrSrc = `(define-syntax pick
  (er-macro-transformer
    (lambda (form rename compare)
      (cadr form))))
(pick 7 8)`

// TestPhase1_ProceduralTransformerUnboundWithoutImport is D4's gate: after
// Stage A a PROCEDURAL transformer body no longer sees phase-0 names for free.
//
// RED on master because NOTHING RAISES — measured, the program compiles and
// EvalMultiple returns 7. Green only after Task 7 deletes the ambient tier.
//
// cadr, NOT car. Design section 6.1's row names car, and car is not
// discriminating: measured on a full engine, car is ambient=true AND
// exact@1=true, because registry/apply.go's phaseTargets loop registers 155 of
// 218 core primitives at both phases while writeCoordinates
// (pkg/environment/environment_frame.go) diverts only the SEALED PHASE-0 write
// to AnyPhase(). Stage A moves that write to ExactPhase(0) and leaves the
// phase-1 sealed slot alone, so car keeps resolving at phase 1 after A and a
// pin written on it could never go green. cadr is DEFINED in bootstrap Scheme
// (pkg/registry/core/bootstrap_procedures.scm) — a phase-0 sealed write — so it
// measures ambient=true, exact@1=false and reaches phase 1 today ONLY through
// the ambient tier. It is also the name (import (for-syntax (scheme base)))
// demonstrably mints at exact phase 1: measured, exact@1 goes false to true
// across that import and only that import, while a plain (import (scheme base))
// mints exact@0 instead.
//
// Design section 6.1's car row is wrong rather than imprecise, and Task 11 owes
// it a correction. The design's own A4 census already contained the refutation.
func TestPhase1_ProceduralTransformerUnboundWithoutImport(t *testing.T) {
	eng := phaseDistinctnessEngine(t)
	_, err := eng.EvalMultiple(context.Background(), phase1TransformerCadrSrc)

	// This assertion carries the whole discriminating weight; today err is nil
	// and the value is 7. The two below only constrain HOW it raises, and they
	// are what stop a green being produced by the wrong failure — a Task 5/6
	// regression that broke er-macro-transformer outright would satisfy the
	// first assertion alone.
	qt.Assert(t, err, qt.IsNotNil,
		qt.Commentf("a transformer body's cadr must not resolve at phase 1 without a for-syntax import"))
	qt.Assert(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
		qt.Commentf("got: %s", err))
	qt.Assert(t, strings.Contains(err.Error(),
		`no such binding "cadr" with compatible scopes at phase 1 of this unit's macro tower`), qt.IsTrue,
		qt.Commentf("got: %s", err))
}

// forSyntaxProbeFS holds a library whose export the ambient tier does NOT
// cover, which is what gives the Bound pin's last subtest teeth on master.
var forSyntaxProbeFS = fstest.MapFS{
	"lib/forsyntaxprobe.sld": &fstest.MapFile{
		Data: []byte(`(define-library (forsyntaxprobe)
  (import (scheme base))
  (export probe-helper)
  (begin
    (define (probe-helper x) (+ x 100))))`),
	},
}

// TestPhase1_ProceduralTransformerBoundWithImport is a GUARD, not a RED pin.
//
// Design section 6.1 lists this row as "n/a on master". Measured, it is neither
// n/a nor red — it PASSES today, because the ambient tier already covers all
// 248 (scheme base) exports at phase 1, so adding the for-syntax import changes
// nothing observable. There is no program on master in which a (scheme base)
// name REQUIRES the import.
//
// What the import does today is structural: it gives an exact phase-1 slot to
// exactly those base exports that reach phase 1 only through ambient, which is
// exactly the set Stage A strands. So this guard pins the mechanism the
// migration rests on. After Task 7 it is the only thing keeping these programs
// alive, and a regression in the for-syntax route would otherwise surface as a
// mass failure with no named cause.
//
// The last subtest is the half with teeth on master: probe-helper is a
// library-private definition the ambient tier does not cover, so a phase-0
// import is observably insufficient and the for-syntax import observably fixes
// it. It discriminates on the MECHANISM rather than on the change.
func TestPhase1_ProceduralTransformerBoundWithImport(t *testing.T) {
	ctx := context.Background()

	newProbeEngine := func(t *testing.T) *wile.Engine {
		t.Helper()
		eng, err := wile.NewEngine(ctx,
			wile.WithProfile(wile.KitchenSink),
			wile.WithSourceFS(wile.StdLibFS),
			wile.WithSourceFS(forSyntaxProbeFS),
			wile.WithLibraryPaths("lib"),
		)
		qt.Assert(t, err, qt.IsNil)
		t.Cleanup(func() {
			_ = eng.Close()
		})
		return eng
	}

	t.Run("scheme base supplies assoc at phase 1", func(t *testing.T) {
		v, err := newProbeEngine(t).EvalMultiple(ctx, `
			(import (for-syntax (scheme base)))
			(define-syntax m
			  (lambda (stx)
			    (if (assoc 2 (list (cons 1 10) (cons 2 20)))
			        (syntax 'found)
			        (syntax 'missing))))
			(m)
		`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v.SchemeString(), qt.Equals, "found")
	})

	t.Run("scheme cxr supplies caddr at phase 1", func(t *testing.T) {
		// caddr is NOT in base.sld's export list — it lives in cxr.sld — which
		// is the measurement behind Fork 4's "(scheme cxr) where the body uses
		// caddr/cadddr/cdddr".
		v, err := newProbeEngine(t).EvalMultiple(ctx, `
			(import (for-syntax (scheme base)))
			(import (for-syntax (scheme cxr)))
			(define-syntax m
			  (lambda (stx) (caddr (list (syntax 1) (syntax 2) (syntax 99)))))
			(m)
		`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v.SchemeString(), qt.Equals, "99")
	})

	t.Run("er-macro-transformer body too", func(t *testing.T) {
		v, err := newProbeEngine(t).EvalMultiple(ctx, `
			(import (for-syntax (scheme base)))
			(define-syntax m
			  (er-macro-transformer (lambda (form rename compare) (car (list 42)))))
			(m)
		`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v.SchemeString(), qt.Equals, "42")
	})

	t.Run("the for-syntax import is what supplies the binding", func(t *testing.T) {
		const src = `
			(define-syntax m
			  (lambda (stx)
			    (if (= 101 (probe-helper 1)) (syntax 'ok) (syntax 'bad))))
			(m)`

		_, err := newProbeEngine(t).EvalMultiple(ctx, "(import (forsyntaxprobe))"+src)
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, err.Error(), qt.Contains,
			`no such binding "probe-helper" with compatible scopes at phase 1 of this unit's macro tower`)

		v, err := newProbeEngine(t).EvalMultiple(ctx, "(import (for-syntax (forsyntaxprobe)))"+src)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v.SchemeString(), qt.Equals, "ok")
	})
}

// evalAtPhase runs one program on a fresh engine and returns only its error.
func evalAtPhase(t *testing.T, src string) error {
	t.Helper()
	eng := phaseDistinctnessEngine(t)
	_, err := eng.EvalMultiple(context.Background(), src)
	return err
}

// transformerAtPhase returns a program whose er-macro-transformer BODY compiles
// at phase n, prefixed by prologue. n is 1 or 2. Exactly one thing varies with
// n: the number of begin-for-syntax wrappers. Same form, same raise site, same
// name — so anything the two rungs do differently is the RULE differing, not
// the program.
//
// The body returns (if ... 1 0) rather than the raw assoc result on purpose:
// returning a pair makes expansion raise "function call must be a proper list
// in call form", which would mask the binding question entirely.
func transformerAtPhase(n int, prologue string) string {
	body := `(define-syntax m (er-macro-transformer (lambda (f r c) (if (assoc 'a '((a . 1))) 1 0))))`
	invoke := "(m)"
	for i := 1; i < n; i++ {
		body = "(begin-for-syntax " + body + ")"
		invoke = ""
	}
	return prologue + "\n" + body + "\n" + invoke
}

// TestPhase2_UniformRule is design section 6.1's phase-2 row. RED on master:
// ambient is phase-BLIND, so a phase-0-only name answers a probe at every rung
// of the tower and both halves of the first subtest fail.
//
// The pin's content is UNIFORMITY, not "phase 2 is empty". transformerAtPhase
// varies exactly one thing across the rungs, so the form, the raise site
// (CompileSymbol's scoped arm) and the name are held constant and the two
// expected diagnostics differ in one character, the digit. Anything the rungs
// do differently is the rule differing, which is what A must not introduce.
//
// The name is assoc, and the choice is load-bearing: assoc has NO exact slot at
// any phase (measured: p0/p1/p2 all absent, ambient present), so after A the
// phase-0 initial import is its only supplier and phases 1 and 2 are symmetric.
// car is unusable here for the same reason it is unusable in the first pin —
// it carries an exact-phase-1 slot but none at phase 2, so a car-based pin
// would assert phase 2 DIFFERING from phase 1, the opposite of this test.
func TestPhase2_UniformRule(t *testing.T) {
	t.Run("a name the dialect supplies only at phase 0 is unbound at every higher rung", func(t *testing.T) {
		for _, n := range []int{1, 2} {
			t.Run(fmt.Sprintf("phase %d", n), func(t *testing.T) {
				err := evalAtPhase(t, transformerAtPhase(n, ""))
				qt.Assert(t, err, qt.IsNotNil,
					qt.Commentf("phase %d resolved assoc with no phase-%d import", n, n))
				want := fmt.Sprintf(
					`no such binding "assoc" with compatible scopes at phase %d of this unit's macro tower`, n)
				qt.Assert(t, strings.Contains(err.Error(), want), qt.IsTrue,
					qt.Commentf("want substring: %s\ngot: %s", want, err.Error()))
			})
		}
	})

	// GUARD. Passes on master vacuously, ambient covering what the import would
	// supply. It is here because without it the first subtest is satisfied by a
	// phase-2 rung that binds nothing at all, and "phase 2 is empty" is not
	// "phase 2 behaves as phase 1". The two prologues are the SAME rule at two
	// coordinates: (for-syntax X) is (for-meta 1 X). Measured on master,
	// (import (for-meta 2 (scheme base))) does give assoc an exact-phase-2
	// binding, so the route exists before A is written. That a phase-2 body
	// still RESOLVES through it once ambient stops masking it is the one
	// prediction here master cannot check — if Task 7 finds otherwise, this
	// rung is where it surfaces, and that is a defect appearing, not a test to
	// weaken.
	t.Run("GUARD: the matching phase-N import binds it, at every rung", func(t *testing.T) {
		prologues := map[int]string{
			1: `(import (for-syntax (scheme base)))`,
			2: `(import (for-meta 2 (scheme base)))`,
		}
		for _, n := range []int{1, 2} {
			t.Run(fmt.Sprintf("phase %d", n), func(t *testing.T) {
				err := evalAtPhase(t, transformerAtPhase(n, prologues[n]))
				qt.Assert(t, err, qt.IsNil)
			})
		}
	})

	// GUARD, and the reason D7 must not be re-filed. LookupPrimitiveExpander
	// reads env.Expand() — an ABSOLUTE phase 1 — against a table whose sole
	// write is SealedWriteViewAt(PhaseExpand). A phase-2 body expands macros
	// BECAUSE that read is absolute, not despite it. This subtest passes for
	// the absolute read's sake; it is not evidence that phase 2 owns an
	// expander table, and it must not be "fixed" by making the read relative.
	// A9 measured NextPhase() there breaking bootstrap loading outright, with
	// ambient still present.
	//
	// when/unless/cond are deliberate: each has an exact-phase-1 slot and NO
	// ambient binding, so this subtest is already independent of the tier A
	// deletes. else is avoided for the opposite reason — it is ambient-only,
	// and Task 7 Step 3 reworks the ambient-last descent in lookupLiteralBinding
	// that serves it.
	t.Run("GUARD: a phase-2 body still expands macros", func(t *testing.T) {
		err := evalAtPhase(t, `(begin-for-syntax (begin-for-syntax
			(define seen (when #t (unless #f (cond (#t 1)))))))`)
		qt.Assert(t, err, qt.IsNil)
	})
}

// TestDeclarativeMacroNeedsNoImport is a GUARD: it passes on both sides.
//
// A syntax-rules TEMPLATE is expanded into the USE site, which is phase 0, so
// the template's names resolve at phase 0. That asymmetry is WHY a declarative
// macro needs no for-syntax import while a procedural one does, and subtests 3
// and 4 are the paired demonstration: the same helper name works from a
// template and is unbound from a transformer body.
//
// One premise behind this row does NOT survive measurement, and Task 10 must
// not repeat it. The claim was that the ellipsis and underscore identifiers
// hold an ambient slot and no exact-phase slot, so after A they must come from
// the dialect declaration or every syntax-rules macro breaks. The antecedent is
// true; the consequent is false. Under WithoutAmbientBindings() both hold ZERO
// slots at every tier and phase, and both still expand correctly — because the
// matcher is NAME-KEYED: pkg/internal/match/syntax_compiler.go compares
// sym.Key() against the ellipsis spelling and against "_". What the two slots
// are actually for is stated where they are created, in
// pkg/registry/core/specialforms.go: "needs a binding for library export
// resolution (like else, =>, ..., _)", and that import runs at phase 0.
//
// syntax-rules itself is in the same position: it holds an exact-phase-1 slot
// from the primitive-expander registration, independent of any ambient slot
// (TestKeywordSlotIsNotTheExpanderSlot pins exactly that), and deleting the
// ambient tier does not remove it. So on today's shape this row survives A
// whether or not the dialect declares anything at phase 1.
func TestDeclarativeMacroNeedsNoImport(t *testing.T) {
	t.Run("template over a phase-0-only name", func(t *testing.T) {
		got, err := evalPhaseDistinctness(t, `
			(define-syntax lookup (syntax-rules () ((_ key alist) (assoc key alist))))
			(lookup 'b '((a 1) (b 2)))`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, got, qt.Equals, "(b 2)")
	})

	t.Run("ellipsis and underscore", func(t *testing.T) {
		got, err := evalPhaseDistinctness(t, `
			(define-syntax drop-first
			  (syntax-rules () ((_ _skip rest ...) (vector-map (lambda (n) (* n 2)) (vector rest ...)))))
			(drop-first ignored 1 2 3)`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, got, qt.Equals, "#(2 4 6)")
	})

	t.Run("template resolves at the use site", func(t *testing.T) {
		got, err := evalPhaseDistinctness(t, `
			(define-syntax call-helper (syntax-rules () ((_ n) (helper n))))
			(define (helper n) (* n 10))
			(call-helper 4)`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, got, qt.Equals, "40")
	})

	t.Run("contrast: the same name in a transformer BODY is at phase 1", func(t *testing.T) {
		_, err := evalPhaseDistinctness(t, `
			(define (helper n) (* n 10))
			(define-syntax call-helper
			  (er-macro-transformer (lambda (form rename compare) (list (rename 'quote) (helper 4)))))
			(call-helper)`)
		qt.Assert(t, err, qt.ErrorIs, werr.ErrNoSuchBinding)
		// Name and phase clause only: the wrapper prefixes and the column are
		// outside this task's control.
		qt.Assert(t, err, qt.ErrorMatches, `(?s).*"helper".*at phase 1 of this unit's macro tower.*`)
	})
}

// TestPhase0DefineDoesNotShadowPhase1 is a GUARD, not a gate.
//
// Design section 6.1 names it and EXCLUDES it from the gate: "a phase-0
// (define car ...) does not shadow phase 1's car" already holds on master,
// since an exact-phase-0 slot classifies as tierNone at a phase-1 query
// (tierOf, inside probeTiersLocked). It is a preservation test. Do NOT cite it
// as evidence A worked.
//
// It is not vacuous. Measured: relaxing that one line from
// `s.phase.level != phase` to `s.phase.level > phase` — the shape a phase
// DESCENT would take — turns this red with "USER-CAR", because a
// tierExactMutable phase-0 slot outranks the phase-1 sealed base. That is
// precisely the regression Task 7 could introduce while relocating the sealed
// base onto (ExactPhase(0), sealed).
//
// ORDER IS LOAD-BEARING. The define must stay ABOVE the import: both the user's
// phase-0 slot and the for-syntax import's phase-1 slot are tierExactMutable
// with empty scope sets, so the cardinality tie-break never fires and the
// earlier-created slot wins on slot-list order alone. Hoisting the import above
// the define makes the same mutated build PASS, and the guard stops guarding.
func TestPhase0DefineDoesNotShadowPhase1(t *testing.T) {
	eng := phaseDistinctnessEngine(t)
	ctx := context.Background()

	const src = `
		(define (car x) 'USER-CAR)
		(import (for-syntax (scheme base)))
		(define-syntax probe
		  (er-macro-transformer
		    (lambda (form rename compare)
		      (list (rename 'quote) (car (list 'PRIMITIVE 'ignored))))))
		(probe)`
	got, err := eng.EvalMultiple(ctx, src)
	qt.Assert(t, err, qt.IsNil)
	// Equality, not "not USER-CAR": the weaker form would also accept an
	// unbound-at-phase-1 error, which is a different outcome and belongs to
	// TestPhase1_ProceduralTransformerUnboundWithoutImport.
	qt.Assert(t, got.SchemeString(), qt.Equals, "PRIMITIVE")

	// Vacuity guard: the phase-0 redefinition really is in effect. Without it
	// the test passes for the wrong reason should a future immutable-top-level
	// tightening start refusing the define.
	shadow, err := eng.EvalMultiple(ctx, `(car (list 1 2))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, shadow.SchemeString(), qt.Equals, "USER-CAR")
}
