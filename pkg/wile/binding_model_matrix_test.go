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
	"errors"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// The binding-model matrix: {define, set!, redefine, delete} × {sealed
// primitive, bootstrap macro name, import, own define, expand-phase primitive,
// phase-1 define} × {top level, library body, child namespace, report
// namespace}, pinned as BEHAVIOR — shadow vs supersede vs refuse, with the
// exact sentinel — so the store fold (design Phase C) diffs against a
// photograph instead of against memory. Every row passes against the PRE-fold
// tree; a fold that flips any row has changed semantics, not encoding.
//
// Two claims this table owns (measured, plan §0 M6/M7):
//   - The define/set! asymmetry: define NEVER lands on a sealed slot (it
//     targets the mutable layer), while a compile-RESOLVED set! pin CAN reach
//     one when nothing refuses it (mutable-top-level variant) — G13's own-frame
//     restriction is about the deferred write, and define is its observable
//     face. The "define shadows sealed primitive" rows assert a captured VALUE,
//     which is insensitive to what later happens to the binding, so they do not
//     carry this claim; the h/car pair in
//     TestBindingModelMatrixMutableTopLevel ("set! observed through an existing
//     call site" / "define NOT observed through an existing call site") does,
//     by observing through a procedure that re-resolves car at call time
//     instead of a value captured once.
//   - car exists twice: the sealed phase-0 binding and the registry's
//     (1, sealed) expand copy. Each is hermetic against a mutation at the
//     other's phase — phase 0 pinned by "expand copy survives phase-0 set!"
//     (mutable table), phase 1 by "define-for-syntax over expand copy leaves
//     phase 0 intact". Neither direction is a supersede: both mutations land
//     in the MUTABLE tier of their own phase and shadow. The phase-1 half of
//     that was not always true — until the expand copies were sealed
//     (registry.Apply's phaseTargets) a define-for-syntax wrote through the
//     copy in place. "define-for-syntax shadows expand copy, pre-resolved
//     reference intact" is what discriminates the two readings; "...value
//     visible at phase 1, by name" reads the same under either.
//
// See plans/2026-08-05-flat-binding-model-{design,impl}.local.md.

type matrixRow struct {
	name string
	// units: one EvalMultiple call per entry, in order. EvalMultiple parses and
	// compiles each top-level FORM in an entry's string independently
	// (pkg/wile/engine.go evalMultiple) — an entry holding several forms is one
	// runMatrix iteration but several compile units underneath (see
	// "StableInUnit" below for why that granularity matters), so runMatrix's
	// per-entry failedAt index can only pin a wantErr failure to the ENTRY, not
	// to a form inside it. A row that needs the failure pinned to one specific
	// form splits that entry into one-form-per-entry (e.g. "undefine own
	// define").
	units []string
	// wantErr non-nil: the LAST unit must fail errors.Is(err, wantErr), AND the
	// failure must come from that last unit specifically (see runMatrix's
	// failedAt check) — an earlier unit failing with the same sentinel is a
	// false pass, not the behavior under test.
	// wantErr nil: every unit succeeds and the last value renders want.
	wantErr error
	want    string
}

func matrixEngine(t *testing.T, extra ...wile.EngineOption) *wile.Engine {
	t.Helper()
	ctx := context.Background()
	opts := []wile.EngineOption{
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(fstest.MapFS{
			"lib-r2.scm": &fstest.MapFile{Data: []byte(`(define-library (lib-r2)
  (export f)
  (import (scheme base))
  (begin (define x 1) (set! x 2) (define (f) x)))
`)},
			"lib-shadow.scm": &fstest.MapFile{Data: []byte(`(define-library (lib-shadow)
  (export g)
  (import (scheme base))
  (begin (define map 3) (define (g) map)))
`)},
		}),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	}
	opts = append(opts, extra...)
	eng, err := wile.NewEngine(ctx, opts...)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

func runMatrix(t *testing.T, rows []matrixRow, extra ...wile.EngineOption) {
	t.Helper()
	ctx := context.Background()
	for _, row := range rows {
		t.Run(row.name, func(t *testing.T) {
			eng := matrixEngine(t, extra...)
			var lastVal string
			var lastErr error
			failedAt := -1
			for i, unit := range row.units {
				v, err := eng.EvalMultiple(ctx, unit)
				lastErr = err
				if err != nil {
					failedAt = i
					break
				}
				lastVal = v.Internal().SchemeString()
			}
			if row.wantErr != nil {
				qt.Assert(t, lastErr, qt.IsNotNil)
				qt.Assert(t, errors.Is(lastErr, row.wantErr), qt.IsTrue,
					qt.Commentf("want %v, got: %v", row.wantErr, lastErr))
				// A row's wantErr pins the failure to its LAST unit. If an
				// earlier unit fails with the same sentinel — e.g. a fold that
				// broke namespace-undefine! itself, or broke define-for-syntax
				// outright — the row above would still pass without ever
				// reaching the behavior under test. That is a hole in the net,
				// not a pass.
				qt.Assert(t, failedAt, qt.Equals, len(row.units)-1,
					qt.Commentf("wrong unit failed: want the LAST unit (index %d) "+
						"to fail, got failure at unit %d (%q)",
						len(row.units)-1, failedAt, row.units[failedAt]))
				return
			}
			qt.Assert(t, lastErr, qt.IsNil)
			qt.Assert(t, lastVal, qt.Equals, row.want)
		})
	}
}

// The engine default: immutable top level.
func TestBindingModelMatrix(t *testing.T) {
	rows := []matrixRow{
		// P1: define over a sealed primitive is a SHADOW — new binding, original
		// untouched (the captured f still holds the primitive). This asserts a
		// captured VALUE only — see the file header and the h/car pair in
		// TestBindingModelMatrixMutableTopLevel for the slot-level claim.
		{name: "define shadows sealed primitive",
			units: []string{`(define f car) (define car 42) (list car (f '(7 8)))`},
			want:  "(42 7)"},
		// P3: unshadowed primitives are Stable anchors under the default.
		{name: "set! sealed primitive refused",
			units:   []string{`(set! car 1)`},
			wantErr: werr.ErrImmutableBinding},
		// P4: redefine of a Stable own define refused (defined-once across units).
		{name: "redefine own define refused",
			units:   []string{`(define x 1)`, `(define x 2)`},
			wantErr: werr.ErrImmutableBinding},
		// StableInUnit: a set! in the SAME unit keeps the binding non-Stable.
		// "Unit" here means "one compiled top-level form", not "one EvalMultiple
		// call": EvalMultiple parses and compiles each top-level form
		// independently (pkg/wile/engine.go evalMultiple), so bare
		// "(define y 1) (set! y 2) y" is THREE units and y is Stable by the
		// time the set! unit compiles. begin splices its body into one form, so
		// this is the actual same-unit case — [Engine.EvalProgram] does the
		// equivalent begin-splice structurally for a whole program; this row
		// open-codes it for one row. Measured: without the begin-wrap this row
		// fails with ErrImmutableBinding (probed via a scratch program against
		// matrixEngine's config).
		{name: "set! own define same unit ok",
			units: []string{`(begin (define y 1) (set! y 2) y)`},
			want:  "2"},
		// ...and a LATER-unit set! of a defined-once binding is refused.
		{name: "set! own define later unit refused",
			units:   []string{`(define z 1)`, `(set! z 2)`},
			wantErr: werr.ErrImmutableBinding},
		// P4: a define after an import wins, and a same-unit set! of it is then
		// permitted (R7RS §5.3.1).
		//
		// THE MECHANISM CHANGED AND THIS BLOCK USED TO DESCRIBE THE OLD ONE. It
		// read "define supersedes an import IN PLACE and drops the import
		// provenance". That was accurate while imports installed at
		// (ExactPhase(0), mutable) — the same coordinates a top-level define
		// writes — so the two shared one slot and define was an ASSIGNMENT
		// through the import, with compile_define.go clearing m.Imported to
		// re-permit set!.
		//
		// Imports now install at (ExactPhase(0), sealed) (T2), so the define gets
		// its OWN (ExactPhase(0), mutable) T1 slot and SHADOWS the import. The
		// import's provenance is not dropped because it is not touched; set! is
		// permitted because the slot being set was never imported. `want: "10"`
		// is unchanged and PASSED THROUGHOUT — which is exactly why the stale
		// rationale had to be corrected by reading, not by a failing test.
		//
		// Uses g (lib-shadow's export) rather than map: map is ALSO a Stable
		// stdlib anchor in this profile with NO import at all (see "set!
		// imported refused" and its two control rows below), so asserting "10"
		// against map would be produced by
		// define-over-a-sealed-primitive-plus-same-unit-set! (already covered
		// by "set! own define same unit ok" above) and would pin nothing
		// about import supersession specifically. The (g) call between the
		// import and the begin-wrap fails directly (ErrNoSuchBinding) if the
		// import ever stops binding g, so this row doesn't depend solely on
		// the sibling control rows to mean what it claims. Measured against
		// matrixEngine's config.
		{name: "define supersedes import then set!",
			units: []string{`(import (lib-shadow))`, `(g)`, `(begin (define g 9) (set! g 10) g)`},
			want:  "10"},
		// The ORDER twin of the row above, and the row that pins the tier move.
		// An import arriving AFTER a define of the same name must leave the
		// define standing. It did not: one shared slot meant the import's
		// SetOwnGlobalValue wrote straight over the user's value and
		// markBindingImported then stamped the user's binding imported=true.
		// Measured before the move, this row read "#<machine-closure>" —
		// lib-shadow's g — with the define silently gone.
		//
		// importConflicts' doc comment claims "a pre-existing user definition is
		// not an import and is left to shadow"; this row is that sentence.
		{name: "import after define does not clobber the define",
			units: []string{`(define g 9)`, `(import (lib-shadow))`, `g`},
			want:  "9"},
		// ...and the import is still THERE, underneath. Shadowing, not refusal:
		// deleting the shadow must reveal the library's g rather than leave the
		// name unbound. Without this the row above would also pass if the import
		// had simply been dropped on the floor.
		{name: "import after define is shadowed, not lost",
			units: []string{
				`(define g 9)`,
				`(import (lib-shadow))`,
				`(namespace-undefine! (interaction-environment) 'g)`,
				`(g)`,
			},
			want: "3"},
		// P7: namespace-undefine! over an IMPORT still deletes it. The import
		// moved to the sealed tier, where the primitive's blanket refusal
		// ("cannot undefine sealed binding … a primitive or bootstrap
		// procedure") would otherwise have caught it — a wrong message and a
		// withdrawn capability, since building a restricted namespace by
		// importing and then removing names is what the primitive is for.
		// PrimNamespaceUndefine narrows the refusal with IsImported(); this row
		// is the discriminator, and "undefine sealed primitive refused" below is
		// the control proving the refusal itself still works.
		{name: "undefine over import deletes it",
			units: []string{
				`(import (lib-shadow))`,
				`(begin (namespace-undefine! (interaction-environment) 'g)
					(namespace-bound? (interaction-environment) 'g))`,
			},
			want: "#f"},
		// P2: set! on an imported binding refused (R7RS §5.2). Uses g rather
		// than map for the reason given at "define supersedes import then
		// set!" above (map is Stable in this profile with no import at all —
		// see the two control rows below). Measured against matrixEngine's
		// config.
		{name: "set! imported refused",
			units:   []string{`(import (lib-shadow))`, `(set! g 1)`},
			wantErr: werr.ErrImmutableBinding},
		// Control for the row above: without the import g is simply unbound,
		// unlike map which is Stable either way — this is the discriminator
		// that makes "set! imported refused" mean something. Measured against
		// matrixEngine's config.
		{name: "set! g without import is unbound, not immutable",
			units:   []string{`(set! g 1)`},
			wantErr: werr.ErrNoSuchBinding},
		// Companion control: map (unlike g) refuses set! even with NO import
		// in play at all — it's a Stable stdlib anchor in this profile
		// regardless. This is the premise the map-vs-g rationale above
		// depends on, previously only asserted in comments; now
		// machine-checked. Measured against matrixEngine's config.
		{name: "set! map without import refused (stdlib anchor)",
			units:   []string{`(set! map 1)`},
			wantErr: werr.ErrImmutableBinding},
		// A bootstrap macro NAME is free at phase 0: define binds the value,
		// the phase-1 transformer keeps expanding.
		{name: "define over bootstrap macro name",
			units: []string{`(define when 5) (list when (when #t 1))`},
			want:  "(5 1)"},
		// M8: delete own define works; delete sealed refused. Three separate
		// units (not a begin-wrap, and not one multi-form EvalMultiple call) so
		// runMatrix's per-unit failedAt can pin the failure to the READ
		// specifically: a fold that broke namespace-undefine! itself — raising
		// ErrNoSuchBinding from the undefine call rather than from the later
		// read — would satisfy the same sentinel from the WRONG unit if the
		// define+undefine+read were lumped into fewer entries.
		{name: "undefine own define",
			units: []string{
				`(define w 1)`,
				`(namespace-undefine! (interaction-environment) 'w)`,
				`w`,
			},
			wantErr: werr.ErrNoSuchBinding},
		{name: "undefine sealed primitive refused",
			units:   []string{`(namespace-undefine! (interaction-environment) 'car)`},
			wantErr: werr.ErrImmutableBinding},
		// Phase-1 define + set! at phase 1; hermeticity from phase 0 (P5).
		// get-fv is a phase-1 closure compiled BEFORE the set!, so its
		// reference to fv resolves (at compile time) to the binding that
		// exists then. Reading 2 back through it after the set! proves the
		// set! mutated that SAME binding in place — a set! that silently
		// created a fresh phase-1 shadow instead would leave get-fv's
		// already-resolved reference at 1 (see the h/car pair in
		// TestBindingModelMatrixMutableTopLevel for the general form of this
		// discriminator; a bare post-set! read of fv by name cannot
		// distinguish mutation from shadowing, since name resolution always
		// finds the newest compatible binding). reveal is an
		// er-macro-transformer: a transformer's return value is used directly
		// as its expansion, which is how these rows surface a phase-1 value at
		// phase 0. Measured against matrixEngine's config.
		{name: "phase-1 define set! at own phase",
			units: []string{
				`(define-for-syntax fv 1)`,
				`(define-for-syntax get-fv (lambda () fv))`,
				`(begin-for-syntax (set! fv 2))`,
				`(define-syntax reveal-fv (er-macro-transformer (lambda (form rename compare) (get-fv)))) (reveal-fv)`,
			},
			want: "2"},
		{name: "phase-0 cannot read phase-1 define",
			units:   []string{`(define-for-syntax fv 1)`, `fv`},
			wantErr: werr.ErrNoSuchBinding},
		// M7: define-for-syntax over the expand-phase registry copy of car
		// SHADOWS it — the registry copy is at (1, sealed) and the
		// define-for-syntax lands at (1, mutable), different coordinates, so
		// CreateGlobalBindingAt mints a new slot. Phase 0's car is a third
		// binding again, untouched. Three rows pin this end to end: this one
		// observes phase 0 untouched (a PRESERVATION assertion — it reads "9"
		// under supersede and under shadow alike, so it discriminates nothing
		// on its own); the next observes the new value BY NAME (which equally
		// can't distinguish the two); the third is the discriminator.
		{name: "define-for-syntax over expand copy leaves phase 0 intact",
			units: []string{`(define-for-syntax car 42) (car '(9 8))`},
			want:  "9"},
		// By-name view only: reveal-car reads car back through a phase-1
		// er-macro-transformer, confirming define-for-syntax's value is
		// visible at phase 1. This does NOT by itself distinguish supersede
		// from shadow — name resolution finds the newest compatible binding
		// either way, so "42" is produced under both readings. (Round-1
		// review of this row was satisfied by this alone under the name
		// "...supersedes expand copy..."; the row is renamed and its claim
		// narrowed to what it actually observes. The row below pins which
		// reading is true.) Measured against matrixEngine's config.
		{name: "define-for-syntax value visible at phase 1, by name",
			units: []string{
				`(define-for-syntax car 42)`,
				`(define-syntax reveal-car (er-macro-transformer (lambda (form rename compare) car))) (reveal-car)`,
			},
			want: "42"},
		// The actual supersede-vs-shadow discriminator: use-car is a phase-1
		// closure compiled BEFORE the define-for-syntax (same technique as the
		// get-fv row above and the h/car pair in
		// TestBindingModelMatrixMutableTopLevel), so its reference to car
		// resolves at COMPILE time to the registry's (1, sealed) copy. Under
		// supersede-in-place that same binding would now hold 42 and applying
		// it would fail with ErrNotAProcedure; under a shadow the pre-resolved
		// reference is untouched and the call still reaches the primitive.
		// Measured against matrixEngine's config: 1 — SHADOW. This is the
		// phase-1 twin of "define shadows sealed primitive" (P1, above): the
		// registry's expand-phase copies moved to the sealed tier, so a
		// define-for-syntax lands at (1, mutable) beside them rather than
		// through them. Before that move this row read ErrNotAProcedure, and
		// that supersede is what made the Stable stamp registerPhasePrimitive
		// puts on those copies an assertion over an OPEN writer set.
		{name: "define-for-syntax shadows expand copy, pre-resolved reference intact",
			units: []string{
				`(define-for-syntax use-car (lambda (l) (car l)))`,
				`(define-for-syntax car 42)`,
				`(define-syntax reveal (er-macro-transformer (lambda (form rename compare) (use-car (list 1 2))))) (reveal)`,
			},
			want: "1"},
		// R2: a library body keeps cross-form define/set! mutable.
		{name: "library body set! own define",
			units: []string{`(import (lib-r2)) (f)`},
			want:  "2"},
		{name: "library body define over import",
			units: []string{`(import (lib-shadow)) (g)`},
			want:  "3"},
		// P7: a child namespace shadows freely; its imports refuse set!.
		{name: "child namespace define over primitive name",
			units: []string{`(define e (environment '(scheme base)))
				(eval '(define car 1) e) (eval 'car e)`},
			want: "1"},
		// Split into two units (rather than one EvalMultiple call holding both
		// forms) so runMatrix's per-unit failedAt can pin the failure to the
		// eval specifically, not to whichever of the two forms in a combined
		// entry happened to run first.
		{name: "child namespace set! imported refused",
			units: []string{
				`(define e (environment '(scheme base)))`,
				`(eval '(set! map 9) e)`,
			},
			wantErr: werr.ErrImmutableBinding},
		// P3 in a copy: report-env anchors stay set!-protected, define-shadow legal.
		{name: "report env set! anchor refused",
			units:   []string{`(eval '(set! car 5) (scheme-report-environment 7))`},
			wantErr: werr.ErrImmutableBinding},
		{name: "report env define shadows anchor",
			units: []string{`(define r (scheme-report-environment 7))
				(eval '(define car 5) r) (eval 'car r)`},
			want: "5"},
		// Whole-branch review round 1, B3: the report env's snapshot spans EVERY
		// phase and rank (namespace.go's Store().Copy()), not just the phase-0
		// tiers a pre-fold seal frame held — so derived syntax (cond, defined via
		// the (1, sealed) bootstrap macros) is reachable through
		// scheme-report-environment, which the pre-fold fresh-phase-1-seal
		// contract refused. Accepted as intended: see NewSchemeReportNamespace.
		{name: "report env reaches derived syntax (cond)",
			units: []string{`(eval '(cond (#t 7)) (scheme-report-environment 7))`},
			want:  "7"},
		// Control for the row above: null-environment's store starts EMPTY (no
		// Copy()), so cond stays unbound there. Without this row, "cond resolves"
		// could mean either "the copy carried derived syntax" or "cond resolves
		// ambiently everywhere" — this discriminates the two.
		{name: "null-environment refuses derived syntax (control)",
			units:   []string{`(eval '(cond (#t 7)) (null-environment 5))`},
			wantErr: werr.ErrNoSuchBinding},
		// The stale-pin self-heal, on the SHIPPED DEFAULT. Its twin in the
		// mutable matrix reads as a WithMutableTopLevel-only concern, and that
		// is what every earlier review pass took it for; it is not.
		//
		// The shadow and the set! must share ONE unit. Split across units the
		// immutable default refuses the set! outright — a top-level define
		// stamps its binding Stable, so a LATER unit's set! is a mutation of a
		// Stable binding. Within one unit the body defines are predeclared and
		// the set! compiles against a binding not yet stamped, so it pins the
		// (0, mutable) shadow and the sequence proceeds exactly as it does with
		// the flag on. A census of a bootstrapped KitchenSink namespace puts
		// 146 names at (1, sealed) with a phase-0 twin — this is not a car
		// special case.
		{name: "stale pin self-heal refused under the immutable default",
			units: []string{
				`(begin (define car 1) (define (f) (set! car 2)))`,
				`(namespace-undefine! (interaction-environment) 'car)`,
				`(f)`,
			},
			wantErr: machine.ErrBindingNotFound},
	}
	runMatrix(t, rows)
}

// WithMutableTopLevel: the variant where nothing is Stable, which is where the
// pinned-store reach and the define asymmetry become visible (M6/M7).
func TestBindingModelMatrixMutableTopLevel(t *testing.T) {
	rows := []matrixRow{
		// M6: a compile-resolved set! pin mutates the SEALED binding. This row
		// only asserts the resulting VALUE — an in-place mutation and a
		// mutable-layer write both produce 99 here, so it does not by itself
		// distinguish the two. The h/car pair below (observed through a call
		// site compiled BEFORE the mutation) is what pins "in place"
		// specifically.
		{name: "set! sealed primitive mutates in place",
			units: []string{`(set! car 99) car`},
			want:  "99"},
		// The pair below is the actual "in place" pin for the row above and for
		// the file header's define-vs-set! asymmetry claim: h is compiled once,
		// before either mutation, so its reference to car resolves (at compile
		// time) to whichever binding exists then — the sealed primitive. set!
		// afterward reaches that SAME binding, so h calls the new,
		// non-callable value and fails; define afterward creates a fresh
		// shadow that h's already-resolved reference never sees, so h still
		// calls the original primitive. Measured against matrixEngine's
		// config with WithMutableTopLevel.
		{name: "set! observed through an existing call site",
			units: []string{
				`(define (h p) (car p))`,
				`(set! car 99)`,
				`(h '(1 2))`,
			},
			wantErr: werr.ErrNotAProcedure},
		{name: "define NOT observed through an existing call site",
			units: []string{
				`(define (h p) (car p))`,
				`(define car 99)`,
				`(h '(1 2))`,
			},
			want: "1"},
		// M7: ...and the (1, sealed) expand copy of car is a different binding
		// — unaffected by the phase-0 set! above. A bare 'ok trailing form
		// can't tell a no-op begin-for-syntax from one that actually ran:
		// reveal-probe reads probe back through a phase-1
		// er-macro-transformer, so if begin-for-syntax never ran its body,
		// probe is unbound and this row fails to compile instead of passing.
		// Measured against matrixEngine's config with WithMutableTopLevel.
		{name: "expand copy survives phase-0 set!",
			units: []string{
				`(set! car 99) 'ok`,
				`(begin-for-syntax (define probe (car (list 1 2)))) (define-syntax reveal-probe (er-macro-transformer (lambda (form rename compare) probe))) (reveal-probe)`,
			},
			want: "1"},
		// define still never lands on the sealed slot, even fully mutable —
		// captured VALUE only; see the h/car pair above for the slot-level
		// claim.
		{name: "define still shadows sealed primitive",
			units: []string{`(define f cdr) (define cdr 7) (f '(1 2))`},
			want:  "(2)"},
		{name: "redefine own define ok",
			units: []string{`(define x 1)`, `(define x 2) x`},
			want:  "2"},
		{name: "set! own define later unit ok",
			units: []string{`(define z 1)`, `(set! z 2) z`},
			want:  "2"},
		// CRITICAL fix, fold C3 review round 1: SetOwnGlobalValue's stale-pin
		// self-heal must not re-heal onto a SEALED slot. square (a bootstrap
		// SCHEME procedure, sealed at (ANY, sealed) with no phase-1 companion
		// registration) is shadowed at (0, mutable); f's set! compiles to a
		// PINNED index at that mutable slot (the same pinned-reach mechanism
		// the h/car pair above exercises); namespace-undefine! deletes the
		// mutable shadow; calling f re-triggers the now-stale pin. Before the
		// fix the self-heal fallback was coordinate-blind and fell through to
		// the SEALED square, silently mutating the primitive in place; after
		// it, the heal asks for (0, mutable) and finds nothing, so the write
		// is refused — machine.ErrBindingNotFound, the same sentinel a
		// name-resolved write through the pre-fold frame-local store reported.
		// The next row is the phase twin: same shape for a name that DOES own
		// a phase-1 companion.
		{name: "stale pin self-heal refuses sealed slot after undefine",
			units: []string{
				`(define square 1)`,
				`(define (f) (set! square 2))`,
				`(namespace-undefine! (interaction-environment) 'square)`,
				`(f)`,
			},
			wantErr: machine.ErrBindingNotFound},
		// The PHASE axis of the same hazard, for a name that owns a phase-1
		// companion — car here, and with it the ~28 dual-phase registration
		// blocks in pkg/registry/core (pairs, lists, strings, arithmetic,
		// vectors, predicates, characters, equality, hashes, syntax). car's
		// slots here are [(ANY, sealed), (1, sealed) expand copy, (0, mutable)
		// shadow]; namespace-undefine! removes only the shadow. A self-heal
		// that filtered on sealed/mutable ALONE did not eliminate this hazard,
		// it relocated it: the expand copy sat at (1, MUTABLE) then, so it was
		// the only remaining candidate, the write succeeded silently, and it
		// landed on the startup set's phase-1 primitive — this row measured "2"
		// for as long as the filter was phase-blind. Sealing the expand copies
		// later shut that door from the other side: with no (1, mutable)
		// candidate left, even a phase-blind filter would refuse, so this row
		// alone no longer discriminates the two filters. What it still pins is
		// the behaviour — the heal asks for (0, mutable), finds nothing, and
		// refuses exactly as the square row above does, which is the point: a
		// dual-phase name and a single-phase one behave the same.
		{name: "stale pin self-heal refuses phase-1 slot after undefine",
			units: []string{
				`(define car 1)`,
				`(define (f) (set! car 2))`,
				`(namespace-undefine! (interaction-environment) 'car)`,
				`(f)`,
			},
			wantErr: machine.ErrBindingNotFound},
		// ...and the phase-1 copy is untouched by the refused write. Read back
		// through a phase-1 er-macro-transformer, since a top-level read
		// cannot observe phase 1: car there is still the primitive, not 2.
		{name: "refused self-heal leaves the phase-1 copy intact",
			units: []string{
				`(define car 1)`,
				`(define (f) (set! car 2))`,
				`(namespace-undefine! (interaction-environment) 'car)`,
				`(begin-for-syntax (define probe (car (list 7 8)))) (define-syntax reveal-probe (er-macro-transformer (lambda (form rename compare) probe))) (reveal-probe)`,
			},
			want: "7"},
	}
	runMatrix(t, rows, wile.WithMutableTopLevel())
}
