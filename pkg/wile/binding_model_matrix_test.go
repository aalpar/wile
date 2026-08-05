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
// Two deliberate asymmetries this table owns (measured, plan §0 M6/M7):
//   - define NEVER lands on a sealed slot (it targets the mutable layer),
//     while a compile-RESOLVED set! pin CAN reach one when nothing refuses it
//     (mutable-top-level variant) — G13's own-frame restriction is about the
//     deferred write, and define is its observable face. The "define shadows
//     sealed primitive" rows assert a captured VALUE, which is insensitive to
//     what later happens to the binding, so they do not carry this claim; the
//     h/car pair in TestBindingModelMatrixMutableTopLevel ("set! observed
//     through an existing call site" / "define NOT observed through an
//     existing call site") does, by observing through a procedure that
//     re-resolves car at call time instead of a value captured once.
//   - car exists twice: the sealed phase-0 binding and the registry's
//     (1, mutable) expand copy. Phase-0 mutation must not touch the copy —
//     pinned by "define-for-syntax supersedes expand copy, phase 1 sees new
//     value" alongside "...phase 0 intact".
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
		// P4: define supersedes an import IN PLACE and drops the import
		// provenance, so a same-unit set! is then permitted (R7RS §5.3.1).
		// Same-unit requires the begin-wrap for the reason given above
		// ([Engine.EvalProgram] does the equivalent begin-splice structurally
		// for a whole program; this row open-codes it for one row): bare
		// "(define g 9) (set! g 10)" are two separate compile units, and the
		// define unit alone stamps Stable before the set! unit ever runs. Uses
		// g (lib-shadow's export, see "set! imported refused" below) rather
		// than map: map is ALSO a Stable stdlib anchor in this profile with NO
		// import at all (see the control row below "set! imported refused"),
		// so asserting "10" against map is produced by
		// define-over-a-sealed-primitive-plus-same-unit-set! (already covered
		// by "set! own define same unit ok" above) and pins nothing about
		// import supersession specifically. Measured against matrixEngine's
		// config.
		{name: "define supersedes import then set!",
			units: []string{`(import (lib-shadow))`, `(begin (define g 9) (set! g 10) g)`},
			want:  "10"},
		// P2: set! on an imported binding refused (R7RS §5.2). Uses g
		// (lib-shadow's export) rather than map: map is a Stable stdlib anchor
		// in KitchenSink even with no import at all (see the control row
		// immediately below), so "(set! map 1)" fails with ErrImmutableBinding
		// either way and does not discriminate import provenance. Measured
		// against matrixEngine's config.
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
		// SUPERSEDES it (scope-set-equal slot reuse) — and phase 0's car is a
		// different binding, untouched.
		{name: "define-for-syntax supersedes expand copy, phase 0 intact",
			units: []string{`(define-for-syntax car 42) (car '(9 8))`},
			want:  "9"},
		// M7 continued: the same define-for-syntax also supersedes the
		// (1, mutable) expand copy itself — nothing above observes that value,
		// only that phase 0 is untouched. reveal-car reads it back through a
		// phase-1 er-macro-transformer. Measured against matrixEngine's
		// config.
		{name: "define-for-syntax supersedes expand copy, phase 1 sees new value",
			units: []string{
				`(define-for-syntax car 42)`,
				`(define-syntax reveal-car (er-macro-transformer (lambda (form rename compare) car))) (reveal-car)`,
			},
			want: "42"},
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
		// M7: ...and the (1, mutable) expand copy of car is a different binding
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
	}
	runMatrix(t, rows, wile.WithMutableTopLevel())
}
