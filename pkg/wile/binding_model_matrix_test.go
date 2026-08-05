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
//   - define NEVER lands on a sealed slot (it targets the mutable layer; the
//     capture rows prove the original binding survives), while a
//     compile-RESOLVED set! pin CAN reach one when nothing refuses it
//     (mutable-top-level variant) — G13's own-frame restriction is about the
//     deferred write, and define is its observable face.
//   - car exists twice: the sealed phase-0 binding and the registry's
//     (1, mutable) expand copy. Phase-0 mutation must not touch the copy.
//
// See plans/2026-08-05-flat-binding-model-{design,impl}.local.md.

type matrixRow struct {
	name  string
	units []string // each unit is one EvalMultiple call, in order
	// wantErr non-nil: the LAST unit must fail errors.Is(err, wantErr).
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
			for _, unit := range row.units {
				v, err := eng.EvalMultiple(ctx, unit)
				lastErr = err
				if err != nil {
					break
				}
				lastVal = v.Internal().SchemeString()
			}
			if row.wantErr != nil {
				qt.Assert(t, lastErr, qt.IsNotNil)
				qt.Assert(t, errors.Is(lastErr, row.wantErr), qt.IsTrue,
					qt.Commentf("want %v, got: %v", row.wantErr, lastErr))
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
		// untouched (the captured f still holds the primitive).
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
		// time the set! unit compiles. begin splices its body into one form,
		// so this is the actual same-unit case. Measured: without the
		// begin-wrap this row fails with ErrImmutableBinding (probed via a
		// scratch program against matrixEngine's config).
		{name: "set! own define same unit ok",
			units: []string{`(begin (define y 1) (set! y 2) y)`},
			want:  "2"},
		// ...and a LATER-unit set! of a defined-once binding is refused.
		{name: "set! own define later unit refused",
			units:   []string{`(define z 1)`, `(set! z 2)`},
			wantErr: werr.ErrImmutableBinding},
		// P4: define supersedes an import IN PLACE and drops the import
		// provenance, so a same-unit set! is then permitted (R7RS §5.3.1).
		// Same-unit requires the begin-wrap for the reason given above: bare
		// "(define map 9) (set! map 10)" are two separate compile units, and
		// the define unit alone stamps Stable (defined-once, unmutated WITHIN
		// THAT UNIT) before the set! unit ever runs, so the set! is refused.
		// Measured against matrixEngine's config with a scratch program.
		{name: "define supersedes import then set!",
			units: []string{`(import (scheme base))`, `(begin (define map 9) (set! map 10) map)`},
			want:  "10"},
		// P2: set! on an imported binding refused (R7RS §5.2).
		{name: "set! imported refused",
			units:   []string{`(import (scheme base))`, `(set! map 1)`},
			wantErr: werr.ErrImmutableBinding},
		// A bootstrap macro NAME is free at phase 0: define binds the value,
		// the phase-1 transformer keeps expanding.
		{name: "define over bootstrap macro name",
			units: []string{`(define when 5) (list when (when #t 1))`},
			want:  "(5 1)"},
		// M8: delete own define works; delete sealed refused.
		{name: "undefine own define",
			units: []string{`(define w 1) (namespace-undefine! (interaction-environment) 'w) 'ok`,
				`w`},
			wantErr: werr.ErrNoSuchBinding},
		{name: "undefine sealed primitive refused",
			units:   []string{`(namespace-undefine! (interaction-environment) 'car)`},
			wantErr: werr.ErrImmutableBinding},
		// Phase-1 define + set! at phase 1; hermeticity from phase 0 (P5).
		{name: "phase-1 define set! at own phase",
			units: []string{`(define-for-syntax fv 1) (begin-for-syntax (set! fv 2)) 'ok`},
			want:  "ok"},
		{name: "phase-0 cannot read phase-1 define",
			units:   []string{`(define-for-syntax fv 1)`, `fv`},
			wantErr: werr.ErrNoSuchBinding},
		// M7: define-for-syntax over the expand-phase registry copy of car
		// SUPERSEDES it (scope-set-equal slot reuse) — and phase 0's car is a
		// different binding, untouched.
		{name: "define-for-syntax supersedes expand copy, phase 0 intact",
			units: []string{`(define-for-syntax car 42) (car '(9 8))`},
			want:  "9"},
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
		{name: "child namespace set! imported refused",
			units: []string{`(define e (environment '(scheme base)))
				(eval '(set! map 9) e)`},
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
		// M6: a compile-resolved set! pin mutates the SEALED binding in place.
		// This is today's behavior and the fold must preserve it bit-for-bit.
		{name: "set! sealed primitive mutates in place",
			units: []string{`(set! car 99) car`},
			want:  "99"},
		// M7: ...and the (1, mutable) expand copy of car is a different binding.
		{name: "expand copy survives phase-0 set!",
			units: []string{`(set! car 99) 'ok`,
				`(begin-for-syntax (define probe (car (list 1 2)))) 'ok`},
			want: "ok"},
		// define still never lands on the sealed slot, even fully mutable.
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
