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

// declPhaseLibs is one helper library plus the four importing shapes. Each
// importer names its shift in its own file so a row's failure names the shift.
var declPhaseLibs = fstest.MapFS{
	"helper.scm": &fstest.MapFile{Data: []byte(`(define-library (helper)
  (export my-helper)
  (import (scheme base))
  (begin (define (my-helper n) (+ n 100))))
`)},
	// DECLARATION position, +1. my-helper must NOT be at phase 0.
	"decl-fs.scm": &fstest.MapFile{Data: []byte(`(define-library (decl-fs)
  (export probe)
  (import (scheme base))
  (import (for-syntax (helper)))
  (begin (define (probe) (my-helper 1))))
`)},
	// DECLARATION position, +1, consumed where it belongs: a transformer body.
	// This is the row that proves the shift LANDS somewhere rather than merely
	// that phase 0 was emptied.
	"decl-fs-used.scm": &fstest.MapFile{Data: []byte(`(define-library (decl-fs-used)
  (export probe)
  (import (scheme base))
  (import (for-syntax (helper)))
  (begin
    (define-syntax m
      (er-macro-transformer (lambda (f r c) (my-helper 5))))
    (define (probe) (m))))
`)},
	// DECLARATION position, -1. A dropped shift is indistinguishable from no
	// modifier, so this row is what separates "shift applied" from "shift read".
	"decl-ft.scm": &fstest.MapFile{Data: []byte(`(define-library (decl-ft)
  (export probe)
  (import (scheme base))
  (import (for-template (helper)))
  (begin (define (probe) (my-helper 6))))
`)},
	// DECLARATION position, +3.
	"decl-fm3.scm": &fstest.MapFile{Data: []byte(`(define-library (decl-fm3)
  (export probe)
  (import (scheme base))
  (import (for-meta 3 (helper)))
  (begin (define (probe) (my-helper 7))))
`)},
	// GUARD, passes on master: an unshifted declaration import still binds at 0.
	"decl-plain.scm": &fstest.MapFile{Data: []byte(`(define-library (decl-plain)
  (export probe)
  (import (scheme base))
  (import (helper))
  (begin (define (probe) (my-helper 8))))
`)},
	// GUARD, passes on master: the BODY position already composed the shift, and
	// must keep doing so — pkg/stdlib/lib/wile/er-macro-test.scm depends on it.
	// These two are the declaration pair's twins, and after the fix the two
	// positions must give the SAME two answers.
	"body-fs.scm": &fstest.MapFile{Data: []byte(`(define-library (body-fs)
  (export probe)
  (import (scheme base))
  (begin
    (import (for-syntax (helper)))
    (define (probe) (my-helper 9))))
`)},
	"body-fs-used.scm": &fstest.MapFile{Data: []byte(`(define-library (body-fs-used)
  (export probe)
  (import (scheme base))
  (begin
    (import (for-syntax (helper)))
    (define-syntax m
      (er-macro-transformer (lambda (f r c) (my-helper 10))))
    (define (probe) (m))))
`)},
	// A declaration-position shift near the int8 ceiling must be REFUSED, not
	// wrapped negative into some other phase's registry.
	"decl-overflow.scm": &fstest.MapFile{Data: []byte(`(define-library (decl-overflow)
  (export probe)
  (import (scheme base))
  (import (for-meta 127 (helper)))
  (begin (define (probe) 1)))
`)},
}

func declPhaseEngine(t *testing.T) *wile.Engine {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(declPhaseLibs),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

// TestLibraryDeclarationImportComposesPhaseShift pins the defect filed
// 2026-09-09 (polarity corrected the same day): inside a define-library
// DECLARATION, (import (for-syntax X)) / (for-template X) / (for-meta n X) were
// parsed, accepted, and then installed at phase 0 as if the modifier were absent.
//
// processLibraryImport ended at copyLibraryBindingsDirect, which took no phase
// argument and never read ImportSet.PhaseShift — the shift was computed by the
// shared parser and discarded. The identical import at TOP LEVEL, and the
// identical import inside the library BODY, both went through
// ResolveAndInstallImportSet, which composes it.
//
// The failure was silent both ways: a body that wanted the shift got a phase-0
// binding it did not ask for, and a body that relied on the shift for hygiene got
// nothing, with no diagnostic naming either.
//
// Rows 1-4 are the RATCHET (red on master). Rows 5-7 are GUARDS that pass on
// master and are kept because they say what must NOT move: the shift-0 no-op,
// and the body position the stdlib's er-macro-test.scm depends on.
func TestLibraryDeclarationImportComposesPhaseShift(t *testing.T) {
	tests := []struct {
		name    string
		src     string
		want    string
		wantErr error
	}{
		{
			name:    "ratchet: for-syntax in the declaration leaves phase 0 unbound",
			src:     `(import (decl-fs)) (probe)`,
			wantErr: werr.ErrNoSuchBinding,
		},
		{
			name: "ratchet: for-syntax in the declaration binds at phase 1",
			src:  `(import (decl-fs-used)) (probe)`,
			want: "105",
		},
		{
			name:    "ratchet: for-template in the declaration leaves phase 0 unbound",
			src:     `(import (decl-ft)) (probe)`,
			wantErr: werr.ErrNoSuchBinding,
		},
		{
			name:    "ratchet: for-meta 3 in the declaration leaves phase 0 unbound",
			src:     `(import (decl-fm3)) (probe)`,
			wantErr: werr.ErrNoSuchBinding,
		},
		{
			name: "guard (green on master): an unshifted declaration import binds at phase 0",
			src:  `(import (decl-plain)) (probe)`,
			want: "108",
		},
		{
			// The declaration twin of this row is ratchet 1, and after the fix the
			// two positions agree. That agreement IS the fix: before it, this row
			// raised and its declaration twin returned 101.
			name:    "guard (green on master): the body position leaves phase 0 unbound",
			src:     `(import (body-fs)) (probe)`,
			wantErr: werr.ErrNoSuchBinding,
		},
		{
			name: "guard (green on master): the body position binds at phase 1",
			src:  `(import (body-fs-used)) (probe)`,
			want: "110",
		},
		{
			name:    "guard: a declaration shift past the int8 ceiling is refused",
			src:     `(import (decl-overflow)) (probe)`,
			wantErr: werr.ErrInvalidArgument,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			eng := declPhaseEngine(t)
			v, err := eng.EvalMultiple(context.Background(), tc.src)
			if tc.wantErr != nil {
				c.Assert(err, qt.IsNotNil,
					qt.Commentf("expected %v; the import silently landed at phase 0 instead", tc.wantErr))
				c.Assert(errors.Is(err, tc.wantErr), qt.IsTrue,
					qt.Commentf("got %v", err))
				return
			}
			c.Assert(err, qt.IsNil)
			c.Assert(v.SchemeString(), qt.Equals, tc.want)
		})
	}
}
