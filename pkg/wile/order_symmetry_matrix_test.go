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
	"testing"
	"testing/fstest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// A definition and an import of one name must agree whichever order they are
// written in, at EVERY phase. I127.
//
// The import install chose its coordinates from the phase it happened to be at:
// placementShadowable routed through CreateImportedGlobalBindingAt only when
// env.PhaseLevel() == PhaseRuntime, so a phase-1 install fell back to the view
// and landed on the user's own slot instead of minting one of the import's.
// Phase 0 was already right, which is what makes this a measurement of the
// phase axis and not a claim about imports in general.
//
// ORACLE: racket 9.2 answers 99 in both orders at both phases (a module-level
// definition shadows a require), measured with (require (for-syntax racket/base))
// and a sibling module providing the same name.
//
// THE PHASE-1 VALUE IS READ THROUGH A LAMBDA TRANSFORMER, deliberately. A
// `syntax-rules` template that names a phase-1 binding reads it from PHASE-0
// code, which Wile allows and Racket refuses — a separate leak, filed in
// TODO.md as its own row. Measuring this matrix through that channel would gate
// on the leak rather than on the install phase. A (lambda (stx) ...) body IS
// phase-1 code, so `vname` there is an ordinary phase-1 reference and
// datum->syntax carries the value out.
const orderSymmetryLib = `(define-library (vlib)
  (import (scheme base))
  (export vname)
  (begin
    (define vname 'from-library)))
`

func orderSymmetryEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(fstest.MapFS{
			"vlib.scm": &fstest.MapFile{Data: []byte(orderSymmetryLib)},
		}),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

func TestOrderSymmetryMatrix(t *testing.T) {
	for _, row := range []struct {
		name string
		src  string
	}{
		{
			// The non-vacuity guard. Stage A already fixed phase 0, so this row
			// passes on master; a matrix that only covered the broken phase
			// would not show that the fix generalizes an answer rather than
			// inventing one.
			name: "var/phase0/define-then-import",
			src: `(import (scheme base))
(define vname 99)
(import (vlib))
vname`,
		},
		{
			name: "var/phase0/import-then-define",
			src: `(import (scheme base) (vlib))
(define vname 99)
vname`,
		},
		{
			// RED on master: the import overwrote the user's phase-1 slot, so
			// `vname` became 'from-library and datum->syntax emitted that
			// symbol as a reference, failing with
			// `no such binding "from-library" with compatible scopes`.
			name: "var/phase1/define-then-import",
			src: `(import (scheme base))
(define-for-syntax vname 99)
(import (for-syntax (vlib)))
(define-syntax use (lambda (stx) (datum->syntax stx vname)))
(use)`,
		},
		{
			name: "var/phase1/import-then-define",
			src: `(import (scheme base) (for-syntax (vlib)))
(define-for-syntax vname 99)
(define-syntax use (lambda (stx) (datum->syntax stx vname)))
(use)`,
		},
		{
			name: "var/phase0/double-import-slot-share",
			src: `(import (scheme base) (vlib))
(define vname 99)
(import (vlib))
vname`,
		},
		{
			// RED on master, and the second red row the plan names: a second
			// import after a define reached the same wrong coordinate.
			name: "var/phase1/double-import-slot-share",
			src: `(import (scheme base) (for-syntax (vlib)))
(define-for-syntax vname 99)
(import (for-syntax (vlib)))
(define-syntax use (lambda (stx) (datum->syntax stx vname)))
(use)`,
		},
	} {
		t.Run(row.name, func(t *testing.T) {
			eng := orderSymmetryEngine(t)
			v, err := eng.EvalMultiple(context.Background(), row.src)
			qt.Assert(t, err, qt.IsNil,
				qt.Commentf("the user's own definition must win whatever the order"))
			qt.Assert(t, v.SchemeString(), qt.Equals, "99")
		})
	}
}

// The soundness argument the one-conjunct deletion rests on, as a test rather
// than as a paragraph: relocating a phase-1 import cannot reach a bootstrap
// macro's slot, because createGlobalBindingAt's
// `sealed && IsImported() != imported` refusal carries NO phase restriction. A
// bootstrap macro is not IsImported(), so the refusal fires and the import
// mints a slot of its own at tierExactImported.
//
// This re-states what TestImportDoesNotOverwriteSealedBootstrapMacro asserts on
// the store, from the Scheme side, so a reader of this matrix does not have to
// take the relocation's safety on trust.
func TestOrderSymmetryDoesNotDisturbBootstrapMacros(t *testing.T) {
	eng := orderSymmetryEngine(t)
	v, err := eng.EvalMultiple(context.Background(),
		`(import (scheme base) (for-syntax (vlib)))
(when #t 'bootstrap-macro-still-works)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "bootstrap-macro-still-works")
}
