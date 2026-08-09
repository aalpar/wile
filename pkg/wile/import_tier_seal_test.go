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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"
)

// This file guards the ONE coordinate the import tier relocation must never
// reach, and it is the test that makes the refusal in
// compilation.installImportedBinding's two placementInPlace call sites checkable
// rather than merely commented.
//
// Imports install at T2, (ExactPhase(0), sealed), so a user top-level define
// SHADOWS an import instead of assigning through it. That is safe only because
// (ExactPhase(0), sealed) is an EMPTY coordinate: EnvironmentFrame's
// writeCoordinates maps a sealed write at phase 0 to AnyPhase(), so no view can
// produce it and nothing was ever there.
//
// (ExactPhase(1), sealed) is NOT empty. Bootstrap macros and primitive expanders
// live there. If the PROPAGATED install (the phase-1 half of a macro import) or
// the library-internal expand install took the same tier, an imported macro
// would land on exactly a bootstrap macro's coordinates under the same ambient
// scope set — so CreateGlobalBindingAt REUSES the slot, `created` is false,
// importConflicts returns false (a bootstrap macro is not IsImported()),
// SetOwnGlobalValue overwrites the sealed ambient transformer IN PLACE and
// ENGINE-WIDE, and markBindingImported then stamps the startup set as imported.
//
// The failure is invisible from Scheme: the import "works", and every program
// that used the bootstrap macro silently gets the imported one. Nothing else in
// the suite would name it, which is why the assertion is on the STORE and not on
// an evaluated value.
//
// To see it fail: change either placementInPlace to placementShadowable in
// pkg/machine/compilation/library_bindings.go. Verified — the imported=false
// assertion below goes red.

const whenShadowLibrary = `(define-library (lib-when-shadow)
  (export when)
  (import (scheme base))
  (begin
    (define-syntax when
      (syntax-rules ()
        ((_ test body ...) (if test (begin 'IMPORTED-WHEN body ...) 'IMPORTED-WHEN-SKIP))))))
`

func sealTierEngine(t *testing.T) *wile.Engine {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(fstest.MapFS{
			"lib-when-shadow.scm": &fstest.MapFile{Data: []byte(whenShadowLibrary)},
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

// TestImportDoesNotOverwriteSealedBootstrapMacro is the hazard gate. It imports a
// library exporting a macro that collides with a bootstrap macro name, and
// asserts the SEALED phase-1 transformer is the same binding holding the same
// value afterwards, still un-imported.
//
// The value identity is the load-bearing half: the whole failure mode is an
// in-place overwrite of a slot that keeps its identity, so asserting the binding
// pointer alone would pass through the bug.
func TestImportDoesNotOverwriteSealedBootstrapMacro(t *testing.T) {
	eng := sealTierEngine(t)
	store := eng.Environment().Namespace().Store()
	sym := values.NewSymbol("when")

	before := store.SealedBindingAt(sym, values.EmptyScopes(), environment.PhaseExpand)
	qt.Assert(t, before, qt.IsNotNil,
		qt.Commentf("`when` must be a sealed phase-1 bootstrap macro for this gate to mean anything; "+
			"if this is nil the hazard has moved and the gate is blind"))
	qt.Assert(t, before.IsImported(), qt.IsFalse)
	beforeValue := before.Value()
	qt.Assert(t, beforeValue, qt.IsNotNil)

	_, err := eng.EvalMultiple(context.Background(), `(import (lib-when-shadow))`)
	qt.Assert(t, err, qt.IsNil,
		qt.Commentf("the import itself must succeed — the hazard is that it succeeds too well"))

	after := store.SealedBindingAt(sym, values.EmptyScopes(), environment.PhaseExpand)
	qt.Assert(t, after, qt.IsNotNil)
	qt.Assert(t, after.IsImported(), qt.IsFalse,
		qt.Commentf("the sealed bootstrap `when` was stamped imported: the phase-1 install "+
			"reused the startup set's slot instead of taking its own"))
	qt.Assert(t, after.Value() == beforeValue, qt.IsTrue,
		qt.Commentf("the sealed bootstrap `when` transformer was overwritten IN PLACE, engine-wide"))
}

// TestImportedBindingTakesTheSealedPhaseZeroTier pins the positive half: the
// BASE install really does land on (ExactPhase(0), sealed), and a top-level
// define really does get its own T1 slot above it. Without this, the refusal
// gate above would still pass on a build where the relocation had been reverted
// wholesale.
func TestImportedBindingTakesTheSealedPhaseZeroTier(t *testing.T) {
	ctx := context.Background()
	eng := sealTierEngine(t)
	store := eng.Environment().Namespace().Store()
	sym := values.NewSymbol("list-copy")

	// Before any import, list-copy resolves to the ambient startup set (T3).
	// SealedBindingAt cannot distinguish T2 from T3, so the discriminator is the
	// import provenance, which only an import carries.
	pre := store.SealedBindingAt(sym, values.EmptyScopes(), environment.PhaseRuntime)
	qt.Assert(t, pre, qt.IsNotNil)
	qt.Assert(t, pre.IsImported(), qt.IsFalse)

	_, err := eng.EvalMultiple(ctx, `(import (scheme base))`)
	qt.Assert(t, err, qt.IsNil)

	// The import now OUTRANKS the ambient primitive, so the sealed probe answers
	// with it. That is T2 beating T3, which is only possible at (ExactPhase(0),
	// sealed) — the coordinate the relocation targets.
	post := store.SealedBindingAt(sym, values.EmptyScopes(), environment.PhaseRuntime)
	qt.Assert(t, post, qt.IsNotNil)
	qt.Assert(t, post.IsImported(), qt.IsTrue,
		qt.Commentf("the import did not reach the sealed tier at phase 0; the base install "+
			"is back on T1 and a later define would assign through it"))

	// ...and a top-level define outranks BOTH, from T1.
	v, err := eng.EvalMultiple(ctx, `(define list-copy 7) list-copy`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.Internal().SchemeString(), qt.Equals, "7")

	// The import is shadowed, not replaced: its slot is still there, still
	// imported. This is the assertion that separates "define shadows" from
	// "define superseded in place", which read identically from Scheme.
	still := store.SealedBindingAt(sym, values.EmptyScopes(), environment.PhaseRuntime)
	qt.Assert(t, still, qt.IsNotNil)
	qt.Assert(t, still.IsImported(), qt.IsTrue,
		qt.Commentf("the define reached the import's slot — that is the assignment this "+
			"relocation exists to prevent"))
}
