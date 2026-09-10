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

	// Before any import, list-copy is the startup set's own binding: sealed at
	// phase 0, and not the imported tier.
	base := store.SealedBindingAt(sym, values.EmptyScopes(), environment.PhaseRuntime)
	qt.Assert(t, base, qt.IsNotNil)
	qt.Assert(t, base.IsImported(), qt.IsFalse)
	qt.Assert(t, store.IsImportedBindingAt(sym, values.EmptyScopes(), environment.PhaseRuntime),
		qt.IsFalse)
	baseValue := base.Value()

	_, err := eng.EvalMultiple(ctx, `(import (scheme base))`)
	qt.Assert(t, err, qt.IsNil)

	// The import outranks the startup set, which is only possible because it took
	// a sealed-coordinate slot of its OWN: tierExactImported sits between the
	// user's mutable tier and the startup set's.
	qt.Assert(t, store.IsImportedBindingAt(sym, values.EmptyScopes(), environment.PhaseRuntime),
		qt.IsTrue,
		qt.Commentf("the import did not reach the imported tier at phase 0; the base install "+
			"is back on T1 and a later define would assign through it"))

	// ...and the startup set is still there UNDERNEATH, untouched. This pair is
	// what the test asserts now and could not before.
	//
	// It used to read the sealed probe and assert the answer IsImported(), i.e.
	// two bits standing in for one coordinate. That could not tell "the import
	// took a slot above the base" from "the import landed ON the base and stamped
	// it", and the second is what was actually happening: one slot, same pointer,
	// the base's value replaced by the library env's copy. The test passed
	// THROUGH the defect. See TestImportDoesNotMutateTheBaseBinding.
	stillBase := store.SealedBindingAt(sym, values.EmptyScopes(), environment.PhaseRuntime)
	qt.Assert(t, stillBase, qt.IsNotNil)
	qt.Assert(t, stillBase.IsImported(), qt.IsFalse,
		qt.Commentf("the import stamped the startup set's own binding instead of taking its own slot"))
	qt.Assert(t, stillBase.Value() == baseValue, qt.IsTrue,
		qt.Commentf("the import overwrote the startup set's value in place"))

	// ...and a top-level define outranks BOTH, from the mutable tier.
	v, err := eng.EvalMultiple(ctx, `(define list-copy 7) list-copy`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.Internal().SchemeString(), qt.Equals, "7")

	// The import is shadowed, not replaced: its slot is still there, still the
	// imported tier. This is the assertion that separates "define shadows" from
	// "define superseded in place", which read identically from Scheme.
	//
	// The FLOORED probe, not the ranked one: after the define, what the name
	// DENOTES is the define, so IsImportedBindingAt is false here and would be
	// false for both outcomes. ImportedBindingAt skips the mutable tier and asks
	// what is underneath.
	qt.Assert(t, store.ImportedBindingAt(sym, values.EmptyScopes(), environment.PhaseRuntime),
		qt.IsNotNil,
		qt.Commentf("the define reached the import's slot — that is the assignment this "+
			"relocation exists to prevent"))
}
