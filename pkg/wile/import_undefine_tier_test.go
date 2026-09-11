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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
)

// TestUndefineOfAnImportLeavesTheStartupSetIntact is the STORE-LEVEL pin for the
// half of the S0 defect that the create-path repair left behind.
//
// namespace-undefine! asks IsImportedBindingAt — a TIER question, and the right
// one — and then deletes with DeleteBindingAt at (ExactPhase(0), sealed), which
// is COORDINATE-addressed. resolveAtCoordsLocked filters only on
// (phase, sealed); the imported tier is invisible to it, so with the base and
// the import both at cardinality 0 the walk returns the FIRST slot in
// p.keys[sym] order — the startup set's, created at bootstrap. The import the
// caller asked to remove survives and the base dies in its place.
//
// No Scheme assertion can see this. namespace-bound? is still #t afterwards,
// because the surviving import answers it; that is exactly how
// TestImportDoesNotMakeABasePrimitiveDeletable passes THROUGH the bug. The pin
// therefore holds the two *Binding pointers across the operation and asks the
// store which tier still has a slot.
//
// The assertion is independent of the fix's shape: anything that deletes the
// import rather than the base passes, and nothing that keeps addressing the
// coordinate without the provenance axis can.
func TestUndefineOfAnImportLeavesTheStartupSetIntact(t *testing.T) {
	// `car` is a Go primitive and `list-copy` a bootstrap Scheme procedure; the
	// two take different install paths, and the defect reached both. A FRESH
	// engine per row: undefine mutates engine state.
	for _, name := range []string{"car", "list-copy"} {
		t.Run(name, func(t *testing.T) {
			c := qt.New(t)
			eng := baseSlotEngine(t)
			store := eng.Environment().Namespace().Store()
			sym := values.NewSymbol(name)

			base := store.SealedBindingAt(sym, values.EmptyScopes(), environment.PhaseRuntime)
			c.Assert(base, qt.IsNotNil,
				qt.Commentf("%q must be a startup-set binding for this gate to mean anything", name))
			c.Assert(store.ImportedBindingAt(sym, values.EmptyScopes(), environment.PhaseRuntime), qt.IsNil,
				qt.Commentf("nothing has imported %q yet", name))

			_, err := eng.EvalMultiple(context.Background(), `(import (scheme base))`)
			c.Assert(err, qt.IsNil)

			// Both tiers are now occupied, by two distinct slots. This is the state
			// the delete has to tell apart, and S0 is what created it.
			imported := store.ImportedBindingAt(sym, values.EmptyScopes(), environment.PhaseRuntime)
			c.Assert(imported, qt.IsNotNil,
				qt.Commentf("the import did not reach the imported tier; the precondition is gone"))
			c.Assert(store.SealedBindingAt(sym, values.EmptyScopes(), environment.PhaseRuntime), qt.Equals, base)

			_, err = eng.EvalMultiple(context.Background(),
				`(namespace-undefine! (interaction-environment) '`+name+`)`)
			c.Assert(err, qt.IsNil,
				qt.Commentf("removing an imported name is a documented capability (design P7)"))

			c.Assert(store.SealedBindingAt(sym, values.EmptyScopes(), environment.PhaseRuntime), qt.Equals, base,
				qt.Commentf("undefining the IMPORT destroyed the STARTUP SET's %q instead: the delete "+
					"addresses (phase, sealed) without the provenance axis, so it took the first slot "+
					"in key order, which is the base's", name))
			c.Assert(store.ImportedBindingAt(sym, values.EmptyScopes(), environment.PhaseRuntime), qt.IsNil,
				qt.Commentf("the import of %q survived the undefine that named it", name))
		})
	}
}

// TestUndefineTwiceCannotUnbindAStartupSetName is the same defect stated as a
// sequence, and it is the observable an embedder actually trips over: two
// namespace-undefine! calls unbind a name that ONE call on a fresh engine
// refuses outright.
//
// The first call destroys the base and leaves the import; the second finds only
// the import at the coordinate and removes that too. Neither call is refused,
// and the name ends up bound nowhere — a state no single undefine can produce,
// because the sealed refusal exists precisely to forbid it.
func TestUndefineTwiceCannotUnbindAStartupSetName(t *testing.T) {
	const name = "list-copy"
	undefine := `(namespace-undefine! (interaction-environment) '` + name + `)`

	// The control row. It is a GUARD, not a pin: it is green on master, and it is
	// here to say that the refusal below is the engine's ordinary answer rather
	// than an artifact of the import.
	t.Run("one undefine on a fresh engine is refused", func(t *testing.T) {
		c := qt.New(t)
		eng := baseSlotEngine(t)
		_, err := eng.EvalMultiple(context.Background(), undefine)
		c.Assert(err, qt.IsNotNil,
			qt.Commentf("undefining a startup-set name must raise; the whole sealed tier rests on it"))
	})

	t.Run("two undefines after an import cannot beat that refusal", func(t *testing.T) {
		c := qt.New(t)
		eng := baseSlotEngine(t)
		ctx := context.Background()

		_, err := eng.EvalMultiple(ctx, `(import (scheme base))`)
		c.Assert(err, qt.IsNil)

		_, err = eng.EvalMultiple(ctx, undefine)
		c.Assert(err, qt.IsNil,
			qt.Commentf("the first undefine removes the IMPORT, which is allowed"))

		_, err = eng.EvalMultiple(ctx, undefine)
		c.Assert(err, qt.IsNotNil,
			qt.Commentf("the second undefine unbound the startup set's %q: the first one deleted the "+
				"base instead of the import, so the sealed refusal had nothing left to refuse", name))
	})
}
