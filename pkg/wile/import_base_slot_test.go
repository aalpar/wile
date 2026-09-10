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
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"
)

// baseSlotEngine is the ordinary embedder shape: a KitchenSink engine that can
// resolve real libraries. Nothing here is exotic; the subject is what the single
// most common line in Scheme does to it.
func baseSlotEngine(t *testing.T) *wile.Engine {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

// TestImportDoesNotMutateTheBaseBinding is the structural pin, and it is the one
// that names the cause rather than a consequence.
//
// A phase-0 import writes (ExactPhase(0), sealed) with an EMPTY scope set. The
// startup set's own binding sits at that coordinate with NIL scopes, and
// scopeSetsEqual(nil, []) is true, so CreateGlobalBindingAt's reuse loop returns
// created == false and the import lands ON the base's binding rather than beside
// it: SetOwnGlobalValue replaces its value and markBindingImported stamps it.
//
// installImportedBinding's doc says this cannot happen — "an import landing
// BESIDE a base slot at this coordinate cannot be mistaken for the base … The two
// coexist because they are distinguishable, not because one of them is absent."
// Measured 2026-09-09, there is no beside: one slot, same pointer,
// imported false -> true, and the value replaced by the library env's own closure.
//
// The assertion holds the binding OBJECT from before the import and asks whether
// the import mutated it. That is deliberately independent of the fix's shape: a
// fix that gives the import its own slot passes, and so does one that leaves the
// base alone some other way, but nothing that keeps writing through the base can.
func TestImportDoesNotMutateTheBaseBinding(t *testing.T) {
	// `not` is a bootstrap Scheme procedure and `car` a Go primitive; the defect
	// reached both, and they take different install paths. A FRESH engine per row:
	// the import mutates engine state, so a shared one would let the first row's
	// import satisfy the second row's precondition and the failure would name the
	// wrong assertion.
	for _, name := range []string{"not", "car"} {
		t.Run(name, func(t *testing.T) {
			c := qt.New(t)
			eng := baseSlotEngine(t)
			store := eng.Environment().Namespace().Store()
			sym := values.NewSymbol(name)
			base := store.SealedBindingAt(sym, values.EmptyScopes(), environment.PhaseRuntime)
			c.Assert(base, qt.IsNotNil,
				qt.Commentf("%q must be a sealed base binding for this gate to mean anything", name))
			c.Assert(base.IsImported(), qt.IsFalse)
			baseValue := base.Value()
			c.Assert(baseValue, qt.IsNotNil)

			_, err := eng.EvalMultiple(context.Background(), `(import (scheme base))`)
			c.Assert(err, qt.IsNil,
				qt.Commentf("the import itself must succeed; the defect is that it succeeds too well"))

			c.Assert(base.IsImported(), qt.IsFalse,
				qt.Commentf("the import stamped the STARTUP SET's own binding of %q as imported, "+
					"which is what makes the base's bulk row (ownInstallsOnly) refuse to supply it", name))
			c.Assert(base.Value() == baseValue, qt.IsTrue,
				qt.Commentf("the import overwrote the startup set's own %q IN PLACE with the "+
					"library env's copy; every compiled pin to the base now reads the import's value", name))
		})
	}
}

// TestImportDoesNotStripThePhase1Vocabulary is the first observable, and the one
// an ordinary program hits. `not` is in the phase-1 macro vocabulary, so a
// transformer body may use it without importing anything — until a PHASE-0 import
// of an unrelated library covers the same name, at which point the base's bulk
// row stops supplying it (ownInstallsOnly refuses a binding carrying Imported
// meta, and the reused slot now carries it).
//
// Green on master. The narrowing row is what says the damage tracks the import's
// coverage rather than being a blanket effect.
func TestImportDoesNotStripThePhase1Vocabulary(t *testing.T) {
	tests := []struct {
		name string
		src  string
	}{
		{
			name: "no import at all",
			src:  `(begin-for-syntax (define a (not #f)))` + "\n'ok",
		},
		{
			name: "after a full phase-0 base import",
			src:  `(import (scheme base))` + "\n" + `(begin-for-syntax (define a (not #f)))` + "\n'ok",
		},
		{
			name: "after a narrow phase-0 base import that does not cover not",
			src:  `(import (only (scheme base) car))` + "\n" + `(begin-for-syntax (define a (not #f)))` + "\n'ok",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			eng := baseSlotEngine(t)
			v, err := eng.EvalMultiple(context.Background(), tc.src)
			c.Assert(err, qt.IsNil,
				qt.Commentf("`not` left the phase-1 macro vocabulary because a phase-0 import "+
					"covered the name"))
			c.Assert(v.SchemeString(), qt.Equals, "ok")
		})
	}
}

// TestImportDoesNotMakeABasePrimitiveDeletable is the second observable.
// namespace-undefine!'s own docstring promises that "a sealed binding (a
// primitive or bootstrap procedure, which lives in the immutable engine-shared
// base) cannot be undefined and raises an error". PrimNamespaceUndefine asks
// IsSealedBindingAt, which answers from the same restamped slot.
//
// Both rows are green on master. Without the import both refuse here too, so the
// import is the whole variable.
func TestImportDoesNotMakeABasePrimitiveDeletable(t *testing.T) {
	tests := []struct {
		name string
		src  string
	}{
		{
			name: "no import",
			src: `(guard (e (#t #f)) (namespace-undefine! (interaction-environment) 'caar))
(namespace-bound? (interaction-environment) 'caar)`,
		},
		{
			name: "after a phase-0 base import",
			src: `(import (scheme base))
(guard (e (#t #f)) (namespace-undefine! (interaction-environment) 'caar))
(namespace-bound? (interaction-environment) 'caar)`,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			eng := baseSlotEngine(t)
			v, err := eng.EvalMultiple(context.Background(), tc.src)
			c.Assert(err, qt.IsNil)
			c.Assert(v.SchemeString(), qt.Equals, "#t",
				qt.Commentf("the base's `caar` was deleted: the import made the startup set's own "+
					"slot look like an ordinary imported binding, so the sealed refusal never fired"))
		})
	}
}
