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
	"slices"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"
)

// Nothing is BOUND at phase 2 when an engine starts. Every fixed coordinate the
// registry writes is (ExactPhase(0), sealed), phase 1 (bootstrap macros and
// primitive expanders), or the mutable runtime. This pins the retirement of the
// phase-2 keyword coordinate: a registration that reintroduces a fixed phase-2
// resident shows up here as a name that resolves at that view's own
// coordinates.
//
// Stage A of the Flatt binding model removed the fourth possibility, the
// ambient (ANY-phase, sealed) coordinate: a sealed phase-0 write now lands at
// ExactPhase(0), and a phase-2 view reaches phase-0 names only if the dialect
// declares an initial import at that phase, which none does.
//
// The phase-2 FRAME is present at startup, and that is a footprint fact rather
// than a binding one. P0.1 made a transformer right-hand side an expression
// compiled at env.NextPhase(), so every bootstrap syntax-rules definition now
// runs an expander rooted at phase 1, and lookupMacroBinding's arm 2 probes
// NextPhase() of that — phase 2 — which AtPhase instantiates
// (phases.GetOrCreate). The frame is empty; the assertion below is what says so,
// and it is the claim this test exists to make. PresentPhases() unions
// instantiated frames with binding-bearing coordinates, so it cannot separate
// the two on its own.
func TestStartupBindsNothingAtPhaseTwo(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()

	env := eng.Environment()
	qt.Assert(t, env.PresentPhases(), qt.DeepEquals,
		[]environment.Phase{environment.PhaseRuntime, environment.PhaseExpand, environment.Phase(2)})

	phase2 := env.AtPhase(environment.Phase(2))
	for _, name := range eng.BoundNames() {
		gi := phase2.OwnGlobalIndex(values.NewSymbol(name), values.AllScopes())
		qt.Assert(t, gi, qt.IsNil, qt.Commentf("%s is bound at phase 2", name))
	}
}

// No keyword name holds a procedure. Every non-DocOnly BindingSpec becomes a
// BindingTypePrimitive binding, and refuseCompileTimeMeaning refuses
// BindingTypePrimitive in value position, so a name that ALSO has to be a
// first-class value cannot be one of them. The two that must not regress are
// apply (a runtime primitive) and dynamic-wind (a bootstrap Scheme define):
// both are R7RS procedures the compiler recognizes in head position, both live
// in procedureFormDocs as DocOnly rows, and both would break in value position
// if an AddBinding put a keyword in the slot their value needs; the write
// order is keyword-first, and DefineOwnGlobal cannot retype an existing slot.
//
// The coordinate the walk reads is (ExactPhase(0), sealed), and Stage A of the
// Flatt binding model is why: a keyword row is a sealed write from a phase-0
// view, which used to be diverted to the ambient (ANY-phase) tier and is now
// left where it was written. The probe moved with it; the claim did not. Read
// at phase 0 rather than through the engine frame deliberately — the frame
// would also answer from the dialect's bulk rows, and a keyword that had lost
// its own slot would still look present.
//
// The gate is the VALUE, not the name: a keyword slot legitimately holds void
// (auxiliary syntax, the compiled forms) or the syntax compiler for names in
// both tables (define-syntax, import, …). Anything applicable there means a
// procedure name was registered as a keyword. TestDynamicWindIsAFirstClassProcedure
// and TestCompiledApply are the behavioural pins; this is the ratchet that names
// the cause before they redden.
//
// The two engines differ in one thing: whether removedForms is empty. Under
// KitchenSink it is, so the walk covers only the unnarrowed installation. A
// dialect that removes a form makes the engine narrow the top-level registry
// with WithoutBindings before Apply, which is the second way a name can be in
// the registry's specs and NOT hold a keyword slot; the first is DocOnly.
// Both must leave the predicate true; only the walked set changes.
func TestKeywordSlotsNeverHoldAProcedure(t *testing.T) {
	ctx := context.Background()
	tests := []struct {
		name string
		opts []wile.EngineOption
		// gone is a form name the dialect removed, so its compile-time binding
		// must be absent from the effective registry. Empty means nothing was
		// narrowed.
		gone string
	}{
		{
			name: "KitchenSink installs every keyword",
			opts: []wile.EngineOption{wile.WithProfile(wile.KitchenSink)},
		},
		{
			name: "a dialect that removed set! narrows the keywords too",
			opts: []wile.EngineOption{wile.WithProfile(wile.KitchenSink), wile.WithDialect(removeSetBangFormsDialect{})},
			gone: "set!",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			eng, err := wile.NewEngine(ctx, tc.opts...)
			qt.Assert(t, err, qt.IsNil)
			defer func() {
				_ = eng.Close()
			}()

			store := eng.Environment().GlobalEnvironment()
			qt.Assert(t, store, qt.IsNotNil)
			reg := eng.EffectiveRegistry()

			if tc.gone != "" {
				qt.Assert(t, slices.Contains(reg.Bindings(), tc.gone), qt.IsFalse,
					qt.Commentf("a removed form must lose its compile-time binding, not just its form"))
			}

			checked := 0
			for _, spec := range reg.BindingSpecs() {
				if spec.DocOnly {
					continue
				}
				bnd, ambiguous := store.ExactBindingAt(values.NewSymbol(spec.Name),
					values.AllScopes(), environment.PhaseRuntime)
				qt.Assert(t, ambiguous, qt.IsFalse, qt.Commentf("%s: phase-0 tie at startup", spec.Name))
				qt.Assert(t, bnd, qt.IsNotNil,
					qt.Commentf("keyword %q has no (ExactPhase(0), sealed) binding", spec.Name))
				qt.Assert(t, bnd.BindingType(), qt.Equals, environment.BindingTypePrimitive,
					qt.Commentf("keyword %q", spec.Name))
				checked++

				v := bnd.Value()
				if v == nil || v == values.Void {
					continue
				}
				_, isCompiler := v.(*compilation.SyntaxCompiler)
				qt.Assert(t, isCompiler, qt.IsTrue,
					qt.Commentf("keyword %q holds %s (%T): a name that carries a runtime value "+
						"must be a DocOnly row (procedureFormDocs), not an installed keyword",
						spec.Name, v.SchemeString(), v))
			}
			// Guard the walk itself: an empty BindingSpecs would make every assertion
			// above vacuous.
			qt.Assert(t, checked > 20, qt.IsTrue, qt.Commentf("only %d keywords walked", checked))
		})
	}
}

// removeSetBangFormsDialect drops set! from the per-engine forms registry. That
// is what makes the engine compute a non-empty removedForms and reach
// WithoutBindings; nothing else in the test suite drives that path from a
// profile-built engine.
type removeSetBangFormsDialect struct{}

func (removeSetBangFormsDialect) Name() string {
	return "no-set!"
}

func (removeSetBangFormsDialect) InstallForms(fr *forms.FormRegistry) error {
	fr.Remove("set!")
	return nil
}
