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

// Nothing is bound at phase 2 when an engine starts. Every fixed coordinate the
// registry writes is phase 0 (ambient or sealed), phase 1 (bootstrap macros and
// primitive expanders), or the mutable runtime; phase 2 exists only once a
// transformer body defines a macro of its own. This pins the retirement of the
// phase-2 keyword coordinate: a registration that reintroduces a fixed phase-2
// resident shows up here as a third present phase.
func TestStartupPresentPhasesAreZeroAndOne(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()

	got := eng.Environment().PresentPhases()
	qt.Assert(t, got, qt.DeepEquals, []environment.Phase{environment.PhaseRuntime, environment.PhaseExpand})
}

// No keyword name holds a procedure. Every non-DocOnly BindingSpec becomes an
// ambient BindingTypePrimitive binding, and refuseCompileTimeMeaning refuses
// BindingTypePrimitive in value position, so a name that ALSO has to be a
// first-class value cannot be one of them. The two that must not regress are
// apply (a runtime primitive) and dynamic-wind (a bootstrap Scheme define):
// both are R7RS procedures the compiler recognizes in head position, both live
// in procedureFormDocs as DocOnly rows, and both would break in value position
// if an AddBinding put a keyword in the slot their value needs; the write
// order is keyword-first, and DefineOwnGlobal cannot retype an existing slot.
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
// the registry's specs and NOT be an ambient keyword; the first is DocOnly.
// Both must leave the predicate true; only the walked set changes.
func TestAmbientKeywordsNeverHoldAProcedure(t *testing.T) {
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
				bnd, ambiguous := store.AmbientBinding(values.NewSymbol(spec.Name), values.AllScopes())
				qt.Assert(t, ambiguous, qt.IsFalse, qt.Commentf("%s: ambient tie at startup", spec.Name))
				qt.Assert(t, bnd, qt.IsNotNil, qt.Commentf("keyword %q has no ambient binding", spec.Name))
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
