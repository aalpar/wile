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
// BindingTypePrimitive in value position — so a name that ALSO has to be a
// first-class value cannot be one of them. The two that must not regress are
// apply (a runtime primitive) and dynamic-wind (a bootstrap Scheme define):
// both are R7RS procedures the compiler recognizes in head position, both live
// in procedureFormDocs as DocOnly rows, and both would break in value position
// if an AddBinding put a keyword in the slot their value needs — the write
// order is keyword-first, and DefineOwnGlobal cannot retype an existing slot.
//
// The gate is the VALUE, not the name: a keyword slot legitimately holds void
// (auxiliary syntax, the compiled forms) or the syntax compiler for names in
// both tables (define-syntax, import, …). Anything applicable there means a
// procedure name was registered as a keyword. TestDynamicWindIsAFirstClassProcedure
// and TestCompiledApply are the behavioural pins; this is the ratchet that names
// the cause before they redden.
func TestAmbientKeywordsNeverHoldAProcedure(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()

	store := eng.Environment().GlobalEnvironment()
	qt.Assert(t, store, qt.IsNotNil)

	checked := 0
	for _, spec := range eng.EffectiveRegistry().BindingSpecs() {
		if spec.DocOnly {
			continue
		}
		bnd := store.AmbientBinding(values.NewSymbol(spec.Name), values.AllScopes())
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
			qt.Commentf("keyword %q holds %s (%T) — a name that carries a runtime value "+
				"must be a DocOnly row (procedureFormDocs), not an installed keyword",
				spec.Name, v.SchemeString(), v))
	}
	// Guard the walk itself: an empty BindingSpecs would make every assertion
	// above vacuous.
	qt.Assert(t, checked > 20, qt.IsTrue, qt.Commentf("only %d keywords walked", checked))
}
