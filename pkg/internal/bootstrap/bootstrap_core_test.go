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

package bootstrap

import (
	"context"
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

func noopExpandPrimitive(name string) registry.PrimitiveSpec {
	return registry.PrimitiveSpec{
		Name:       name,
		ParamCount: 1,
		Impl: func(mc machine.CallContext) error {
			mc.SetValue(values.Void)
			return nil
		},
	}
}

// TestLoadBootstrapCore_ExpandPrimitiveCollision characterizes what a name
// collision at (1, sealed) now costs. Sealing registry.Apply's expand-phase
// copies moved them onto the coordinate two LATER writers in LoadBootstrapCore's
// fixed order also address, and both are covered here:
//
//   - step 2, RegisterAllPhaseHandlers → RegisterPrimitiveExpanders, which
//     writes through DefineOwnGlobal;
//   - steps 3/5, the bootstrap macro sources, which compile with env == the
//     owner's phase-0 seal, so a define-syntax/define-for-syntax in them writes
//     one phase up into that same sealed expand view.
//
// Before the carve the registry copies sat at (1, mutable), so each of these
// coexisted with the copy at a different coordinate: a registry primitive named
// after a special form was silently unreachable through the expander path, and a
// registry primitive named after a bootstrap macro was silently unreachable
// through the macro path — shadows nobody was told about.
//
// Now they share the coordinate, and under the shipped default
// (WithStableBasePrimitives, which the engine appends for WithImmutableTopLevel)
// both writers refuse the Stable slot: engine construction fails LOUDLY at the
// collision instead of silently resolving it. The refusals come from two
// different guards — DefineOwnGlobal's for the expander, and
// createPhaseBindingUnlessStable's for the macro source, whose write path is
// deliberately not DefineOwnGlobal. Neither can live in registerPhasePrimitive:
// at Apply time the slot is still empty.
//
// This is characterization, not a contract: no shipped extension registers a
// special-form or bootstrap-macro name at the expand phase. It exists so the
// failure mode is recorded rather than rediscovered. Under WithMutableTopLevel
// there is no Stable stamp and both collisions stay silent, the second writer
// winning.
func TestLoadBootstrapCore_ExpandPrimitiveCollision(t *testing.T) {
	tests := []struct {
		name  string
		build func() *registry.PrimitiveRegistry
	}{
		{
			// "if" is claimed by a primitive expander (step 2).
			name: "collides with a primitive expander",
			build: func() *registry.PrimitiveRegistry {
				reg := registry.NewRegistry()
				reg.AddPrimitive(noopExpandPrimitive("if"), registry.PhaseSetExpand)
				return reg
			},
		},
		{
			// A registry that supplies both halves of the collision itself,
			// which is the reachable shape: AddMacroSource is extension-facing.
			name: "collides with a bootstrap macro source",
			build: func() *registry.PrimitiveRegistry {
				reg := registry.NewRegistry()
				reg.AddPrimitive(noopExpandPrimitive("myprim"), registry.PhaseSetExpand)
				reg.AddMacroSource("(define-syntax myprim (syntax-rules () ((_ x) x)))")
				return reg
			},
		},
		{
			// The define-for-syntax half of the same writer. Its create goes
			// through the same helper, so the same guard fires.
			name: "collides with a bootstrap define-for-syntax",
			build: func() *registry.PrimitiveRegistry {
				reg := registry.NewRegistry()
				reg.AddPrimitive(noopExpandPrimitive("myprim"), registry.PhaseSetExpand)
				reg.AddMacroSource("(define-for-syntax myprim 42)")
				return reg
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			env := environment.NewNamespace().Runtime()
			_, err := LoadBootstrapCore(context.Background(), env, tc.build(), registry.WithStableBasePrimitives())
			qt.Assert(t, errors.Is(err, werr.ErrImmutableBinding), qt.IsTrue,
				qt.Commentf("want the second writer to refuse the occupied (1, sealed) slot, got: %v", err))
		})
	}
}

// TestLoadBootstrapCore_ExpandPrimitiveSurvivesRefusedCollision is the other
// half of the guard: the refusal must leave the registry's own copy in place.
// A guard that reported the error AFTER overwriting the slot would satisfy the
// test above and still destroy the primitive.
func TestLoadBootstrapCore_ExpandPrimitiveSurvivesRefusedCollision(t *testing.T) {
	reg := registry.NewRegistry()
	reg.AddPrimitive(noopExpandPrimitive("myprim"), registry.PhaseSetExpand)
	reg.AddMacroSource("(define-syntax myprim (syntax-rules () ((_ x) x)))")

	env := environment.NewNamespace().Runtime()
	_, err := LoadBootstrapCore(context.Background(), env, reg, registry.WithStableBasePrimitives())
	qt.Assert(t, errors.Is(err, werr.ErrImmutableBinding), qt.IsTrue)

	sealedExpand := env.SealedWriteViewAt(environment.PhaseExpand)
	gi := sealedExpand.OwnGlobalIndex(values.NewSymbol("myprim"), values.EmptyScopes())
	qt.Assert(t, gi, qt.IsNotNil)
	b := sealedExpand.GlobalEnvironment().GetOwnGlobalBinding(gi)
	qt.Assert(t, b, qt.IsNotNil)
	// The registry copy is a *machine.ForeignClosure; the transformer the macro
	// source would have written in its place is a *machine.MachineClosure.
	_, ok := b.Value().(*machine.ForeignClosure)
	qt.Assert(t, ok, qt.IsTrue,
		qt.Commentf("(1, sealed) myprim holds %T, want the registry's ForeignClosure", b.Value()))
}
