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

// TestLoadBootstrapCore_ExpandPrimitiveCollidesWithExpander characterizes what a
// name collision at (1, sealed) now costs. Two writers reach that coordinate in
// LoadBootstrapCore's fixed order: step 1 (reg.Apply's phaseTargets) and step 2
// (RegisterAllPhaseHandlers → RegisterPrimitiveExpanders). Before the phase-1
// carve the first wrote at (1, mutable), so the two coexisted at different
// coordinates and a registry primitive named after a special form was silently
// unreachable through the expander path — a shadow nobody was told about.
//
// Now they share the coordinate, and under the shipped default
// (WithStableBasePrimitives, which the engine appends for WithImmutableTopLevel)
// the Stable stamp from step 1 makes step 2's DefineOwnGlobal refuse: engine
// construction fails LOUDLY at the collision instead of silently resolving it.
// The guard is Phase 4's, in DefineOwnGlobal, and it fires from the SECOND
// writer — a created-guard inside registerPhasePrimitive could not catch this,
// because at Apply time the slot is still empty.
//
// This is characterization, not a contract: no shipped extension registers a
// special-form name at the expand phase. It exists so the failure mode is
// recorded rather than rediscovered.
func TestLoadBootstrapCore_ExpandPrimitiveCollidesWithExpander(t *testing.T) {
	reg := registry.NewRegistry()
	reg.AddPrimitive(registry.PrimitiveSpec{
		Name:       "if",
		ParamCount: 1,
		Impl: func(mc machine.CallContext) error {
			mc.SetValue(values.Void)
			return nil
		},
	}, registry.PhaseSetExpand)

	env := environment.NewNamespace().Runtime()
	_, err := LoadBootstrapCore(context.Background(), env, reg, registry.WithStableBasePrimitives())
	qt.Assert(t, errors.Is(err, werr.ErrImmutableBinding), qt.IsTrue,
		qt.Commentf("want the phase-handler pass to refuse the occupied (1, sealed) slot, got: %v", err))
}
