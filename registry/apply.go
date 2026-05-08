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

package registry

import (
	"context"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// ApplyOption configures the behavior of Registry.Apply. Options are applied
// in order; later options override earlier ones.
type ApplyOption func(*applyConfig)

// applyConfig holds tunables for Apply. New knobs should extend this struct
// and be set via an ApplyOption constructor.
type applyConfig struct {
	contractEnforcement bool
}

// WithContractEnforcement installs a type-checking validator on each
// registered primitive whose spec declares ParamTypes. The validator runs
// after argument binding and before the implementation, rejecting
// mismatched types with a wrapped error. Disabled by default — validators
// cost nothing when not installed (ForeignClosure.validate stays nil).
func WithContractEnforcement() ApplyOption {
	return func(cfg *applyConfig) {
		cfg.contractEnforcement = true
	}
}

// Apply materializes registry contents into an environment: compile-time bindings,
// runtime/expand-time primitives, global values, and init functions (in that order).
func (p *Registry) Apply(ctx context.Context, env *environment.EnvironmentFrame, opts ...ApplyOption) error {
	var cfg applyConfig
	for _, opt := range opts {
		opt(&cfg)
	}

	p.mu.RLock()
	defer p.mu.RUnlock()

	// Register compile-time bindings first
	for _, spec := range p.bindingSpecs {
		err := registerCompileTimeBinding(env, spec)
		if err != nil {
			return err
		}
	}

	// Register compile-only primitives (binding-only, no runtime value).
	// Skipped for primitives that also have PhaseRuntime — the runtime path
	// installs the binding via SetOwnGlobalValue.
	for _, reg := range p.primitives {
		if reg.Phases.Has(environment.PhaseCompile) && !reg.Phases.Has(environment.PhaseRuntime) {
			err := registerCompileTimeBinding(env, BindingSpec{Name: reg.Spec.Name})
			if err != nil {
				return err
			}
		}
	}

	// Register runtime and expand primitives. Both create a ForeignClosure
	// in a phase-specific environment frame; only the frame differs. Iterate
	// the phase axis as data instead of replicating the loop body.
	phaseTargets := []struct {
		phase environment.Phase
		env   *environment.EnvironmentFrame
	}{
		{environment.PhaseRuntime, env},
		{environment.PhaseExpand, env.Expand()},
	}
	for _, pt := range phaseTargets {
		for _, reg := range p.primitives {
			if !reg.Phases.Has(pt.phase) {
				continue
			}
			err := registerPhasePrimitive(pt.env, pt.phase, reg.Spec, cfg.contractEnforcement)
			if err != nil {
				return err
			}
		}
	}

	// Register global values
	for _, gv := range p.globalValues {
		err := registerGlobalValue(env, gv.Name, gv.Value)
		if err != nil {
			return err
		}
	}

	// Run initialization functions
	for _, f := range p.initFuncs {
		err := f()
		if err != nil {
			return err
		}
	}

	return nil
}

//nolint:unparam // Returns error for consistency with other register functions
func registerCompileTimeBinding(env *environment.EnvironmentFrame, spec BindingSpec) error {
	compileEnv := env.Compile()
	sym := values.NewSymbol(spec.Name)
	compileEnv.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypePrimitive)
	return nil
}

// registerPhasePrimitive installs a ForeignClosure for spec in phaseEnv.
// The phase parameter is used only for error message context; the actual
// target frame is phaseEnv (chosen by the caller). This is the single
// registration helper shared by Runtime and Expand phases — earlier
// versions had two near-identical helpers differing only in target env
// and error message; collapsed per Instance C of the dispatch-axis-as-data
// finding (plans/2026-05-08-dispatch-axis-as-data.md).
func registerPhasePrimitive(phaseEnv *environment.EnvironmentFrame, phase environment.Phase, spec PrimitiveSpec, contractEnforcement bool) error {
	sym := values.NewSymbol(spec.Name)
	phaseEnv.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)

	closure := machine.NewForeignClosure(
		phaseEnv,
		spec.ParamCount,
		spec.IsVariadic,
		spec.Impl,
	)
	closure.SetName(spec.Name)
	closure.SetDoc(spec.Doc)
	if contractEnforcement {
		closure.SetValidator(BuildValidator(spec))
	}

	err := phaseEnv.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure)
	if err != nil {
		return werr.WrapForeignErrorf(err, "error registering %s at phase %s", spec.Name, phase)
	}
	return nil
}

func registerGlobalValue(env *environment.EnvironmentFrame, name string, value values.Value) error {
	sym := values.NewSymbol(name)
	env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)

	err := env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), value)
	if err != nil {
		return werr.WrapForeignErrorf(err, "error registering global value %s", name)
	}
	return nil
}

// ApplyDocs attaches documentation entries to existing bindings in the environment.
// It searches all phases for each documented name and sets the doc string on every
// matching binding. This is necessary because some names (e.g., special forms) have
// bindings in multiple phases (expand and compile), and the REPL's ,doc command may
// find any of them.
func (p *Registry) ApplyDocs(env *environment.EnvironmentFrame) {
	p.mu.RLock()
	defer p.mu.RUnlock()

	topLevel := env.Namespace()
	if topLevel == nil {
		return
	}
	phases := topLevel.Phases()

	// Merge both doc sources: explicit DocEntry entries and BindingSpec.Doc fields.
	allDocs := make([]DocEntry, 0, len(p.docs)+len(p.bindingSpecs))
	allDocs = append(allDocs, p.docs...)
	for _, spec := range p.bindingSpecs {
		if spec.Doc != "" {
			allDocs = append(allDocs, DocEntry(spec))
		}
	}

	for _, doc := range allDocs {
		sym := values.NewSymbol(doc.Name)
		for _, phase := range phases.Phases() {
			phaseEnv := phases.Get(phase)
			if phaseEnv == nil {
				continue
			}
			bnd := phaseEnv.GetBinding(sym, nil)
			if bnd != nil {
				bnd.SetDoc(doc.Doc)
			}
		}
	}
}
