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

// Apply materializes registry contents into an environment: compile-time bindings,
// runtime/expand-time primitives, global values, and init functions (in that order).
func (p *Registry) Apply(ctx context.Context, env *environment.EnvironmentFrame) error {
	p.mu.RLock()
	defer p.mu.RUnlock()

	// Register compile-time bindings first
	for _, spec := range p.bindingSpecs {
		err := registerCompileTimeBinding(env, spec)
		if err != nil {
			return err
		}
	}

	// Register compile-time primitives (bindings only, no values)
	for _, reg := range p.primitives {
		if reg.Phases.HasCompile() && !reg.Phases.HasRuntime() {
			err := registerCompileTimeBinding(env, BindingSpec{Name: reg.Spec.Name})
			if err != nil {
				return err
			}
		}
	}

	// Register runtime primitives
	for _, reg := range p.primitives {
		if reg.Phases.HasRuntime() {
			err := registerRuntimePrimitive(env, reg.Spec)
			if err != nil {
				return err
			}
		}
	}

	// Register expand-time primitives
	for _, reg := range p.primitives {
		if reg.Phases.HasExpand() {
			err := registerExpandTimePrimitive(env, reg.Spec)
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

func registerRuntimePrimitive(env *environment.EnvironmentFrame, spec PrimitiveSpec) error {
	sym := values.NewSymbol(spec.Name)
	env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)

	closure := machine.NewForeignClosure(
		env,
		spec.ParamCount,
		spec.IsVariadic,
		spec.Impl,
	)
	closure.SetName(spec.Name)
	closure.SetDoc(spec.Doc)

	err := env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure)
	if err != nil {
		return werr.WrapForeignErrorf(err, "error registering %s", spec.Name)
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

func registerExpandTimePrimitive(env *environment.EnvironmentFrame, spec PrimitiveSpec) error {
	expandEnv := env.Expand()
	sym := values.NewSymbol(spec.Name)
	expandEnv.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)

	closure := machine.NewForeignClosure(
		expandEnv,
		spec.ParamCount,
		spec.IsVariadic,
		spec.Impl,
	)
	closure.SetName(spec.Name)
	closure.SetDoc(spec.Doc)

	err := expandEnv.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure)
	if err != nil {
		return werr.WrapForeignErrorf(err, "error registering expand-time primitive %s", spec.Name)
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
