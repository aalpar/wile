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

// Apply registers all primitives and runs init functions on an environment.
func (p *Registry) Apply(ctx context.Context, env *environment.EnvironmentFrame) error {
	p.mu.RLock()
	defer p.mu.RUnlock()

	// Register compile-time bindings first
	for _, name := range p.bindings {
		err := registerCompileTimeBinding(env, name)
		if err != nil {
			return err
		}
	}

	// Register compile-time primitives (bindings only, no values)
	for _, reg := range p.primitives {
		if reg.Phases.HasCompile() && !reg.Phases.HasRuntime() {
			err := registerCompileTimeBinding(env, reg.Spec.Name)
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
func registerCompileTimeBinding(env *environment.EnvironmentFrame, name string) error {
	compileEnv := env.Compile()
	sym := compileEnv.InternSymbol(values.NewSymbol(name))
	compileEnv.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypePrimitive)
	return nil
}

func registerRuntimePrimitive(env *environment.EnvironmentFrame, spec PrimitiveSpec) error {
	sym := env.InternSymbol(values.NewSymbol(spec.Name))
	env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)

	closure := machine.NewForeignClosure(
		env,
		spec.ParamCount,
		spec.IsVariadic,
		spec.Impl,
	)
	closure.SetName(spec.Name)

	err := env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure)
	if err != nil {
		return werr.WrapForeignErrorf(err, "error registering %s", spec.Name)
	}
	return nil
}

func registerGlobalValue(env *environment.EnvironmentFrame, name string, value values.Value) error {
	sym := env.InternSymbol(values.NewSymbol(name))
	env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)

	err := env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), value)
	if err != nil {
		return werr.WrapForeignErrorf(err, "error registering global value %s", name)
	}
	return nil
}

func registerExpandTimePrimitive(env *environment.EnvironmentFrame, spec PrimitiveSpec) error {
	expandEnv := env.Expand()
	sym := expandEnv.InternSymbol(values.NewSymbol(spec.Name))
	expandEnv.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)

	closure := machine.NewForeignClosure(
		expandEnv,
		spec.ParamCount,
		spec.IsVariadic,
		spec.Impl,
	)
	closure.SetName(spec.Name)

	err := expandEnv.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure)
	if err != nil {
		return werr.WrapForeignErrorf(err, "error registering expand-time primitive %s", spec.Name)
	}
	return nil
}
