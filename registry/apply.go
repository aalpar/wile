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
)

// ApplyContext provides context during registry application.
type ApplyContext interface {
	Context() context.Context
	Environment() *environment.EnvironmentFrame
}

type applyContext struct {
	ctx context.Context
	env *environment.EnvironmentFrame
}

func (p *applyContext) Context() context.Context {
	return p.ctx
}

func (p *applyContext) Environment() *environment.EnvironmentFrame {
	return p.env
}

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

	// Run initialization functions
	actx := &applyContext{ctx: ctx, env: env}
	for _, f := range p.initFuncs {
		err := f(actx)
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

	err := env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure)
	if err != nil {
		return values.WrapForeignErrorf(err, "error registering %s", spec.Name)
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

	err := expandEnv.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure)
	if err != nil {
		return values.WrapForeignErrorf(err, "error registering expand-time primitive %s", spec.Name)
	}
	return nil
}
