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

package wile

import (
	"context"
	"strings"

	"github.com/aalpar/wile/go/environment"
	"github.com/aalpar/wile/go/machine"
	"github.com/aalpar/wile/go/parser"
	"github.com/aalpar/wile/go/registry"
	"github.com/aalpar/wile/go/registry/core"
	"github.com/aalpar/wile/go/syntax"
	"github.com/aalpar/wile/go/values"
)

// Engine is the main entry point for embedding Wile.
type Engine struct {
	topLevel *environment.TopLevelEnvironment
	env      *environment.EnvironmentFrame
	registry *registry.Registry
}

// NewEngine creates a new Wile engine.
// By default, only core primitives are included.
// Use WithExtension to add optional extensions.
func NewEngine(opts ...EngineOption) (*Engine, error) {
	cfg := &engineConfig{
		registry: nil,
	}
	for _, opt := range opts {
		opt(cfg)
	}

	// Build registry
	reg := cfg.registry
	if reg == nil {
		reg = registry.NewRegistry()
		err := core.AddToRegistry(reg)
		if err != nil {
			return nil, err
		}
	}

	// Add any additional extensions
	for _, ext := range cfg.extensions {
		err := ext.AddToRegistry(reg)
		if err != nil {
			return nil, err
		}
	}

	// Create TopLevelEnvironment (per-instance symbol interning)
	topLevel := environment.NewTopLevelEnvironment()
	env := topLevel.Runtime()

	// Apply registry
	ctx := context.Background()
	err := reg.Apply(ctx, env)
	if err != nil {
		return nil, err
	}

	// Register syntax compilers and primitive expanders
	err = machine.RegisterSyntaxCompilers(env)
	if err != nil {
		return nil, &Error{Message: "failed to register syntax compilers", Cause: err}
	}

	err = machine.RegisterPrimitiveExpanders(env)
	if err != nil {
		return nil, &Error{Message: "failed to register primitive expanders", Cause: err}
	}

	// Load bootstrap macros
	err = loadBootstrapMacros(ctx, env, reg.MacroSources())
	if err != nil {
		return nil, err
	}

	q := &Engine{
		topLevel: topLevel,
		env:      env,
		registry: reg,
	}
	return q, nil
}

// Eval parses, compiles, and executes Scheme code, returning the result.
func (p *Engine) Eval(ctx context.Context, code string) (Value, error) {
	compiled, err := p.Compile(code)
	if err != nil {
		return nil, err
	}
	return p.Run(ctx, compiled)
}

// EvalMultiple evaluates multiple expressions, returning the last result.
func (p *Engine) EvalMultiple(ctx context.Context, code string) (Value, error) {
	reader := strings.NewReader(code)
	pr := parser.NewParser(p.env, true, reader)

	var lastResult Value
	for {
		stx, err := pr.ReadSyntax(ctx)
		if err != nil {
			if isEOF(err) {
				break
			}
			return nil, err
		}

		compiled, err := p.compileExpr(stx)
		if err != nil {
			return nil, err
		}

		result, err := p.runCompiled(ctx, compiled)
		if err != nil {
			return nil, err
		}
		lastResult = result
	}

	return lastResult, nil
}

// Compile parses and compiles code without executing.
func (p *Engine) Compile(code string) (*CompiledCode, error) {
	reader := strings.NewReader(code)
	pr := parser.NewParser(p.env, true, reader)

	stx, err := pr.ReadSyntax(context.Background())
	if err != nil {
		return nil, err
	}

	return p.compileExpr(stx)
}

// Run executes previously compiled code.
func (p *Engine) Run(ctx context.Context, cc *CompiledCode) (Value, error) {
	return p.runCompiled(ctx, cc)
}

// Define binds a value to a name in the top-level environment.
func (p *Engine) Define(name string, value Value) error {
	sym := p.env.InternSymbol(values.NewSymbol(name))
	p.env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
	return p.env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), unwrapValue(value))
}

// Get retrieves a value by name from the environment.
func (p *Engine) Get(name string) (Value, bool) {
	sym := p.env.InternSymbol(values.NewSymbol(name))
	idx := environment.NewGlobalIndex(sym)
	binding := p.env.GetGlobalBinding(idx)
	if binding == nil {
		return nil, false
	}
	return wrapValue(binding.Value()), true
}

// RegisterPrimitive adds a Go function as a Scheme primitive.
func (p *Engine) RegisterPrimitive(spec PrimitiveSpec) error {
	sym := p.env.InternSymbol(values.NewSymbol(spec.Name))
	p.env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)

	closure := machine.NewForeignClosure(
		p.env,
		spec.ParamCount,
		spec.IsVariadic,
		spec.Impl,
	)

	return p.env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure)
}

// Call invokes a Scheme procedure with arguments.
func (p *Engine) Call(ctx context.Context, proc Value, args ...Value) (Value, error) {
	closure, ok := unwrapValue(proc).(*machine.MachineClosure)
	if !ok {
		return nil, &Error{Message: "not a procedure"}
	}

	unwrappedArgs := make([]values.Value, len(args))
	for i, arg := range args {
		unwrappedArgs[i] = unwrapValue(arg)
	}

	// Create a template and continuation to start the machine
	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, p.env)
	mc := machine.NewMachineContext(ctx, cont)

	// Create sub-context and apply the closure
	sub := mc.NewSubContext()
	_, err := sub.Apply(closure, unwrappedArgs...)
	if err != nil {
		return nil, err
	}

	err = sub.Run()
	if err != nil {
		if err != machine.ErrMachineHalt {
			return nil, err
		}
	}

	return wrapValue(sub.GetValue()), nil
}

// Environment returns the underlying environment for advanced use.
func (p *Engine) Environment() *environment.EnvironmentFrame {
	return p.env
}

// TopLevelEnvironment returns the TopLevelEnvironment for advanced use.
// This provides access to per-instance symbol interning and phase management.
func (p *Engine) TopLevelEnvironment() *environment.TopLevelEnvironment {
	return p.topLevel
}

// internal helpers

func (p *Engine) compileExpr(stx syntax.SyntaxValue) (*CompiledCode, error) {
	tpl := machine.NewNativeTemplate(0, 0, false)

	ectx := machine.NewExpandTimeCallContext()
	expanded, err := machine.NewExpanderTimeContinuation(p.env).ExpandExpression(ectx, stx)
	if err != nil {
		return nil, err
	}

	cctx := machine.NewCompileTimeCallContext(false, true, p.env)
	err = machine.NewCompiletimeContinuation(tpl, p.env).CompileExpression(cctx, expanded)
	if err != nil {
		return nil, err
	}

	return &CompiledCode{template: tpl, env: p.env}, nil
}

func (p *Engine) runCompiled(ctx context.Context, cc *CompiledCode) (Value, error) {
	cont := machine.NewMachineContinuation(nil, cc.template, cc.env)
	mc := machine.NewMachineContext(ctx, cont)
	err := mc.Run()
	if err != nil {
		return nil, err
	}
	return wrapValue(mc.GetValue()), nil
}

func loadBootstrapMacros(ctx context.Context, env *environment.EnvironmentFrame, sources []string) error {
	for _, source := range sources {
		reader := strings.NewReader(source)
		p := parser.NewParser(env, true, reader)

		for {
			stx, err := p.ReadSyntax(ctx)
			if err != nil {
				if isEOF(err) {
					break
				}
				return err
			}

			tpl := machine.NewNativeTemplate(0, 0, false)
			ectx := machine.NewExpandTimeCallContext()
			expanded, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
			if err != nil {
				return err
			}

			cctx := machine.NewCompileTimeCallContext(false, true, env)
			err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
			if err != nil {
				return err
			}

			cont := machine.NewMachineContinuation(nil, tpl, env)
			mc := machine.NewMachineContext(ctx, cont)
			err = mc.Run()
			if err != nil {
				if err != machine.ErrMachineHalt {
					return err
				}
			}
		}
	}
	return nil
}
