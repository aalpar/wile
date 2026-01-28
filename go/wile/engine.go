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

	"wile/environment"
	"wile/machine"
	"wile/parser"
	"wile/registry"
	"wile/registry/core"
	"wile/syntax"
	"wile/values"
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
func (e *Engine) Eval(ctx context.Context, code string) (Value, error) {
	compiled, err := e.Compile(code)
	if err != nil {
		return nil, err
	}
	return e.Run(ctx, compiled)
}

// EvalMultiple evaluates multiple expressions, returning the last result.
func (e *Engine) EvalMultiple(ctx context.Context, code string) (Value, error) {
	reader := strings.NewReader(code)
	p := parser.NewParser(e.env, true, reader)

	var lastResult Value
	for {
		stx, err := p.ReadSyntax(ctx)
		if err != nil {
			if isEOF(err) {
				break
			}
			return nil, err
		}

		compiled, err := e.compileExpr(stx)
		if err != nil {
			return nil, err
		}

		result, err := e.runCompiled(ctx, compiled)
		if err != nil {
			return nil, err
		}
		lastResult = result
	}

	return lastResult, nil
}

// Compile parses and compiles code without executing.
func (e *Engine) Compile(code string) (*CompiledCode, error) {
	reader := strings.NewReader(code)
	p := parser.NewParser(e.env, true, reader)

	stx, err := p.ReadSyntax(context.Background())
	if err != nil {
		return nil, err
	}

	return e.compileExpr(stx)
}

// Run executes previously compiled code.
func (e *Engine) Run(ctx context.Context, cc *CompiledCode) (Value, error) {
	return e.runCompiled(ctx, cc)
}

// Define binds a value to a name in the top-level environment.
func (e *Engine) Define(name string, value Value) error {
	sym := e.env.InternSymbol(values.NewSymbol(name))
	e.env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
	return e.env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), unwrapValue(value))
}

// Get retrieves a value by name from the environment.
func (e *Engine) Get(name string) (Value, bool) {
	sym := e.env.InternSymbol(values.NewSymbol(name))
	idx := environment.NewGlobalIndex(sym)
	binding := e.env.GetGlobalBinding(idx)
	if binding == nil {
		return nil, false
	}
	return wrapValue(binding.Value()), true
}

// RegisterPrimitive adds a Go function as a Scheme primitive.
func (e *Engine) RegisterPrimitive(spec PrimitiveSpec) error {
	sym := e.env.InternSymbol(values.NewSymbol(spec.Name))
	e.env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)

	closure := machine.NewForeignClosure(
		e.env,
		spec.ParamCount,
		spec.IsVariadic,
		spec.Impl,
	)

	return e.env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure)
}

// Call invokes a Scheme procedure with arguments.
func (e *Engine) Call(ctx context.Context, proc Value, args ...Value) (Value, error) {
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
	cont := machine.NewMachineContinuation(nil, tpl, e.env)
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
func (e *Engine) Environment() *environment.EnvironmentFrame {
	return e.env
}

// TopLevelEnvironment returns the TopLevelEnvironment for advanced use.
// This provides access to per-instance symbol interning and phase management.
func (e *Engine) TopLevelEnvironment() *environment.TopLevelEnvironment {
	return e.topLevel
}

// internal helpers

func (e *Engine) compileExpr(stx syntax.SyntaxValue) (*CompiledCode, error) {
	tpl := machine.NewNativeTemplate(0, 0, false)

	ectx := machine.NewExpandTimeCallContext()
	expanded, err := machine.NewExpanderTimeContinuation(e.env).ExpandExpression(ectx, stx)
	if err != nil {
		return nil, err
	}

	cctx := machine.NewCompileTimeCallContext(false, true, e.env)
	err = machine.NewCompiletimeContinuation(tpl, e.env).CompileExpression(cctx, expanded)
	if err != nil {
		return nil, err
	}

	return &CompiledCode{template: tpl, env: e.env}, nil
}

func (e *Engine) runCompiled(ctx context.Context, cc *CompiledCode) (Value, error) {
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
