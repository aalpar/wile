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
	"errors"
	"fmt"
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/registry/core"
	"github.com/aalpar/wile/values"
)

// Engine is the main entry point for embedding Wile.
//
// An Engine is NOT safe for concurrent use from multiple goroutines.
// The underlying environment's global bindings use a RWMutex (concurrent
// reads are safe), but Eval, Compile, and Run mutate the environment.
// Each goroutine should use its own Engine, or synchronize externally.
//
// SRFI-18 threads within a single Engine are safe — the VM handles
// thread coordination internally.
type Engine struct {
	topLevel     *environment.TopLevelEnvironment
	env          *environment.EnvironmentFrame
	registry     *registry.Registry
	lastCounters machine.VMCounters
}

// NewEngine creates a new Wile engine.
// By default, only core primitives are included.
// Use WithExtension to add optional extensions.
func NewEngine(ctx context.Context, opts ...EngineOption) (*Engine, error) {
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
	compiled, err := p.Compile(ctx, code)
	if err != nil {
		return nil, err
	}
	return p.Run(ctx, compiled)
}

// EvalWithSource parses, compiles, and executes Scheme code, returning the result.
// The source parameter identifies where the code came from (e.g. a filename)
// and appears in error messages and stack traces.
func (p *Engine) EvalWithSource(ctx context.Context, code string, source string) (Value, error) {
	compiled, err := p.CompileWithSource(ctx, code, source)
	if err != nil {
		return nil, err
	}
	return p.Run(ctx, compiled)
}

// EvalMultiple evaluates multiple expressions, returning the last result.
func (p *Engine) EvalMultiple(ctx context.Context, code string) (Value, error) {
	return p.evalMultiple(ctx, code, "")
}

// EvalMultipleWithSource evaluates multiple expressions, returning the last result.
// The source parameter identifies where the code came from (e.g. a filename)
// and appears in error messages and stack traces.
func (p *Engine) EvalMultipleWithSource(ctx context.Context, code string, source string) (Value, error) {
	return p.evalMultiple(ctx, code, source)
}

func (p *Engine) evalMultiple(ctx context.Context, code string, source string) (Value, error) {
	reader := strings.NewReader(code)
	pr := parser.NewParserWithFile(p.env, true, reader, source)

	var lastResult = Void
	for {
		stx, err := pr.ReadSyntax(ctx)
		if err != nil {
			if isEOF(err) {
				break
			}
			return nil, &CompilationError{Message: "parse error", Cause: err}
		}

		compiled, err := p.compileExpr(ctx, stx)
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
func (p *Engine) Compile(ctx context.Context, code string) (*CompiledCode, error) {
	return p.compile(ctx, code, "")
}

// CompileWithSource parses and compiles code without executing.
// The source parameter identifies where the code came from (e.g. a filename)
// and appears in error messages and stack traces.
func (p *Engine) CompileWithSource(ctx context.Context, code string, source string) (*CompiledCode, error) {
	return p.compile(ctx, code, source)
}

func (p *Engine) compile(ctx context.Context, code string, source string) (*CompiledCode, error) {
	reader := strings.NewReader(code)
	pr := parser.NewParserWithFile(p.env, true, reader, source)

	stx, err := pr.ReadSyntax(ctx)
	if err != nil {
		return nil, &CompilationError{Message: "parse error", Cause: err}
	}

	return p.compileExpr(ctx, stx)
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
// Supports all callable types: lambdas, case-lambdas, and parameters.
// Composable continuations cannot be called from Go (they require the VM
// winding stack) and return an error.
func (p *Engine) Call(ctx context.Context, proc Value, args ...Value) (Value, error) {
	unwrappedArgs := make([]values.Value, len(args))
	for i, arg := range args {
		unwrappedArgs[i] = unwrapValue(arg)
	}

	callee := unwrapValue(proc)
	switch cls := callee.(type) {
	case *machine.MachineClosure:
		return p.callClosure(ctx, cls, unwrappedArgs)

	case *machine.CaseLambdaClosure:
		return p.callCaseLambda(ctx, cls, unwrappedArgs)

	case *machine.Parameter:
		return p.callParameter(ctx, cls, unwrappedArgs)

	case *machine.ComposableContinuation:
		return nil, &RuntimeError{Message: "cannot call composable continuation from Go"}

	default:
		return nil, &RuntimeError{Message: "not a procedure"}
	}
}

func (p *Engine) callClosure(ctx context.Context, cls *machine.MachineClosure, args []values.Value) (Value, error) {
	tpl := machine.NewEmptyNativeTemplate()
	cont := machine.NewMachineContinuation(nil, tpl, p.env)
	mc := machine.NewMachineContext(ctx, cont)

	sub := mc.NewSubContext()
	_, err := sub.Apply(cls, args...)
	if err != nil {
		return nil, p.wrapRuntimeError(err)
	}

	err = sub.Run()
	if err != nil {
		return nil, p.wrapRuntimeError(err)
	}
	return wrapValue(sub.GetValue()), nil
}

func (p *Engine) callCaseLambda(ctx context.Context, cls *machine.CaseLambdaClosure, args []values.Value) (Value, error) {
	tpl := machine.NewEmptyNativeTemplate()
	cont := machine.NewMachineContinuation(nil, tpl, p.env)
	mc := machine.NewMachineContext(ctx, cont)

	sub := mc.NewSubContext()
	_, err := sub.ApplyCaseLambda(cls, args...)
	if err != nil {
		return nil, p.wrapRuntimeError(err)
	}

	err = sub.Run()
	if err != nil {
		return nil, p.wrapRuntimeError(err)
	}
	return wrapValue(sub.GetValue()), nil
}

func (p *Engine) callParameter(ctx context.Context, param *machine.Parameter, args []values.Value) (Value, error) {
	switch len(args) {
	case 0:
		return wrapValue(param.Value()), nil

	case 1:
		newVal := args[0]
		if param.HasConverter() {
			converter := param.Converter()
			converted, err := p.callClosure(ctx, converter, []values.Value{newVal})
			if err != nil {
				return nil, &RuntimeError{Message: "parameter: converter error", Cause: err}
			}
			newVal = unwrapValue(converted)
		}
		param.SetValue(newVal)
		return Void, nil

	default:
		return nil, &RuntimeError{
			Message: fmt.Sprintf("parameter: expected 0 or 1 arguments, got %d", len(args)),
		}
	}
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

func (p *Engine) compileExpr(ctx context.Context, stx syntax.SyntaxValue) (*CompiledCode, error) {
	tpl := machine.NewEmptyNativeTemplate()

	expanded, err := machine.NewExpanderTimeContinuation(p.env).ExpandExpression(ctx, stx)
	if err != nil {
		return nil, &CompilationError{Message: "expansion error", Cause: err}
	}

	cctx := machine.NewCompileTimeCallContext(ctx, false, true)
	err = machine.NewCompiletimeContinuation(tpl, p.env).CompileExpression(cctx, expanded)
	if err != nil {
		return nil, &CompilationError{Message: "compilation error", Cause: err}
	}

	return &CompiledCode{template: tpl, env: p.env}, nil
}

func (p *Engine) runCompiled(ctx context.Context, cc *CompiledCode) (Value, error) {
	cont := machine.NewMachineContinuation(nil, cc.template, cc.env)
	mc := machine.NewMachineContext(ctx, cont)
	err := mc.RunWithEscapeHandling()
	p.lastCounters = mc.Counters()
	if err != nil {
		return nil, p.wrapRuntimeError(err)
	}
	return wrapValue(mc.GetValue()), nil
}

// LastCounters returns the VM performance counters from the most recent
// Run or Eval call. Sub-context counters are not aggregated.
func (p *Engine) LastCounters() machine.VMCounters {
	return p.lastCounters
}

func (p *Engine) wrapRuntimeError(err error) *RuntimeError {
	var ee *machine.ErrExceptionEscape
	if errors.As(err, &ee) {
		re := &RuntimeError{
			Message:   "runtime error",
			Cause:     err,
			Condition: wrapValue(ee.Condition),
		}
		if ee.Source != nil && ee.Source.File != "" {
			re.Source = fmt.Sprintf("%s:%d:%d",
				ee.Source.File,
				ee.Source.Start.Line(),
				ee.Source.Start.Column())
		}
		if len(ee.StackTrace) > 0 {
			re.StackTrace = ee.StackTrace.String()
		}
		return re
	}
	return &RuntimeError{Message: "runtime error", Cause: err}
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

			err = runBootstrapMacroStx(ctx, env, stx)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

// runBootstrapMacroStx expands, compiles, and runs a single syntax value as part of the bootstrap process.
func runBootstrapMacroStx(ctx context.Context, env *environment.EnvironmentFrame, stx syntax.SyntaxValue) error {
	tpl := machine.NewEmptyNativeTemplate()
	expanded, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ctx, stx)
	if err != nil {
		return err
	}

	cctx := machine.NewCompileTimeCallContext(ctx, false, true)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	if err != nil {
		return err
	}

	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(ctx, cont)
	err = mc.Run()
	if err != nil {
		return err
	}
	return nil
}

// LoadPath Stack API
// These methods provide access to the load path stack for tracking files
// currently being loaded. The stack enables relative path resolution during
// load operations.

// WithLoadPath executes fn with absPath pushed onto the load path stack.
// This is the recommended API for embedders - it guarantees balanced push/pop
// via defer even if fn panics or returns an error.
//
// Returns an error if absPath is not an absolute path.
//
// Example:
//
//	err := engine.WithLoadPath("/app/scripts/main.scm", func() error {
//	    _, err := engine.Eval(ctx, "(load \"helper.scm\")") // resolves relative to /app/scripts/
//	    return err
//	})
func (p *Engine) WithLoadPath(absPath string, fn func() error) error {
	err := p.PushLoadPath(absPath)
	if err != nil {
		return err
	}
	defer p.PopLoadPath()
	return fn()
}

// CurrentLoadPath returns the absolute path of the file currently being loaded,
// or empty string if no file is being loaded.
func (p *Engine) CurrentLoadPath() string {
	stack := p.topLevel.LoadPathStack()
	if stack == nil {
		return ""
	}
	return stack.Current()
}

// CurrentLoadDirectory returns the directory of the file currently being loaded,
// or empty string if no file is being loaded.
func (p *Engine) CurrentLoadDirectory() string {
	stack := p.topLevel.LoadPathStack()
	if stack == nil {
		return ""
	}
	return stack.CurrentDir()
}

// PushLoadPath pushes an absolute path onto the load path stack.
// Returns an error if absPath is not absolute.
//
// Advanced embedders who need fine-grained control can use Push/Pop directly,
// but most should use WithLoadPath for automatic cleanup.
func (p *Engine) PushLoadPath(absPath string) error {
	stack := p.topLevel.LoadPathStack()
	if stack == nil {
		return nil
	}
	return stack.Push(absPath)
}

// PopLoadPath removes the top path from the load path stack.
// Does nothing if the stack is empty.
//
// Advanced embedders who need fine-grained control can use Push/Pop directly,
// but most should use WithLoadPath for automatic cleanup.
func (p *Engine) PopLoadPath() {
	stack := p.topLevel.LoadPathStack()
	if stack != nil {
		stack.Pop()
	}
}
