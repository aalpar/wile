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
	"slices"
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/registry/core"
	"github.com/aalpar/wile/values"
)

// ErrEngineClosed is returned when Close is called on an already-closed engine.
var ErrEngineClosed = values.NewStaticError("engine is closed")

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
	closers      []registry.Closeable
	closed       bool
	maxCallDepth uint64
}

// extSnapshot tracks the primitive index range for an extension so it can be
// registered as a synthetic R7RS library after environment setup.
type extSnapshot struct {
	name       string
	startIndex int
	endIndex   int
	namer      registry.LibraryNamer // nil if not implemented
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
		if !cfg.skipCore {
			err := core.AddToRegistry(reg)
			if err != nil {
				return nil, values.WrapForeignErrorWithCause(values.ErrEngineInit, err, "register core primitives")
			}
		}
	}

	// Add any additional extensions, tracking primitive snapshots for library creation
	var extSnapshots []extSnapshot
	for _, ext := range cfg.extensions {
		startIdx := reg.PrimitiveCount()
		err := ext.AddToRegistry(reg)
		if err != nil {
			return nil, values.WrapForeignErrorWithCause(values.ErrEngineInit, err, "register extension %q", ext.Name())
		}
		endIdx := reg.PrimitiveCount()
		var namer registry.LibraryNamer
		n, ok := ext.(registry.LibraryNamer)
		if ok {
			namer = n
		}
		extSnapshots = append(extSnapshots, extSnapshot{
			name:       ext.Name(),
			startIndex: startIdx,
			endIndex:   endIdx,
			namer:      namer,
		})
	}

	// Create TopLevelEnvironment (per-instance symbol interning)
	topLevel := environment.NewTopLevelEnvironment()
	env := topLevel.Runtime()

	// Apply registry, syntax compilers, primitive expanders, and bootstrap macros
	macroSources := reg.MacroSources()
	err := applyBaseEnvironment(ctx, env, reg, macroSources)
	if err != nil {
		return nil, err
	}

	// Set up library system if WithLibraryPaths was called
	if cfg.libraryEnabled {
		libReg := machine.NewLibraryRegistry()

		// Prepend user paths in reverse order so first path has highest priority.
		// AddSearchPath prepends, so reverse-iterating produces the correct order.
		for i := len(cfg.libraryPaths) - 1; i >= 0; i-- {
			libReg.AddSearchPath(cfg.libraryPaths[i])
		}

		if cfg.importObserver != nil {
			libReg.SetImportObserver(cfg.importObserver)
		}

		env.SetLibraryRegistry(libReg)

		err = registerExtensionLibraries(reg, env, libReg, extSnapshots)
		if err != nil {
			return nil, err
		}

		// LibraryEnvFactory creates isolated library environments that mirror
		// this engine's configuration — same registry, same macros.
		topLevel.SetLibraryEnvFactory(func(ctx context.Context, callerEnv *environment.EnvironmentFrame, _ []string) (*environment.EnvironmentFrame, error) {
			callerTopLevel := callerEnv.TopLevelEnv()
			if callerTopLevel == nil {
				return nil, values.WrapForeignErrorf(values.ErrEngineInit, "library env factory: caller has no TopLevelEnvironment")
			}

			libEnv := callerTopLevel.NewChildRuntime()

			applyErr := applyBaseEnvironment(ctx, libEnv, reg, macroSources)
			if applyErr != nil {
				return nil, applyErr
			}

			return libEnv, nil
		})
	}

	// Collect closeable extensions for Engine.Close()
	var closers []registry.Closeable
	for _, ext := range cfg.extensions {
		c, ok := ext.(registry.Closeable)
		if ok {
			closers = append(closers, c)
		}
	}

	q := &Engine{
		topLevel:     topLevel,
		env:          env,
		registry:     reg,
		closers:      closers,
		maxCallDepth: cfg.maxCallDepth,
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

	// Parameter has special handling: 0-arg get doesn't need the VM.
	param, isParam := callee.(*machine.Parameter)
	if isParam {
		return p.callParameter(ctx, param, unwrappedArgs)
	}

	// Composable continuations require the VM winding stack.
	_, isCC := callee.(*machine.ComposableContinuation)
	if isCC {
		return nil, &RuntimeError{Message: "cannot call composable continuation from Go"}
	}

	// Reject non-procedures before entering the VM.
	// Parameter and ComposableContinuation are both Callable but require
	// special handling above — Parameter supports 0-arg get without the VM,
	// and ComposableContinuation needs the winding stack. The general
	// Callable path below handles closures and case-lambdas.
	callable, isCallable := callee.(values.Callable)
	if !isCallable {
		return nil, &RuntimeError{Message: "not a procedure"}
	}

	return p.callCallable(ctx, callable, unwrappedArgs)
}

func (p *Engine) callCallable(ctx context.Context, callable values.Callable, args []values.Value) (Value, error) {
	tpl := machine.NewEmptyNativeTemplate()
	cont := machine.NewMachineContinuation(nil, tpl, p.env)
	mc := machine.NewMachineContext(ctx, cont)
	mc.SetMaxCallDepth(p.maxCallDepth)

	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	_, err := sub.ApplyCallable(callable, args...)
	if err != nil {
		return nil, p.wrapRuntimeError(err)
	}

	err = sub.RunWithEscapeHandling()
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
			converted, err := p.callCallable(ctx, param.Converter(), []values.Value{newVal})
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

// Registry returns a clone of the engine's registry. The returned registry
// can be filtered with Without, WithoutCategory, or WithoutBindings and
// passed to NewEngine via WithRegistry to create a restricted engine.
func (p *Engine) Registry() *registry.Registry {
	return p.registry.Clone()
}

// internal helpers

// registerExtensionLibraries registers each extension as a synthetic R7RS library
// so Scheme code can selectively import extension primitives via (import (wile math)) etc.
func registerExtensionLibraries(reg *registry.Registry, env *environment.EnvironmentFrame, libReg *machine.LibraryRegistry, snapshots []extSnapshot) error {
	for _, snap := range snapshots {
		names := reg.RuntimePrimitiveNamesRange(snap.startIndex, snap.endIndex)
		if len(names) == 0 {
			continue
		}

		var parts []string
		if snap.namer != nil {
			parts = snap.namer.LibraryName()
		} else {
			parts = []string{"wile", snap.name}
		}
		if len(parts) == 0 {
			return values.WrapForeignErrorf(
				values.ErrEngineInit,
				"invalid library name for extension %q: no name parts", snap.name,
			)
		}
		if slices.Contains(parts, "") {
			return values.WrapForeignErrorf(
				values.ErrEngineInit,
				"invalid library name for extension %q: empty name part", snap.name,
			)
		}

		libName := machine.NewLibraryName(parts...)
		lib := machine.NewCompiledLibrary(libName, env)
		for _, name := range names {
			lib.AddExport(name, "")
		}
		regErr := libReg.Register(lib)
		if regErr != nil {
			return values.WrapForeignErrorWithCause(values.ErrEngineInit, regErr, "failed to register extension library")
		}
	}
	return nil
}

// applyBaseEnvironment performs the four-step setup that every usable environment
// requires: apply registry bindings, register syntax compilers, register primitive
// expanders, and load bootstrap macros. Each step wraps errors with ErrEngineInit.
func applyBaseEnvironment(ctx context.Context, env *environment.EnvironmentFrame, reg *registry.Registry, macroSources []string) error {
	err := reg.Apply(ctx, env)
	if err != nil {
		return values.WrapForeignErrorWithCause(values.ErrEngineInit, err, "apply registry")
	}

	err = machine.RegisterSyntaxCompilers(env)
	if err != nil {
		return values.WrapForeignErrorWithCause(values.ErrEngineInit, err, "register syntax compilers")
	}

	err = machine.RegisterPrimitiveExpanders(env)
	if err != nil {
		return values.WrapForeignErrorWithCause(values.ErrEngineInit, err, "register primitive expanders")
	}

	err = loadBootstrapMacros(ctx, env, macroSources)
	if err != nil {
		return values.WrapForeignErrorWithCause(values.ErrEngineInit, err, "load bootstrap macros")
	}
	return nil
}

// expandAndCompile runs the expand → compile → optimize pipeline for a single
// syntax value, returning the resulting template. Callers own error wrapping.
func expandAndCompile(ctx context.Context, env *environment.EnvironmentFrame, stx syntax.SyntaxValue) (*machine.NativeTemplate, error) {
	tpl := machine.NewEmptyNativeTemplate()

	expanded, err := machine.NewExpanderTimeContinuation(ctx, env).ExpandExpression(stx)
	if err != nil {
		return nil, err
	}

	cctx := machine.NewCompileTimeCallContext(ctx, false, true)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	if err != nil {
		return nil, err
	}

	tpl.Optimize()
	return tpl, nil
}

func (p *Engine) compileExpr(ctx context.Context, stx syntax.SyntaxValue) (*CompiledCode, error) {
	tpl, err := expandAndCompile(ctx, p.env, stx)
	if err != nil {
		return nil, &CompilationError{Message: "expand/compile error", Cause: err}
	}
	return &CompiledCode{template: tpl, env: p.env}, nil
}

func (p *Engine) runCompiled(ctx context.Context, cc *CompiledCode) (Value, error) {
	cont := machine.NewMachineContinuation(nil, cc.template, cc.env)
	mc := machine.NewMachineContext(ctx, cont)
	mc.SetMaxCallDepth(p.maxCallDepth)
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
		pr := parser.NewParser(env, true, reader)

		for {
			stx, err := pr.ReadSyntax(ctx)
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
	tpl, err := expandAndCompile(ctx, env, stx)
	if err != nil {
		return err
	}

	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(ctx, cont)
	return mc.Run()
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
// Returns an error if absPath is not absolute. When the library system
// is not enabled (no WithLibraryPaths option), this is a silent no-op.
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

// Close releases resources held by closeable extensions.
// Extensions that implement registry.Closeable have their Close method called.
// Errors from individual closers are collected and returned via errors.Join.
// Calling Close on an already-closed engine returns ErrEngineClosed.
func (p *Engine) Close() error {
	if p.closed {
		return values.WrapForeignErrorf(ErrEngineClosed, "engine: already closed")
	}
	p.closed = true

	var errs []error
	for _, c := range p.closers {
		err := c.Close()
		if err != nil {
			errs = append(errs, err)
		}
	}
	return errors.Join(errs...)
}
