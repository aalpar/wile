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
	"github.com/aalpar/wile/machine/compilation"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/registry/core"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// ErrEngineClosed is returned when Close is called on an already-closed engine.
var ErrEngineClosed = werr.NewStaticError("engine is closed")

// DefaultMaxCallDepth is the default call depth limit for new engines.
// At ~500 bytes per frame, 10000 frames ≈ 5MB. Use WithMaxCallDepth(0)
// to opt out of the limit explicitly.
const DefaultMaxCallDepth uint64 = 10000

// Engine is the main entry point for embedding Wile.
//
// An Engine is NOT safe for concurrent use from multiple goroutines.
// Most methods that parse, compile, or evaluate code mutate the
// environment. Each goroutine should use its own Engine, or
// synchronize externally.
//
// SRFI-18 threads within a single Engine are safe — the VM handles
// thread coordination internally.
type Engine struct {
	namespace       *environment.Namespace
	env             *environment.EnvironmentFrame
	registry        *registry.Registry
	lastCounters    machine.VMCounters
	closers         []registry.Closeable
	closed          bool
	maxCallDepth    uint64
	inlineThreshold int
}

// extSnapshot tracks the primitive index range for an extension so it can be
// registered as a synthetic R7RS library after environment setup.
type extSnapshot struct {
	name       string
	startIndex int
	endIndex   int
	namer      registry.LibraryNamer // nil if not implemented
}

// NewNamespace creates a fully initialized namespace with a registry,
// base environment bindings, syntax compilers, expanders, and bootstrap
// macros. The namespace can be passed to NewEngine via WithNamespace.
//
// Options are shared with NewEngine: WithExtension, WithRegistry,
// WithoutCore, WithAuthorizer all work. Engine-specific options
// (WithMaxCallDepth, WithLibraryPaths, etc.) are accepted but ignored.
//
// Example:
//
//	ns, err := wile.NewNamespace(ctx,
//	    wile.WithExtension(math.Extension),
//	    wile.WithAuthorizer(security.ReadOnly()),
//	)
//	eng, err := wile.NewEngine(ctx, wile.WithNamespace(ns))
func NewNamespace(ctx context.Context, opts ...EngineOption) (*environment.Namespace, error) {
	cfg := &engineConfig{}
	for _, opt := range opts {
		opt(cfg)
	}

	reg, _, _, err := buildRegistry(cfg)
	if err != nil {
		return nil, err
	}

	ns := environment.NewNamespace()
	ns.SetRegistry(reg)
	if cfg.authorizer != nil {
		ns.SetAuthorizer(cfg.authorizer)
	}

	env := ns.Runtime()
	macroSources := reg.MacroSources()
	bootstrapResolver := compilation.NewEmbedFileResolver(core.BootstrapFS)
	err = applyBaseEnvironment(ctx, env, reg, macroSources, bootstrapResolver)
	if err != nil {
		return nil, err
	}

	return ns, nil
}

// NewEngine creates a new Wile engine.
// By default, only core primitives are included.
// Use WithExtension to add optional extensions.
//
// When WithNamespace is used, the engine uses the pre-built namespace
// and ignores registry/extension/core options (they were applied when
// the namespace was created). Library paths and other engine-specific
// options still apply.
//
// # Initialization Order Invariant
//
// NewEngine performs 6 initialization steps that MUST execute in this order.
// Each step depends on prior steps; reordering causes silent failures or panics.
//
//  1. Config         — build engineConfig from options
//  2. Registry       — buildRegistry(cfg): register core + extension primitives
//  3. Namespace      — NewNamespace() + SetRegistry + SetAuthorizer
//  4. Bootstrap      — applyBaseEnvironment: bind primitives, syntax compilers,
//     expanders, bootstrap macros (uses EmbedFileResolver, NOT
//     the runtime file resolver)
//  5. File resolver  — env.SetFileResolver: runtime include/load resolver.
//     Must come AFTER bootstrap (step 4) so bootstrap uses its
//     own EmbedFileResolver, not the runtime resolver.
//  6. Library system — setupLibrarySystem: search paths, extension libraries,
//     library env factory. Requires file resolver (step 5)
//     and bootstrap macros (step 4) for define-library parsing.
//
// The WithNamespace path (pre-built namespace) skips steps 2-5 and trusts that
// the caller bootstrapped correctly. NewNamespace() performs steps 2-4.
func NewEngine(ctx context.Context, opts ...EngineOption) (*Engine, error) {
	cfg := &engineConfig{}
	for _, opt := range opts {
		opt(cfg)
	}

	// Apply default call depth when the caller did not set one explicitly.
	// WithMaxCallDepth(0) means unlimited — callDepthSet tracks whether the
	// caller opted in, so we don't override an explicit zero.
	if !cfg.callDepthSet {
		cfg.maxCallDepth = DefaultMaxCallDepth
	}

	// Apply default inline threshold when the caller did not set one explicitly.
	// WithInlineThreshold(0) disables inlining — inlineThresholdSet tracks
	// whether the caller opted in, so we don't override an explicit zero.
	if !cfg.inlineThresholdSet {
		cfg.inlineThreshold = compilation.DefaultInlineThreshold
	}

	var ns *environment.Namespace
	var reg *registry.Registry
	var snapshots []extSnapshot
	var closers []registry.Closeable

	if cfg.namespace != nil {
		// Use pre-built namespace
		ns = cfg.namespace
		regAny := ns.Registry()
		if regAny == nil {
			return nil, werr.WrapForeignErrorf(werr.ErrEngineInit,
				"WithNamespace: namespace has no registry — use wile.NewNamespace() or SetRegistry()")
		}
		var ok bool
		reg, ok = regAny.(*registry.Registry)
		if !ok {
			return nil, werr.WrapForeignErrorf(werr.ErrEngineInit,
				"WithNamespace: namespace registry is %T, expected *registry.Registry", regAny)
		}
	} else {
		// Build namespace from engine options (backward compat)
		var err error
		reg, snapshots, closers, err = buildRegistry(cfg)
		if err != nil {
			return nil, err
		}

		ns = environment.NewNamespace()
		ns.SetRegistry(reg)
		if cfg.authorizer != nil {
			ns.SetAuthorizer(cfg.authorizer)
		}

		env := ns.Runtime()
		macroSources := reg.MacroSources()
		bootstrapResolver := compilation.NewEmbedFileResolver(core.BootstrapFS)
		err = applyBaseEnvironment(ctx, env, reg, macroSources, bootstrapResolver)
		if err != nil {
			return nil, err
		}

		// Set the default file resolver for runtime include/load operations.
		// This must happen after bootstrap (which uses EmbedFileResolver).
		env.SetFileResolver(newFileResolver(cfg, env))
	}

	env := ns.Runtime()

	if cfg.libraryEnabled {
		// File resolver must be set before library loading
		if env.FileResolver() == nil {
			env.SetFileResolver(newFileResolver(cfg, env))
		}
		macroSources := reg.MacroSources()
		bootstrapResolver := compilation.NewEmbedFileResolver(core.BootstrapFS)
		err := setupLibrarySystem(cfg, reg, env, ns, macroSources, snapshots, bootstrapResolver)
		if err != nil {
			return nil, err
		}
	}

	q := &Engine{
		namespace:       ns,
		env:             env,
		registry:        reg,
		closers:         closers,
		maxCallDepth:    cfg.maxCallDepth,
		inlineThreshold: cfg.inlineThreshold,
	}
	return q, nil
}

// buildRegistry creates and populates the registry from engine configuration.
// It registers extensions (tracking primitive index ranges for library creation)
// and collects closeable extensions for Engine.Close().
func buildRegistry(cfg *engineConfig) (*registry.Registry, []extSnapshot, []registry.Closeable, error) {
	reg := cfg.registry
	if reg == nil {
		reg = registry.NewRegistry()
		err := core.AddToRegistry(reg)
		if err != nil {
			return nil, nil, nil, werr.WrapForeignErrorWithCause(werr.ErrEngineInit, err, "register core primitives")
		}
	}

	var snapshots []extSnapshot
	var closers []registry.Closeable
	for _, ext := range cfg.extensions {
		startIdx := reg.PrimitiveCount()
		err := ext.AddToRegistry(reg)
		if err != nil {
			return nil, nil, nil, werr.WrapForeignErrorWithCause(werr.ErrEngineInit, err, "register extension %q", ext.Name())
		}
		endIdx := reg.PrimitiveCount()

		var namer registry.LibraryNamer
		n, ok := ext.(registry.LibraryNamer)
		if ok {
			namer = n
		}
		snapshots = append(snapshots, extSnapshot{
			name:       ext.Name(),
			startIndex: startIdx,
			endIndex:   endIdx,
			namer:      namer,
		})

		c, ok := ext.(registry.Closeable)
		if ok {
			closers = append(closers, c)
		}
	}

	return reg, snapshots, closers, nil
}

// setupLibrarySystem configures the R7RS library system: search paths,
// import observer, extension libraries, and the library environment factory.
func setupLibrarySystem(cfg *engineConfig, reg *registry.Registry, env *environment.EnvironmentFrame, ns *environment.Namespace, macroSources []string, snapshots []extSnapshot, bootstrapResolver compilation.FileResolver) error {
	libReg := compilation.NewLibraryRegistry()

	// Prepend user paths in reverse order so first path has highest priority.
	// PrependSearchPath prepends, so reverse-iterating produces the correct order.
	for i := len(cfg.libraryPaths) - 1; i >= 0; i-- {
		libReg.PrependSearchPath(cfg.libraryPaths[i])
	}

	if cfg.importObserver != nil {
		libReg.SetImportObserver(cfg.importObserver)
	}

	env.SetLibraryRegistry(libReg)

	err := registerExtensionLibraries(reg, env, libReg, snapshots)
	if err != nil {
		return err
	}

	// LibraryEnvFactory creates isolated library environments that mirror
	// this engine's configuration — same registry, same macros.
	ns.SetLibraryEnvFactory(func(ctx context.Context, callerEnv *environment.EnvironmentFrame, _ []string) (*environment.EnvironmentFrame, error) {
		callerTopLevel := callerEnv.Namespace()
		if callerTopLevel == nil {
			return nil, werr.WrapForeignErrorf(werr.ErrEngineInit, "library env factory: caller has no Namespace")
		}

		libEnv := callerTopLevel.NewChildRuntime()

		applyErr := applyBaseEnvironment(ctx, libEnv, reg, macroSources, bootstrapResolver)
		if applyErr != nil {
			return nil, applyErr
		}

		return libEnv, nil
	})

	return nil
}

// Eval compiles and executes a parsed expression, returning the result.
// Use [Engine.Parse] to obtain an [Expression] from source code.
// For evaluating multi-expression strings, use [Engine.EvalMultiple].
func (p *Engine) Eval(ctx context.Context, expr *Expression) (Value, error) {
	compiled, err := p.Compile(ctx, expr)
	if err != nil {
		return nil, err
	}
	return p.Run(ctx, compiled)
}

// EvalIn compiles and executes a parsed expression in the given namespace,
// rather than the engine's own namespace.
//
// The target namespace's authorizer governs security checks during
// execution. If the target namespace has no authorizer, the engine's
// authorizer is propagated to it before evaluation.
func (p *Engine) EvalIn(ctx context.Context, expr *Expression, ns *environment.Namespace) (Value, error) {
	if ns.Authorizer() == nil && p.namespace.Authorizer() != nil {
		ns.SetAuthorizer(p.namespace.Authorizer())
	}
	env := ns.Runtime()

	tpl, err := expandAndCompile(ctx, env, expr.stx, nil, p.inlineThreshold)
	if err != nil {
		return nil, &CompilationError{Message: "expand/compile error", Cause: err}
	}

	cc := &CompiledCode{template: tpl, env: env}
	return p.runCompiled(ctx, cc)
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

// Compile compiles a parsed expression without executing.
// The result can be executed later with [Engine.Run].
func (p *Engine) Compile(ctx context.Context, expr *Expression) (*CompiledCode, error) {
	return p.compileExpr(ctx, expr.stx)
}

// Run executes previously compiled code.
func (p *Engine) Run(ctx context.Context, cc *CompiledCode) (Value, error) {
	return p.runCompiled(ctx, cc)
}

// Define binds a value to a name in the top-level environment.
func (p *Engine) Define(name string, value Value) error {
	sym := values.NewSymbol(name)
	p.env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
	return p.env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), unwrapValue(value))
}

// Get retrieves a value by name from the environment.
func (p *Engine) Get(name string) (Value, bool) {
	sym := values.NewSymbol(name)
	idx := environment.NewGlobalIndex(sym)
	binding := p.env.GetGlobalBinding(idx)
	if binding == nil {
		return nil, false
	}
	return wrapValue(binding.Value()), true
}

// RegisterPrimitive adds a Go function as a Scheme primitive.
func (p *Engine) RegisterPrimitive(spec PrimitiveSpec) error {
	sym := values.NewSymbol(spec.Name)
	p.env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)

	closure := machine.NewForeignClosure(
		p.env,
		spec.ParamCount,
		spec.IsVariadic,
		spec.Impl,
	)
	closure.SetName(spec.Name)
	closure.SetDoc(spec.Doc)

	return p.env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure)
}

// Call invokes a Scheme procedure with arguments.
// Supports lambdas, foreign closures, case-lambdas, and parameters.
// Composable continuations cannot be called from Go
// (they require the VM winding stack) and return an error.
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
		return nil, newRuntimeError("cannot call composable continuation from Go")
	}

	// Reject non-procedures before entering the VM.
	// Parameter and ComposableContinuation are both Callable but require
	// special handling above — Parameter supports 0-arg get without the VM,
	// and ComposableContinuation needs the winding stack. The general
	// Callable path below handles closures and case-lambdas.
	callable, isCallable := callee.(values.Callable)
	if !isCallable {
		return nil, newRuntimeError("not a procedure")
	}

	return p.callCallable(ctx, callable, unwrappedArgs)
}

// callCallable spins up a sub-context to apply a general Callable (closure,
// case-lambda) and returns the result. Used by Call and callParameter.
func (p *Engine) callCallable(ctx context.Context, callable values.Callable, args []values.Value) (Value, error) {
	// Build a throwaway top-level context with an empty template — the
	// real work happens in the sub-context, but the VM needs a root.
	tpl := machine.NewEmptyNativeTemplate()
	mc := machine.AcquireTopLevelContext(ctx, tpl, p.env)
	mc.SetMaxCallDepth(p.maxCallDepth)

	// Create a sub-context, set up the call frame, and execute.
	sub := mc.NewSubContext()
	defer machine.ReleaseTopLevelContext(mc)
	defer machine.ReleaseSubContext(sub)
	_, err := sub.ApplyCallable(callable, args...)
	if err != nil {
		return nil, p.wrapRuntimeError(err)
	}

	// Run the VM loop; escape continuations are caught and converted.
	err = sub.RunWithEscapeHandling()
	if err != nil {
		return nil, p.wrapRuntimeError(err)
	}
	return wrapValue(sub.GetValue()), nil
}

// callParameter handles Parameter invocation from Go: zero args returns the
// current value, one arg sets it (running the converter if present).
func (p *Engine) callParameter(ctx context.Context, param *machine.Parameter, args []values.Value) (Value, error) {
	switch len(args) {
	case 0:
		// (param) — read the current value.
		return wrapValue(param.Value()), nil

	case 1:
		// (param val) — set the value, running the converter first if one exists.
		newVal := args[0]
		if param.HasConverter() {
			converted, err := p.callCallable(ctx, param.Converter(), []values.Value{newVal})
			if err != nil {
				return nil, newRuntimeErrorWithCause("parameter: converter error", err)
			}
			newVal = unwrapValue(converted)
		}
		param.SetValue(newVal)
		return Void, nil

	default:
		return nil, newRuntimeError(fmt.Sprintf("parameter: expected 0 or 1 arguments, got %d", len(args)))
	}
}

// Environment returns the underlying environment for advanced use.
func (p *Engine) Environment() *environment.EnvironmentFrame {
	return p.env
}

// Namespace returns the Namespace for advanced use.
// This provides access to per-instance symbol interning and phase management.
func (p *Engine) Namespace() *environment.Namespace {
	return p.namespace
}

// Registry returns a clone of the engine's registry. The returned registry
// can be filtered with Without, WithoutCategory, or WithoutBindings and
// passed to NewEngine via WithRegistry to create a restricted engine.
func (p *Engine) Registry() *registry.Registry {
	return p.registry.Clone()
}

// AvailableLibraries returns all importable library names by combining
// filesystem discovery with registry-known libraries (synthetic extensions).
// Returns a sorted, deduplicated list. If the library system is not enabled
// (no WithLibraryPaths call), returns an empty list.
func (p *Engine) AvailableLibraries(ctx context.Context) ([]compilation.LibraryName, error) {
	_ = ctx // reserved for future cancellation support

	regSearcher := p.env.LibraryRegistry()
	if regSearcher == nil {
		return nil, nil
	}
	reg, ok := regSearcher.(*compilation.LibraryRegistry)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryConfiguration,
			"AvailableLibraries: library registry has unexpected type %T", regSearcher)
	}

	return compilation.DiscoverAvailableLibraries(p.env.FileResolver(), reg)
}

// internal helpers

// registerExtensionLibraries registers each extension as a synthetic R7RS library
// so Scheme code can selectively import extension primitives via (import (wile math)) etc.
func registerExtensionLibraries(reg *registry.Registry, env *environment.EnvironmentFrame, libReg *compilation.LibraryRegistry, snapshots []extSnapshot) error {
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
			return werr.WrapForeignErrorf(
				werr.ErrEngineInit,
				"invalid library name for extension %q: no name parts", snap.name,
			)
		}
		if slices.Contains(parts, "") {
			return werr.WrapForeignErrorf(
				werr.ErrEngineInit,
				"invalid library name for extension %q: empty name part", snap.name,
			)
		}

		libName := compilation.NewLibraryName(parts...)
		lib := compilation.NewCompiledLibrary(libName, env)
		for _, name := range names {
			lib.AddExport(name, "")
		}
		regErr := libReg.Register(lib)
		if regErr != nil {
			return werr.WrapForeignErrorWithCause(werr.ErrEngineInit, regErr, "failed to register extension library")
		}
	}
	return nil
}

// applyBaseEnvironment performs the five-step setup that every usable environment
// requires: apply registry bindings, register syntax compilers, register primitive
// expanders, load bootstrap macros, and inject documentation into bindings.
// Each step wraps errors with ErrEngineInit. The resolver controls how include/load
// finds files during bootstrap; pass nil for OS filesystem defaults.
func applyBaseEnvironment(ctx context.Context, env *environment.EnvironmentFrame, reg *registry.Registry, macroSources []string, resolver compilation.FileResolver) error {
	err := reg.Apply(ctx, env)
	if err != nil {
		return werr.WrapForeignErrorWithCause(werr.ErrEngineInit, err, "apply registry")
	}

	err = compilation.RegisterAllPhaseHandlers(env)
	if err != nil {
		return werr.WrapForeignErrorWithCause(werr.ErrEngineInit, err, "register phase handlers")
	}

	err = loadBootstrapMacros(ctx, env, macroSources, resolver)
	if err != nil {
		return werr.WrapForeignErrorWithCause(werr.ErrEngineInit, err, "load bootstrap macros")
	}

	// Inject documentation into bootstrap macro bindings (expand-time).
	// Must run after loadBootstrapMacros so define-syntax bindings exist.
	reg.ApplyDocs(env)

	return nil
}

// expandAndCompile runs the expand → compile → optimize pipeline for a single
// syntax value, returning the resulting template. An optional FileResolver
// overrides how include/load finds files (nil uses the OS filesystem default).
// inlineThreshold controls procedure inlining: procedures with bodies longer
// than this threshold are not inlined; 0 disables inlining entirely.
// Callers own error wrapping.
func expandAndCompile(ctx context.Context, env *environment.EnvironmentFrame, stx syntax.SyntaxValue, resolver compilation.FileResolver, inlineThreshold int) (*machine.NativeTemplate, error) {
	tpl := machine.NewEmptyNativeTemplate()

	expanded, err := compilation.NewExpanderTimeContinuation(ctx, env, machine.NewVMMacroEvaluator()).ExpandExpression(stx)
	if err != nil {
		return nil, err
	}

	cctx := compilation.NewCompileTimeCallContext(ctx, false)
	compiler := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	if resolver != nil {
		compiler.SetFileResolver(resolver)
	}
	compiler.SetInlineThreshold(inlineThreshold)
	err = compiler.CompileExpression(cctx, expanded)
	if err != nil {
		return nil, err
	}

	tpl.Optimize()
	return tpl, nil
}

func (p *Engine) compileExpr(ctx context.Context, stx syntax.SyntaxValue) (*CompiledCode, error) {
	tpl, err := expandAndCompile(ctx, p.env, stx, nil, p.inlineThreshold)
	if err != nil {
		return nil, &CompilationError{Message: "expand/compile error", Cause: err}
	}
	return &CompiledCode{template: tpl, env: p.env}, nil
}

func (p *Engine) runCompiled(ctx context.Context, cc *CompiledCode) (Value, error) {
	mc := machine.AcquireTopLevelContext(ctx, cc.template, cc.env)
	defer machine.ReleaseTopLevelContext(mc)
	mc.SetMaxCallDepth(p.maxCallDepth)

	err := mc.RunWithEscapeHandling()
	p.lastCounters = mc.Counters()
	val := mc.GetValue()
	if err != nil {
		return nil, p.wrapRuntimeError(err)
	}
	return wrapValue(val), nil
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
	return newRuntimeErrorWithCause("runtime error", err)
}

// newFileResolver creates the appropriate FileResolver based on engine config.
// When no resolver factories are configured, defaults to OSFileResolver.
// A single factory returns its resolver directly; multiple factories
// produce a ChainFileResolver that tries each in order.
func newFileResolver(cfg *engineConfig, env *environment.EnvironmentFrame) compilation.FileResolver {
	if len(cfg.resolverFactories) == 0 {
		return compilation.NewOSFileResolver(env)
	}
	if len(cfg.resolverFactories) == 1 {
		return cfg.resolverFactories[0](env)
	}
	resolvers := make([]compilation.FileResolver, len(cfg.resolverFactories))
	for i, f := range cfg.resolverFactories {
		resolvers[i] = f(env)
	}
	return compilation.NewChainFileResolver(resolvers)
}

func loadBootstrapMacros(ctx context.Context, env *environment.EnvironmentFrame, sources []string, resolver compilation.FileResolver) error {
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

			err = runBootstrapMacroStx(ctx, env, stx, resolver)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

// runBootstrapMacroStx expands, compiles, and runs a single syntax value as part of the bootstrap process.
func runBootstrapMacroStx(ctx context.Context, env *environment.EnvironmentFrame, stx syntax.SyntaxValue, resolver compilation.FileResolver) error {
	tpl, err := expandAndCompile(ctx, env, stx, resolver, compilation.DefaultInlineThreshold)
	if err != nil {
		return err
	}

	mc := machine.AcquireTopLevelContext(ctx, tpl, env)
	defer machine.ReleaseTopLevelContext(mc)
	return mc.Run()
}

// LoadPath Stack API
// These methods provide access to the load path stack for tracking files
// currently being loaded. The stack enables relative path resolution during
// load operations.

// WithLoadPath executes fn with filePath pushed onto the load path stack.
// This is the recommended API for embedders — it guarantees balanced push/pop
// via defer even if fn panics or returns an error.
//
// Returns an error if filePath is empty.
//
// Example:
//
//	err := engine.WithLoadPath("/app/scripts/main.scm", func() error {
//	    _, err := engine.EvalMultiple(ctx, "(load \"helper.scm\")") // resolves relative to /app/scripts/
//	    return err
//	})
func (p *Engine) WithLoadPath(filePath string, fn func() error) error {
	err := p.PushLoadPath(filePath)
	if err != nil {
		return err
	}
	defer p.PopLoadPath()
	return fn()
}

// CurrentLoadPath returns the path of the file currently being loaded,
// or empty string if no file is being loaded.
func (p *Engine) CurrentLoadPath() string {
	stack := p.namespace.LoadPathStack()
	if stack == nil {
		return ""
	}
	return stack.Current()
}

// CurrentLoadDirectory returns the directory of the file currently being loaded,
// or empty string if no file is being loaded.
func (p *Engine) CurrentLoadDirectory() string {
	stack := p.namespace.LoadPathStack()
	if stack == nil {
		return ""
	}
	return stack.CurrentDir()
}

// PushLoadPath pushes a path onto the load path stack.
// Returns an error if the path is empty.
//
// Advanced embedders who need fine-grained control can use Push/Pop directly,
// but most should use WithLoadPath for automatic cleanup.
func (p *Engine) PushLoadPath(filePath string) error {
	stack := p.namespace.LoadPathStack()
	if stack == nil {
		return nil
	}
	return stack.Push(filePath)
}

// PopLoadPath removes the top path from the load path stack.
// Does nothing if the stack is empty.
//
// Advanced embedders who need fine-grained control can use Push/Pop directly,
// but most should use WithLoadPath for automatic cleanup.
func (p *Engine) PopLoadPath() {
	stack := p.namespace.LoadPathStack()
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
		return werr.WrapForeignErrorf(ErrEngineClosed, "engine: already closed")
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
