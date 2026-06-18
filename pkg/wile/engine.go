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
	"sync"

	"github.com/aalpar/wile/coverage"
	"github.com/aalpar/wile/pkg/docparse"
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/machine/compilation/sourceload"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/registry/core"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// ErrEngineClosed is returned when Close is called on an already-closed engine.
var ErrEngineClosed = werr.NewStaticError("engine is closed")

// DefaultMaxCallDepth is the default call depth limit for new engines.
// At ~500 bytes per frame, 10000 frames ≈ 5MB. Use WithMaxCallDepth(0)
// to opt out of the limit explicitly; WithMaxCallDepth(n) with n < 0 is
// clamped to 0 (also unlimited).
const DefaultMaxCallDepth int = 10000

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
	namespace               *environment.Namespace
	env                     *environment.EnvironmentFrame
	registry                *registry.Registry
	debugger                *Debugger
	lastCounters            machine.VMCounters
	closers                 []registry.Closeable
	closed                  bool
	maxCallDepth            int
	maxParseDepth           int
	maxExpandDepth          int
	maxStackSize            uint64
	inlineThreshold         int
	contractEnforcement     bool // propagated to RegisterPrimitive via cfg
	lossyConversionsAllowed bool // captured into FFI closures at RegisterFunc time
	coverageCollector       *coverage.Collector

	exportIndexMu    sync.Mutex
	exportIndexBuilt bool
	exportIndex      *compilation.LibraryExportIndex
}

// extSnapshot tracks the primitive index range for an extension so it can be
// registered as a synthetic R7RS library after environment setup.
//
// namer and describer are populated whenever the extension implements the
// optional interface; *ExtensionFunc always does. Empty results (nil/empty
// library-name slice, empty description string) are interpreted uniformly
// as "not set" by registerExtensionLibraries — there is no distinction
// between "did not implement" and "implemented but returned the zero value."
type extSnapshot struct {
	name       string
	startIndex int
	endIndex   int
	namer      registry.LibraryNamer
	describer  registry.Describer
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
	cfg := newEngineConfig()
	for _, opt := range opts {
		opt(cfg)
	}
	ns, _, _, err := bootstrapNamespace(ctx, cfg)
	if err != nil {
		return nil, err
	}
	return ns, nil
}

// bootstrapNamespace creates a new namespace from engine config: builds the
// registry, creates the namespace, binds primitives, and loads bootstrap macros.
// Returns the snapshots and closers from buildRegistry for callers that need them
// (NewEngine uses snapshots for extension library registration and closers for
// Engine.Close).
func bootstrapNamespace(ctx context.Context, cfg *engineConfig) (*environment.Namespace, []extSnapshot, []registry.Closeable, error) {
	reg, snapshots, closers, err := buildRegistry(cfg)
	if err != nil {
		return nil, nil, nil, err
	}

	ns := environment.NewNamespace()
	ns.SetRegistry(reg)
	ns.SetLoadPathStack(sourceload.NewLoadStack())
	auth := cfg.resolveAuthorizer()
	if auth != nil {
		ns.SetAuthorizer(auth)
	}
	if cfg.envMap != nil {
		ns.SetEnvMap(cfg.envMap)
	}
	if cfg.immutableTopLevel {
		ns.SetImmutableTopLevel(true)
	}

	env := ns.Runtime()
	err = applyBaseEnvironment(ctx, env, reg, applyOptionsFromConfig(cfg)...)
	if err != nil {
		return nil, nil, nil, err
	}

	return ns, snapshots, closers, nil
}

// applyOptionsFromConfig translates engineConfig into the ApplyOption slice
// consumed by registry.Apply. Centralizing this keeps new enforcement-style
// toggles in one place instead of scattered across applyBaseEnvironment
// call sites (initial bootstrap and the library env factory).
func applyOptionsFromConfig(cfg *engineConfig) []registry.ApplyOption {
	var opts []registry.ApplyOption
	if cfg.contractEnforcement {
		opts = append(opts, registry.WithContractEnforcement())
	}
	// Under top-level immutability (the default), ambient base primitives are also
	// stamped Stable so the frame-reclaim classifier trusts calls to them without an
	// explicit (import (scheme base)). Threads to both the bootstrap env and the library
	// env factory via every applyOptionsFromConfig call site.
	if cfg.immutableTopLevel {
		opts = append(opts, registry.WithStableBasePrimitives())
	}
	return opts
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
	cfg := newEngineConfig()
	for _, opt := range opts {
		opt(cfg)
	}

	// Apply default call depth when the caller did not set one explicitly.
	// WithMaxCallDepth(0) means unlimited — callDepthSet tracks whether the
	// caller opted in, so we don't override an explicit zero.
	if !cfg.callDepthSet {
		cfg.maxCallDepth = DefaultMaxCallDepth
	}

	// Apply default parse depth when the caller did not set one explicitly.
	// WithMaxParseDepth(0) means unlimited — parseDepthSet tracks the opt-in.
	if !cfg.parseDepthSet {
		cfg.maxParseDepth = parser.DefaultMaxParseDepth
	}

	// Apply default expand depth when the caller did not set one explicitly.
	// WithMaxExpandDepth(0) means unlimited — expandDepthSet tracks the opt-in.
	if !cfg.expandDepthSet {
		cfg.maxExpandDepth = compilation.DefaultMaxExpandDepth
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
		var err error
		ns, snapshots, closers, err = bootstrapNamespace(ctx, cfg)
		if err != nil {
			return nil, err
		}
		reg = ns.Registry().(*registry.Registry)
	}

	env := ns.Runtime()

	// Set the default file resolver for runtime include/load operations.
	// This must happen after bootstrap (which uses EmbedFileResolver).
	// Pre-built namespaces (WithNamespace) may already have a resolver.
	if env.FileResolver() == nil {
		env.SetFileResolver(newFileResolver(cfg.resolverFactories, env))
	}

	if cfg.libraryEnabled {
		err := setupLibrarySystem(cfg.libraryPaths, cfg.importObserver, reg, env, ns, snapshots, applyOptionsFromConfig(cfg))
		if err != nil {
			return nil, err
		}
	}

	q := &Engine{
		namespace:               ns,
		env:                     env,
		registry:                reg,
		closers:                 closers,
		maxCallDepth:            cfg.maxCallDepth,
		maxParseDepth:           cfg.maxParseDepth,
		maxExpandDepth:          cfg.maxExpandDepth,
		maxStackSize:            cfg.maxStackSize,
		inlineThreshold:         cfg.inlineThreshold,
		contractEnforcement:     cfg.contractEnforcement,
		lossyConversionsAllowed: cfg.lossyConversionsAllowed,
		coverageCollector:       cfg.coverageCollector,
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

		namer, _ := ext.(registry.LibraryNamer)
		describer, _ := ext.(registry.Describer)
		snapshots = append(snapshots, extSnapshot{
			name:       ext.Name(),
			startIndex: startIdx,
			endIndex:   endIdx,
			namer:      namer,
			describer:  describer,
		})
		closer, ok := ext.(registry.Closeable)
		if ok {
			closers = append(closers, closer)
		}
	}

	return reg, snapshots, closers, nil
}

// setupLibrarySystem configures the R7RS library system: search paths,
// import observer, extension libraries, and the library environment factory.
// applyOpts propagates registry.Apply toggles (e.g., contract enforcement)
// into child library environments so they mirror the parent's configuration.
func setupLibrarySystem(libraryPaths []string, importObserver func(LibraryImportEvent), reg *registry.Registry, env *environment.EnvironmentFrame, ns *environment.Namespace, snapshots []extSnapshot, applyOpts []registry.ApplyOption) error {
	libReg := compilation.NewLibraryRegistry()

	// Prepend user paths in reverse order so first path has highest priority.
	// PrependSearchPath prepends, so reverse-iterating produces the correct order.
	for _, p := range slices.Backward(libraryPaths) {
		libReg.PrependSearchPath(p)
	}

	// Register docstrings from imported libraries so that `,topic` and
	// `,apropos` reflect Scheme-defined procedures as they become visible.
	docObserver := makeDocRegistrationObserver(libReg, reg)
	if importObserver != nil {
		libReg.SetImportObserver(func(evt compilation.LibraryImportEvent) {
			docObserver(evt)
			importObserver(evt)
		})
	} else {
		libReg.SetImportObserver(docObserver)
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

		applyErr := applyBaseEnvironment(ctx, libEnv, reg, applyOpts...)
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

	tpl, err := expandAndCompileOptimized(ctx, env, expr.stx, nil, p.inlineThreshold, p.maxExpandDepth)
	if err != nil {
		return nil, wrapCompilationError("expand/compile error", err)
	}
	trackTemplateTree(p.coverageCollector, tpl)

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
//
// Each top-level form is compiled and run independently, so a forward reference
// between two separate defines — (define (f) (g)) before (define (g) ...) —
// fails to compile. Use [Engine.EvalProgram] for whole-program/file semantics
// where all top-level defines are mutually visible.
func (p *Engine) EvalMultipleWithSource(ctx context.Context, code string, source string) (Value, error) {
	return p.evalMultiple(ctx, code, source)
}

// EvalProgram evaluates code as a single compilation unit: it parses every
// top-level form, splices them into one (begin form ...), and compiles that as a
// unit so all top-level defines are mutually visible — a (define (f) (g)) may
// precede (define (g) ...). This is the forward-reference behavior of loading a
// file, and the recommended entry point for evaluating a whole program or script.
// source labels the code in diagnostics; pass "" if none.
//
// It contrasts with [Engine.EvalMultiple], which compiles and runs each top-level
// form independently. The (begin ...) wrapper is built structurally rather than by
// concatenating source text, so every form keeps its own source location.
func (p *Engine) EvalProgram(ctx context.Context, code string, source string) (Value, error) {
	pr := parser.NewParserWithFile(p.env, true, strings.NewReader(code), source)
	pr.SetMaxDepth(p.maxParseDepth)

	var forms []syntax.SyntaxValue
	for {
		stx, err := pr.ReadSyntax(ctx)
		if err != nil {
			if isEOF(err) {
				break
			}
			return nil, wrapCompilationError("parse error", err)
		}
		forms = append(forms, stx)
	}
	if len(forms) == 0 {
		// No forms (empty input, or only whitespace/comments) — like (begin).
		return Void, nil
	}

	compiled, err := p.compileExpr(ctx, wrapInBegin(forms))
	if err != nil {
		return nil, err
	}
	return p.runCompiled(ctx, compiled)
}

// wrapInBegin splices forms into a single top-level (begin form ...) syntax
// structure — the same shape the expander builds for bodies (see
// machine/compilation/expander_lambda.go). Building the wrapper structurally,
// rather than concatenating "(begin " onto source text, keeps each form's real
// source location and avoids the trailing-line-comment and line-shift hazards of
// string surgery. begin is dispatched by name, so the synthetic head (borrowing
// the first form's source context) resolves to the special form.
func wrapInBegin(forms []syntax.SyntaxValue) syntax.SyntaxValue {
	srcCtx := forms[0].SourceContext()
	beginSym := syntax.NewSyntaxSymbol("begin", srcCtx)
	return syntax.NewSyntaxCons(beginSym, syntax.SyntaxList(srcCtx, forms...), srcCtx)
}

func (p *Engine) evalMultiple(ctx context.Context, code string, source string) (Value, error) {
	reader := strings.NewReader(code)
	pr := parser.NewParserWithFile(p.env, true, reader, source)
	pr.SetMaxDepth(p.maxParseDepth)

	var lastResult = Void
	for {
		stx, err := pr.ReadSyntax(ctx)
		if err != nil {
			if isEOF(err) {
				break
			}
			return nil, wrapCompilationError("parse error", err)
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
	if p.contractEnforcement {
		closure.SetValidator(registry.BuildValidator(spec))
	}

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
		return nil, newRuntimeErrorWithCause("cannot call composable continuation from Go", werr.ErrComposableContinuationFromGo)
	}

	// Reject non-procedures before entering the VM.
	// Parameter and ComposableContinuation are both Callable but require
	// special handling above — Parameter supports 0-arg get without the VM,
	// and ComposableContinuation needs the winding stack. The general
	// Callable path below handles closures and case-lambdas.
	callable, isCallable := callee.(values.Callable)
	if !isCallable {
		return nil, newRuntimeErrorWithCause("not a procedure", werr.ErrNotAProcedure)
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
	mc.SetMaxStackSize(p.maxStackSize)

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
		return nil, newRuntimeErrorWithCause(fmt.Sprintf("parameter: expected 0 or 1 arguments, got %d", len(args)), werr.ErrWrongNumberOfArguments)
	}
}

// Environment returns the underlying environment frame.
//
// This is an advanced escape hatch: it exposes the internal
// environment.EnvironmentFrame type for white-box embedders that need direct
// access to phase frames, the namespace, or the sealed base. That type is
// internal and may change between minor versions, so it is not part of the
// stable API surface. Prefer the typed Engine methods (Get, Define,
// LoadedLibraries, …) where they suffice.
func (p *Engine) Environment() *environment.EnvironmentFrame {
	return p.env
}

// Namespace returns the Namespace for advanced use.
// This provides access to per-instance symbol interning and phase management.
//
// Like Environment, this is an advanced escape hatch exposing an internal type
// (environment.Namespace) that may change between minor versions; it is not
// part of the stable API surface.
func (p *Engine) Namespace() *environment.Namespace {
	return p.namespace
}

// BoundNames returns a sorted, deduplicated list of every binding name visible
// in the engine across all phases (runtime, expand, compile) and the sealed
// base. It includes macro and special-form keywords, not only runtime value
// bindings, so it is broader than the (environment-bound-names) primitive — it
// is the set a REPL wants for tab completion. Returns nil if the engine has no
// namespace.
//
// This is the stable, typed alternative to walking Environment().Namespace()
// phase frames directly.
func (p *Engine) BoundNames() []string {
	if p.namespace == nil {
		return nil
	}
	return p.namespace.BoundNamesAcrossPhases()
}

// SetDebugger attaches a debugger to the engine. Subsequent [Engine.Run]
// calls will execute with the debugger active, enabling breakpoints and
// stepping. Pass nil to detach the debugger.
func (p *Engine) SetDebugger(d *Debugger) {
	p.debugger = d
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
func (p *Engine) AvailableLibraries(ctx context.Context) ([]LibraryName, error) {
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

	internal, err := compilation.DiscoverAvailableLibraries(p.env.FileResolver(), reg)
	if err != nil {
		return nil, err
	}
	q := make([]LibraryName, len(internal))
	for i, name := range internal {
		q[i] = LibraryName{Parts: append([]string(nil), name.Parts...)}
	}
	return q, nil
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
		}
		if len(parts) == 0 {
			parts = []string{"wile", snap.name}
		}
		if slices.Contains(parts, "") {
			return werr.WrapForeignErrorf(
				werr.ErrEngineInit,
				"invalid library name for extension %q: empty name part", snap.name,
			)
		}

		libName := compilation.NewLibraryName(parts...)
		lib := compilation.NewCompiledLibrary(libName, env)
		if snap.describer != nil && snap.describer.Description() != "" {
			lib.Description = snap.describer.Description()
		}
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
// Each step wraps errors with ErrEngineInit. Additional registry.ApplyOption
// values are forwarded to the registry Apply call.
func applyBaseEnvironment(ctx context.Context, env *environment.EnvironmentFrame, reg *registry.Registry, opts ...registry.ApplyOption) error {
	// Runtime primitives + bootstrap procedures are routed to the sealed-base frame for
	// a namespace-owning runtime env (engine root / profile child); a flat library env
	// receives them in its own frame. SealedBaseTarget() picks the right frame for each
	// case, keeping the carve decision in one place. WithRuntimeTarget(self) is a no-op,
	// so the library path is unchanged. A fresh slice avoids racing the shared opts
	// backing array across concurrent library-env creation.
	runtimeTarget := env.SealedBaseTarget()
	applyOpts := make([]registry.ApplyOption, 0, len(opts)+1)
	applyOpts = append(applyOpts, opts...)
	applyOpts = append(applyOpts, registry.WithRuntimeTarget(runtimeTarget))

	err := reg.Apply(ctx, env, applyOpts...)
	if err != nil {
		return werr.WrapForeignErrorWithCause(werr.ErrEngineInit, err, "apply registry")
	}

	err = compilation.RegisterAllPhaseHandlers(env)
	if err != nil {
		return werr.WrapForeignErrorWithCause(werr.ErrEngineInit, err, "register phase handlers")
	}

	// Macros (define-syntax) load into the mutable expand frame FIRST; procedures
	// (define) use bootstrap macros (let/and), so they load into the sealed base AFTER.
	bootstrapResolver := compilation.NewEmbedFileResolver(core.BootstrapFS)
	err = loadBootstrapMacros(ctx, env, reg.MacroSources(), bootstrapResolver)
	if err != nil {
		return werr.WrapForeignErrorWithCause(werr.ErrEngineInit, err, "load bootstrap macros")
	}
	err = loadBootstrapProcedures(ctx, runtimeTarget, reg.ProcedureSources(), bootstrapResolver)
	if err != nil {
		return werr.WrapForeignErrorWithCause(werr.ErrEngineInit, err, "load bootstrap procedures")
	}

	// Inject documentation into bootstrap macro bindings (expand-time).
	// Must run after loadBootstrapMacros so define-syntax bindings exist.
	reg.ApplyDocs(env)

	// Register documentation-only entries for Scheme-defined procedures
	// that have structured docstrings (parameters, return type, category).
	registerSchemeDocstrings(env, reg)

	return nil
}

// expandAndCompileOptimized runs the expand → compile → optimize pipeline for a
// single syntax value. Thin wrapper around compilation.ExpandAndCompile that adds
// the Optimize() call used by the public Engine API.
func expandAndCompileOptimized(ctx context.Context, env *environment.EnvironmentFrame, stx syntax.SyntaxValue, resolver compilation.FileResolver, inlineThreshold int, maxExpandDepth int) (*machine.NativeTemplate, error) {
	tpl, err := compilation.ExpandAndCompile(ctx, env, stx, resolver, inlineThreshold, maxExpandDepth)
	if err != nil {
		return nil, err
	}
	tpl.Optimize()
	return tpl, nil
}

func (p *Engine) compileExpr(ctx context.Context, stx syntax.SyntaxValue) (*CompiledCode, error) {
	tpl, err := expandAndCompileOptimized(ctx, p.env, stx, nil, p.inlineThreshold, p.maxExpandDepth)
	if err != nil {
		return nil, wrapCompilationError("expand/compile error", err)
	}
	trackTemplateTree(p.coverageCollector, tpl)
	return &CompiledCode{template: tpl, env: p.env}, nil
}

func (p *Engine) runCompiled(ctx context.Context, cc *CompiledCode) (Value, error) {
	mc := machine.AcquireTopLevelContext(ctx, cc.template, cc.env)
	defer machine.ReleaseTopLevelContext(mc)
	mc.SetMaxCallDepth(p.maxCallDepth)
	mc.SetMaxStackSize(p.maxStackSize)
	if p.debugger != nil {
		mc.SetDebugger(p.debugger.machineDebugger())
	}

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

// wrapCompilationError creates a CompilationError, extracting the innermost
// source location from the SourcedError chain. The innermost location points
// at the actual error site (e.g., the undefined variable), not the enclosing
// form. The full chain remains available via Cause for outer context.
func wrapCompilationError(msg string, cause error) *CompilationError {
	ce := &CompilationError{Message: msg, Cause: cause}
	var se *compilation.SourcedError
	for err := cause; errors.As(err, &se); err = se.Cause {
		loc := se.Source.Location()
		if loc != "" {
			ce.Source = loc
		}
	}
	return ce
}

// wrapRuntimeError creates a RuntimeError from a VM execution error, extracting
// source location, stack trace, and condition value from ErrExceptionEscape when
// present. Falls back to a plain RuntimeError for non-exception errors.
func (p *Engine) wrapRuntimeError(err error) *RuntimeError {
	var ee *machine.ErrExceptionEscape
	if errors.As(err, &ee) {
		re := &RuntimeError{
			Message:   "runtime error",
			Cause:     err,
			Condition: wrapValue(ee.Condition),
		}
		re.Source = ee.Source.Location()
		if len(ee.StackTrace) > 0 {
			re.StackTrace = ee.StackTrace.String()
		}
		return re
	}
	return newRuntimeErrorWithCause("runtime error", err)
}

// newFileResolver creates the appropriate FileResolver from resolver factories.
// When no factories are provided, defaults to OSFileResolver.
// A single factory returns its resolver directly; multiple factories
// produce a ChainFileResolver that tries each in order.
func newFileResolver(factories []resolverFactory, env *environment.EnvironmentFrame) compilation.FileResolver {
	if len(factories) == 0 {
		return compilation.NewOSFileResolver(env)
	}
	if len(factories) == 1 {
		return factories[0](env)
	}
	resolvers := make([]compilation.FileResolver, len(factories))
	for i, f := range factories {
		resolvers[i] = f(env)
	}
	return compilation.NewChainFileResolver(resolvers)
}

// loadBootstrapMacros and loadBootstrapProcedures delegate to the shared
// compilation.LoadBootstrapSources pipeline (parse → expand → compile → optimize →
// pooled execute). They differ only in target frame and error-context kind; macros MUST
// load before procedures (procedures use bootstrap macros). See compilation/load_bootstrap.go.
func loadBootstrapMacros(ctx context.Context, env *environment.EnvironmentFrame, sources []string, resolver compilation.FileResolver) error {
	return compilation.LoadBootstrapSources(ctx, env, sources, resolver, "macro")
}

// loadBootstrapProcedures loads runtime-procedure sources (define forms) into the given
// target frame — the sealed base for an engine root/profile, or a flat library frame.
func loadBootstrapProcedures(ctx context.Context, target *environment.EnvironmentFrame, sources []string, resolver compilation.FileResolver) error {
	return compilation.LoadBootstrapSources(ctx, target, sources, resolver, "procedure")
}

// trackTemplateTree registers tpl and every *machine.NativeTemplate
// reachable via its literals pool with the given collector. Sub-templates
// (lambda bodies, etc.) appear as *NativeTemplate values in each parent
// template's literals pool; OpMakeClosure reads them from there at runtime.
// BFS with a visited set cuts cycles (possible via self-referencing closures).
func trackTemplateTree(col *coverage.Collector, root *machine.NativeTemplate) {
	if col == nil || root == nil {
		return
	}
	visited := make(map[*machine.NativeTemplate]bool)
	queue := []*machine.NativeTemplate{root}
	for len(queue) > 0 {
		tpl := queue[0]
		queue = queue[1:]
		if visited[tpl] {
			continue
		}
		visited[tpl] = true
		col.Track(tpl)
		for _, lit := range tpl.Literals() {
			child, ok := lit.(*machine.NativeTemplate)
			if !ok {
				continue
			}
			if !visited[child] {
				queue = append(queue, child)
			}
		}
	}
}

// makeDocRegistrationObserver returns an import observer that scans each
// imported library's exported bindings for structured docstrings and
// registers them as doc-only entries in the primitive registry. This makes
// Scheme-defined procedures from imported libraries visible to ,topic,
// ,apropos, and ,doc without requiring a full environment rescan.
func makeDocRegistrationObserver(libReg *compilation.LibraryRegistry, reg *registry.Registry) func(compilation.LibraryImportEvent) {
	return func(evt compilation.LibraryImportEvent) {
		lib := libReg.Lookup(evt.Library)
		if lib == nil {
			return
		}

		for _, name := range evt.Imported {
			internalName := lib.GetInternalName(name)
			if internalName == "" {
				internalName = name
			}

			sym := values.NewSymbol(internalName)
			bnd := lib.Env.GetBinding(sym, nil)
			if bnd == nil || bnd.BindingType() != environment.BindingTypeVariable {
				continue
			}

			raw := callableDocFromValue(bnd.Value())
			if raw == "" {
				continue
			}

			parsed := docparse.ParseDocstring(raw)
			if !parsed.HasStructuredMetadata() {
				continue
			}

			reg.AddDocOnlyPrimitive(docOnlySpec(name, parsed))
		}
	}
}

// docOnlySpec builds a documentation-only PrimitiveSpec from a parsed
// docstring and the binding's external name. Used by both the library-export
// and runtime-binding docstring registration walks.
func docOnlySpec(name string, parsed docparse.DocInfo) registry.PrimitiveSpec {
	return registry.PrimitiveSpec{
		Name:       name,
		Doc:        parsed.Doc,
		ParamNames: parsed.ParamNames,
		ParamTypes: parsed.ParamTypes,
		ReturnType: parsed.ReturnType,
		Category:   parsed.Category,
		Keywords:   parsed.Keywords,
		ParamCount: len(parsed.ParamNames),
	}
}

// registerSchemeDocstrings walks runtime bindings and registers documentation-only
// entries in the registry for Scheme-defined procedures with structured docstrings.
func registerSchemeDocstrings(env *environment.EnvironmentFrame, reg *registry.Registry) {
	ns := env.Namespace()
	if ns == nil {
		return
	}

	// Post-carve, Scheme-defined bootstrap procedures with structured docstrings live in
	// the sealed base, not the mutable runtime child (which is empty at bootstrap time).
	// Read the sealed base so their docs are indexed; reading the mutable child's own
	// frame (ns.Phases().Get(0)) would silently drop every stdlib procedure doc (G2).
	base := ns.SealedBase()
	if base == nil {
		return
	}

	// Skip flat library frames (NewChildRuntime): they share the root namespace, so this
	// base is the ROOT's sealed base — already indexed at root bootstrap, and
	// AddDocOnlyPrimitive dedups the re-add anyway. applyBaseEnvironment runs once per
	// (import ...), so without this guard every import re-parses ~all root docstrings for
	// nothing (E3). A namespace-owning runtime frame (engine root / profile child) has
	// SealedBaseTarget() != env (it returns the sealed base); a library frame returns env.
	if env.SealedBaseTarget() == env {
		return
	}

	global := base.GlobalEnvironment()
	keys := global.Keys()
	bindings := global.Bindings()

	for sym, idx := range keys {
		if idx >= len(bindings) {
			continue
		}
		bnd := bindings[idx]
		if bnd == nil || bnd.BindingType() != environment.BindingTypeVariable {
			continue
		}

		raw := callableDocFromValue(bnd.Value())
		if raw == "" {
			continue
		}

		parsed := docparse.ParseDocstring(raw)
		if !parsed.HasStructuredMetadata() {
			continue
		}

		reg.AddDocOnlyPrimitive(docOnlySpec(sym.Key, parsed))
	}
}

// callableDocFromValue extracts the docstring from a callable value.
func callableDocFromValue(v values.Value) string {
	dc, ok := v.(interface{ Doc() string })
	if !ok {
		return ""
	}
	return dc.Doc()
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
// Returns an error if the path is empty. Returns nil (no-op) if the stack
// is not configured — path tracking requires SetLoadPathStack on the Namespace.
//
// Advanced embedders who need fine-grained control can use Push/Pop directly,
// but most should use WithLoadPath for automatic cleanup.
func (p *Engine) PushLoadPath(filePath string) error {
	stack := p.namespace.LoadPathStack()
	if stack == nil {
		return nil
	}
	if filePath == "" {
		return werr.WrapForeignErrorf(werr.ErrInvalidLoadPath, "path must not be empty")
	}
	stack.Push(filePath)
	return nil
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
