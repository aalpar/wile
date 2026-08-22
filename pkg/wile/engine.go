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
	"github.com/aalpar/wile/pkg/internal/bootstrap"
	"github.com/aalpar/wile/pkg/internal/forms"
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
// Within an Engine, the VM coordinates its own runtime structures and
// SRFI-18 thread scheduling. It does NOT make concurrent mutation of shared
// Scheme objects atomic: vector-set!, set-car!/set-cdr!, record and port
// writes, and set! on a captured variable are plain stores. Programs that
// share mutable state across SRFI-18 threads must synchronize it themselves,
// with SRFI-18 mutexes or the atomic primitives.
type Engine struct {
	namespace               *environment.Namespace
	env                     *environment.EnvironmentFrame
	registry                *registry.PrimitiveRegistry
	debugger                *Debugger
	lastCounters            machine.VMCounters
	closers                 []registry.CloseFunc
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
func bootstrapNamespace(ctx context.Context, cfg *engineConfig) (*environment.Namespace, []extSnapshot, []registry.CloseFunc, error) {
	// Both strict levels derive the visible surface from a registry this package
	// mints (coreOnlyRegistry, or an empty one, below), not from cfg.registry. A
	// caller-supplied registry (WithRegistry/WithoutCore, both of which set
	// cfg.registry) may omit or replace core, so minting fresh core would bind
	// primitives the caller excluded — silently widening the top level past the
	// configured registry and breaking the family's "never widens" invariant.
	// Level 2's visible surface is empty and so cannot widen anything, but
	// WithoutCore empties BOTH the visible surface and the registry backing
	// library environments, where level 2 empties only the former; accepting the
	// pair would quietly answer a question the caller asked two ways. Reject at
	// construction; use WithProfile to bound capability.
	if cfg.strictLevel > strictLevelOff && cfg.registry != nil {
		return nil, nil, nil, werr.WrapForeignErrorf(werr.ErrEngineInit,
			"WithStrictNamespace/WithoutAmbientBindings is incompatible with WithRegistry/WithoutCore: the strict levels derive the visible surface from the default core registry, not from a caller-supplied one; use WithProfile to bound capability")
	}

	reg, snapshots, closers, err := buildRegistry(cfg)
	if err != nil {
		return nil, nil, nil, err
	}

	ns := environment.NewNamespace()
	// Name the engine's top level eagerly so (interaction-environment) is a pure
	// read. The introspection primitive previously labeled this shared namespace
	// lazily on first call ("" -> "interaction-environment"), which is a data race
	// when concurrent SRFI-18 threads query it (each reads "" and writes). Setting
	// it once here, before any thread runs, removes that race and the prior
	// order-dependence of (namespace-name (interaction-environment)).
	ns.Name = "interaction-environment"
	ns.SetRegistry(reg)
	// Fork a per-engine forms registry from the R7RS default, then apply the
	// engine's dialect to customize it — add/remove/rename special forms for this
	// engine only. R7RS is itself a dialect (DefaultDialect), applied when the
	// caller supplies none, so the path is uniform: always fork, always apply a
	// dialect. The registry is copy-on-write over the shared default, so a
	// dialect's mutations never affect the default or other engines. DefaultDialect
	// installs nothing (the clone already carries R7RS), so the no-dialect path is
	// behavior-identical to the pre-dialect seam.
	dialect := cfg.dialect
	if dialect == nil {
		dialect = DefaultDialect
	}
	fr := forms.DefaultRegistry().Clone()
	err = dialect.InstallForms(fr)
	if err != nil {
		return nil, nil, nil, werr.WrapForeignErrorWithCause(werr.ErrEngineInit, err,
			"install dialect %q forms", dialect.Name())
	}
	ns.SetFormRegistry(fr)
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

	// Strictness picks the registry the VISIBLE top level is bound from; the
	// profile's full registry (reg) stays on the namespace and backs library
	// environments at every level, so what strictness withholds is always
	// reachable via (import …) and never unreachable. Level 0 keeps the full
	// registry, so topLevelReg == reg and that path is byte-for-byte unchanged.
	//
	// The level-2 arm is an EMPTY registry, not a coreless engine: an empty
	// registry supplies no primitives and no bootstrap macros, so the visible
	// frame ends up holding only the phase handlers, which never came from a
	// registry at all (see WithoutAmbientBindings).
	topLevelReg := reg
	switch cfg.strictLevel {
	case strictLevelCore:
		topLevelReg, err = coreOnlyRegistry()
		if err != nil {
			return nil, nil, nil, err
		}
	case strictLevelNoBindings:
		topLevelReg = registry.NewRegistry()
	}

	// A dialect may cross the forms-only ceiling by also implementing
	// PrimitiveRemover — naming procedures (the mutation set, etc.) to omit from
	// the visible top level. registry.Without returns a filtered copy, so the full
	// reg still backs library environments and imports; only the top-level binding
	// set shrinks. Composes with strict mode: topLevelReg is already core-only there.
	remover, ok := dialect.(PrimitiveRemover)
	if ok {
		removed := remover.RemovedPrimitives()
		if len(removed) > 0 {
			topLevelReg = topLevelReg.Without(removed...)
		}
	}

	// A dialect may also rewrite the bootstrap procedure sources — e.g. swap the
	// mutating vector-map/string-map for a mutation-free fragment so the mutation
	// primitives (removed just above) leave no dangling bootstrap reference. Runs
	// after PrimitiveRemover so the rewritten fragment sees the narrowed surface.
	// WithProcedureSources copies, so the full reg backing library envs is untouched.
	rewriter, ok := dialect.(BootstrapProcedureRewriter)
	if ok {
		topLevelReg = topLevelReg.WithProcedureSources(rewriter.RewriteBootstrapProcedures(topLevelReg.ProcedureSources()))
	}

	// Record what the top level was actually bound from, before applyBaseEnvironment
	// consumes it and the local goes out of scope. Engine.EffectiveRegistry reads this;
	// without it the only registry an embedder can reach is the pre-dialect reg on the
	// namespace, which still advertises primitives a dialect removed.
	ns.SetEffectiveRegistry(topLevelReg)

	env := ns.Runtime()
	err = applyBaseEnvironment(ctx, env, topLevelReg, applyOptionsFromConfig(cfg)...)
	if err != nil {
		return nil, nil, nil, err
	}

	return ns, snapshots, closers, nil
}

// coreOnlyRegistry builds a registry holding only the core primitive surface. It is
// the bare top-level binding set for strict-namespace mode and mirrors the registry a
// Tiny engine produces, so a strict top level equals the Tiny visible surface. The
// engine's full (profile) registry is still used for library environments and synthetic
// library registration, so (import …) reaches the profile's extensions.
func coreOnlyRegistry() (*registry.PrimitiveRegistry, error) {
	reg := registry.NewRegistry()
	err := core.AddToRegistry(reg)
	if err != nil {
		return nil, werr.WrapForeignErrorWithCause(werr.ErrEngineInit, err, "build core-only registry")
	}
	return reg, nil
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
// options still apply. See WithNamespace for the full list of options
// this path silently ignores, several of them security-relevant.
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
// The WithNamespace path (pre-built namespace) skips steps 2-4 and trusts that
// the caller bootstrapped correctly. Step 5 still runs on that path, but only
// if the supplied namespace has no file resolver of its own; step 6 runs
// unconditionally. NewNamespace() performs steps 2-4.
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
	var reg *registry.PrimitiveRegistry
	var snapshots []extSnapshot
	var closers []registry.CloseFunc

	if cfg.namespace != nil {
		// A dialect customizes the forms registry during bootstrapNamespace, which
		// the pre-built-namespace path skips entirely — so a dialect supplied here
		// would be silently ignored. Reject the combination rather than drop it,
		// mirroring the WithStrictNamespace+WithRegistry conflict in
		// bootstrapNamespace. Install the dialect's forms on the namespace before
		// WithNamespace, or drop WithNamespace.
		if cfg.dialect != nil {
			return nil, werr.WrapForeignErrorf(werr.ErrEngineInit,
				"WithDialect is incompatible with WithNamespace: a dialect customizes the forms registry at engine origin, which the pre-built-namespace path skips")
		}
		// Use pre-built namespace
		ns = cfg.namespace
		regAny := ns.Registry()
		if regAny == nil {
			return nil, werr.WrapForeignErrorf(werr.ErrEngineInit,
				"WithNamespace: namespace has no registry — use wile.NewNamespace() or SetRegistry()")
		}
		var ok bool
		reg, ok = regAny.(*registry.PrimitiveRegistry)
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
		reg = ns.Registry().(*registry.PrimitiveRegistry)
	}

	env := ns.Runtime()

	// Forward the resolved inline threshold onto the shared EngineServices so that
	// runtime-triggered library compilation (import / (environment …)) honors
	// WithInlineThreshold — the top-level compile path passes it explicitly, but
	// the library load path reaches the compiler through the namespace, not a
	// parameter. cfg.inlineThreshold is already defaulted above; an explicit 0
	// (inlining disabled) is preserved by SetInlineThreshold's set-flag.
	ns.SetInlineThreshold(cfg.inlineThreshold)

	// Forward the resolved expansion-depth bound too, for the same reason: the
	// library load path reaches the expander through the namespace, not a
	// parameter, so without this an imported library ignores WithMaxExpandDepth
	// and expands at the default bound. cfg.maxExpandDepth is already defaulted
	// above; an explicit 0 (unlimited) is preserved by the set-flag.
	ns.SetMaxExpandDepth(cfg.maxExpandDepth)

	// Set the default file resolver for runtime include/load operations.
	// This must happen after bootstrap (which uses EmbedFileResolver).
	// Pre-built namespaces (WithNamespace) may already have a resolver.
	if env.FileResolver() == nil {
		env.SetFileResolver(newFileResolver(cfg.resolverFactories, env))
	}

	if cfg.libraryEnabled {
		err := setupLibrarySystem(ctx, cfg.libraryPaths, cfg.importObserver, reg, env, ns, snapshots, applyOptionsFromConfig(cfg), cfg.strictLevel)
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
// and collects this engine's closers for Engine.Close().
//
// Closers come from two places. An extension implementing registry.Closeable
// contributes its Close method, which is process-global — the Extension value is
// a package-level var shared by every engine, so such a hook has no way to tell
// two engines apart. An extension that needs per-engine cleanup instead calls
// PrimitiveRegistry.AddCloser from its AddToRegistry and reaches its state
// through the environment frame Engine.Close passes it (registry.CloseFunc).
//
// Only the hooks THIS loop registered are taken. On a registry reused across
// engines (WithRegistry) each engine's AddToRegistry appends another copy of the
// same hook, and the delta keeps Close from running that hook once per engine
// that ever loaded the extension. It is de-duplication, not ownership: ownership
// is decided by the frame the hook is called with.
func buildRegistry(cfg *engineConfig) (*registry.PrimitiveRegistry, []extSnapshot, []registry.CloseFunc, error) {
	reg := cfg.registry
	if reg == nil {
		reg = registry.NewRegistry()
		err := core.AddToRegistry(reg)
		if err != nil {
			return nil, nil, nil, werr.WrapForeignErrorWithCause(werr.ErrEngineInit, err, "register core primitives")
		}
	}

	var snapshots []extSnapshot
	var closers []registry.CloseFunc
	startClosers := len(reg.Closers())
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
			// Adapt the engine-blind Closeable to the CloseFunc shape. The frame is
			// dropped because a process-global Close has nothing to do with it.
			closers = append(closers, func(*environment.EnvironmentFrame) error {
				return closer.Close()
			})
		}
	}
	closers = append(closers, reg.Closers()[startClosers:]...)

	return reg, snapshots, closers, nil
}

// setupLibrarySystem configures the R7RS library system: search paths,
// import observer, extension libraries, and the library environment factory.
// applyOpts propagates registry.Apply toggles (e.g., contract enforcement)
// into child library environments so they mirror the parent's configuration.
func setupLibrarySystem(ctx context.Context, libraryPaths []string, importObserver func(LibraryImportEvent), reg *registry.PrimitiveRegistry, env *environment.EnvironmentFrame, ns *environment.Namespace, snapshots []extSnapshot, applyOpts []registry.ApplyOption, level strictLevel) error {
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

	// Synthetic extension libraries re-export the profile's primitives and resolve
	// their exports against their backing env. In strict mode the visible top-level
	// env is bare (core-only), so resolve them against a full-registry child runtime
	// instead. NewChildRuntime owns its OWN sealed base, so the full registry lands
	// there and does NOT pollute the shared namespace's sealed base — the user top
	// level stays bare.
	// Non-strict uses env directly.
	// Only worth building when there are extension libraries to back: with no
	// snapshots (zero-extension profile, or the pre-built-namespace path where
	// snapshots is empty) registerExtensionLibraries is a no-op, so the full apply
	// would be wasted.
	synthEnv := env
	if level > strictLevelOff && len(snapshots) > 0 {
		synthEnv = ns.NewChildRuntime()
		applyErr := applyBaseEnvironment(ctx, synthEnv, reg, applyOpts...)
		if applyErr != nil {
			return applyErr
		}
	}

	err := registerExtensionLibraries(reg, synthEnv, libReg, snapshots)
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
// Security checks during execution are decided by the intersection of the
// engine's authorizer and the target namespace's, most-restrictive-wins, so a
// target cannot widen the engine's policy. If the target has no authorizer of
// its own, the engine's is propagated to it before evaluation.
//
// That policy follows the EXECUTING namespace, not the one a primitive was
// registered in. The distinction is invisible until they differ, which is here
// and nowhere else in production: a primitive reached from this namespace runs
// on an apply frame belonging to the namespace that registered it — the engine
// root, for every extension library — and reading policy off that frame is what
// used to leave the target's authorizer unconsulted.
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
// string surgery.
//
// THE HEAD CARRIES A FRESH SCOPE, and that is load-bearing, not decoration. The
// head is no longer dispatched by name: validate.headDenotesSpecialForm decides
// by the binding it resolves to, so a program containing (define begin 9) made
// its OWN wrapper a call — the whole file compiled to (9 form ...) and died with
// "expected a procedure, got #<void>" after running. A scope the user's ∅-scoped
// binder does not carry keeps the wrapper out of its reach: an identifier with
// scopes reaches a ∅-scoped global only through the macro-expansion widening
// that referenceReachesBinderDirectly refuses. Forms keep their own contexts;
// only this head is scoped, so nothing inside the program is affected.
func wrapInBegin(forms []syntax.SyntaxValue) syntax.SyntaxValue {
	srcCtx := forms[0].SourceContext()
	beginSym := syntax.NewSyntaxSymbol("begin", srcCtx).
		AddScope(syntax.NewScopeWithLabel("program"))
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
//
// It REFUSES (werr.ErrImmutableBinding) to rebind a name that Scheme code
// already defined at the top level, because under the default immutable top
// level such a binding is proven rebind-stable and the compiler acts on that
// proof irreversibly: it pins the value into self-tail-call sites and it refuses
// a program at compile time on an arity mismatch against a stable callee. There
// is no run at which to re-check the second one, so Define cannot be permitted
// and merely deoptimized — it would turn a program that runs correctly into a
// compile error.
//
// What still works, and is the intended shape: a name the host owns end to end.
// A name never defined from Scheme carries no proof, so Define creates it and
// may rebind it as often as it likes. A name the embedder also IMPORTED is also
// rebindable — a define shadows an import rather than assigning through it.
//
// Use WithMutableTopLevel to opt the whole engine out; nothing is stamped stable
// there, at the cost of the frame-reclaim win on user recursion.
func (p *Engine) Define(name string, value Value) error {
	sym := values.NewSymbol(name)
	_, err := p.env.DefineOwnGlobal(sym, environment.BindingTypeVariable, nil, unwrapValue(value))
	return err
}

// Get retrieves a value by name from the environment.
//
// Resolution is scoped to the ambient (empty) scope set, symmetric with
// [Engine.Define], which creates under that same set. A wildcard read would
// return the name's first slot regardless of hygiene, so once any macro
// introduced a same-named top-level binder the host could not round-trip its
// own value: Define would write one slot and Get would read another.
//
// A name bound ONLY under a non-empty scope set is correctly reported as
// absent. Such a binding is macro-introduced and is unreachable from any
// source-written reference too, so there is no name the host could legitimately
// use to ask for it.
//
// The same reasoning governs every reflective read of a bare symbol, so
// environment-ref/environment-bound? and namespace-ref/namespace-bound? resolve
// under the ambient set as well. Wildcard survives only where a NAME, not a
// variable, is the unit: registerSchemeDocstrings below and
// searchEnvironmentBindings (registry/search.go) attach and retrieve docstrings,
// and one name owns one doc entry however many hygiene-distinct bindings share
// it, so the ambiguity does not arise. REPL completion still walks the
// unfiltered key map and can therefore offer a name that resolves to nothing;
// that is a known defect, filed in TODO.md, not an endorsement of wildcard.
func (p *Engine) Get(name string) (Value, bool) {
	sym := values.NewSymbol(name)
	idx := p.env.GetGlobalIndexWithScopes(sym, values.EmptyScopes())
	if idx == nil {
		return nil, false
	}
	binding := p.env.GetGlobalBinding(idx)
	if binding == nil {
		return nil, false
	}
	return wrapValue(binding.Value()), true
}

// RegisterPrimitive adds a Go function as a Scheme primitive.
//
// The spec is validated first ([registry.PrimitiveSpec.Validate]) and an invalid
// spec is returned as an error, never bound. This is the same contract
// [registry.PrimitiveRegistry.AddPrimitives] enforces by panicking: an embedder
// assembling a spec dynamically gets a value it can handle, rather than a
// binding whose first call takes down the engine. The IsVariadic +
// ParamCount:0 shape is the one that matters — its panic fires during frame
// setup, so it wedges every subsequent evaluation on this engine, not just the
// offending call.
//
// It shares [Engine.Define]'s refusal: a name Scheme code already defined at the
// top level is rebind-stable under the default immutable top level and cannot be
// replaced. Register before running Scheme that defines the same name.
func (p *Engine) RegisterPrimitive(spec PrimitiveSpec) error {
	err := spec.Validate()
	if err != nil {
		return werr.WrapForeignErrorWithCause(werr.ErrInvalidArgument, err,
			"RegisterPrimitive: invalid spec %q", spec.Name)
	}

	sym := values.NewSymbol(spec.Name)

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

	_, err = p.env.DefineOwnGlobal(sym, environment.BindingTypeVariable, nil, closure)
	return err
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
// This provides access to per-instance syntax interning and phase management.
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
//
// This is the pre-dialect base: everything registered, which is what makes it the
// right input for building a derived engine. It is NOT what this engine can call —
// WithStrictNamespace and a dialect's PrimitiveRemover both narrow the visible top
// level below it, and this clone still lists what they removed. For the surface
// this engine actually has, use [Engine.EffectiveRegistry] and [Engine.Forms].
func (p *Engine) Registry() *registry.PrimitiveRegistry {
	return p.registry.Clone()
}

// EffectiveRegistry returns a clone of the registry the engine's visible top level
// was bound from: [Engine.Registry] narrowed by WithStrictNamespace and by a
// dialect's PrimitiveRemover. Registry answers "what was registered"; this answers
// "what can this engine call". They differ only when something narrowed the surface,
// and are otherwise the same content.
//
// Procedures only. A dialect shapes two axes, and special forms live in the forms
// registry rather than here: wile.NoMutation removes both the set! form and the
// set-car! procedure, and only [Engine.Forms] shows the former's absence.
//
// The full registry still backs library environments and imports, so a narrowed
// primitive may remain reachable through (import …) — this reports the flat top
// level, not total reachability.
func (p *Engine) EffectiveRegistry() *registry.PrimitiveRegistry {
	regAny := p.namespace.EffectiveRegistry()
	reg, ok := regAny.(*registry.PrimitiveRegistry)
	if !ok {
		// No narrowing was recorded (a WithNamespace caller built the namespace
		// without one, or nothing narrowed): the base is the effective surface.
		return p.registry.Clone()
	}
	return reg.Clone()
}

// Forms returns the sorted names of the special forms this engine's dialect
// installed. Together with [Engine.EffectiveRegistry] this is the engine's actual
// surface: syntax here, procedures there.
//
// Names rather than the specs themselves: a FormSpec carries validator and codegen
// hooks that are internal by construction and are not an embedder contract.
func (p *Engine) Forms() []string {
	frAny := p.namespace.FormRegistry()
	fr, ok := frAny.(*forms.FormRegistry)
	if !ok {
		return nil
	}
	q := fr.Names()
	slices.Sort(q)
	return q
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
func registerExtensionLibraries(reg *registry.PrimitiveRegistry, env *environment.EnvironmentFrame, libReg *compilation.LibraryRegistry, snapshots []extSnapshot) error {
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

// applyBaseEnvironment performs the setup that every usable environment requires,
// in order: apply registry bindings, register phase handlers (syntax compilers +
// primitive expanders), load bootstrap macros, load bootstrap procedures, stamp
// curated inline HOFs and build their loop templates, then inject documentation
// (into macro bindings and as doc-only entries for Scheme-defined procedures).
// Steps that can fail wrap errors with ErrEngineInit. Additional registry.ApplyOption
// values are forwarded to the registry Apply call.
func applyBaseEnvironment(ctx context.Context, env *environment.EnvironmentFrame, reg *registry.PrimitiveRegistry, opts ...registry.ApplyOption) error {
	// Run the ordering-sensitive sequence shared with internal/bootstrap: apply the
	// registry, register phase handlers, then load bootstrap macros -> procedures ->
	// late macros. It returns the runtime target frame (sealed base for a
	// namespace-owning runtime env, the frame itself for a flat library env) that the
	// post-steps below operate on.
	runtimeTarget, err := bootstrap.LoadBootstrapCore(ctx, env, reg, opts...)
	if err != nil {
		return werr.WrapForeignErrorWithCause(werr.ErrEngineInit, err, "load bootstrap core")
	}

	// Stamp the bootstrap-resident curated tail HOFs (for-each, vector-map, …)
	// with the InlineHOF capability, now that the bootstrap procedures are bound.
	// A post-load metadata sweep, like the doc injection below. Read by the
	// compiler's inline-HOF dispatch (callback specialization A). Import-gated HOFs
	// (fold, srfi/1) are stamped on their import path instead.
	compilation.StampInlineHOFs(runtimeTarget)

	// Build the per-Namespace inline-HOF loop templates against the now-loaded
	// sealed base, so the dispatch can substitute a curated HOF's reclaiming loop
	// at a capture-safe call site. Idempotent per Namespace.
	err = compilation.BuildInlineHOFTemplates(ctx, runtimeTarget)
	if err != nil {
		return werr.WrapForeignErrorWithCause(werr.ErrEngineInit, err, "build inline-HOF templates")
	}

	// Inject documentation into bootstrap macro bindings (expand-time).
	// Must run after loadBootstrapMacros so define-syntax bindings exist.
	//
	// Skip flat library frames (NewChildRuntime): they share the root namespace,
	// so ApplyDocs walks the ROOT's phase bindings (env.Namespace().Phases()),
	// never the flat frame's own phases. Re-running it per (import …) only
	// re-writes the shared base bindings' Doc meta — redundant with the root-init
	// application, and a data race when several imports run concurrently from
	// SRFI-18 threads (all writing the same shared base binding's meta). Mirrors
	// the identical guard in registerSchemeDocstrings below.
	if env.IsNamespaceRuntime() {
		reg.ApplyDocs(env)
	}

	// Register documentation-only entries for Scheme-defined procedures
	// that have structured docstrings (parameters, return type, category).
	registerSchemeDocstrings(env, reg)

	return nil
}

// expandAndCompileOptimized runs the expand → compile → optimize pipeline for a
// single syntax value. Thin wrapper around compilation.ExpandAndCompile that adds
// the Optimize() call used by the public Engine API.
func expandAndCompileOptimized(ctx context.Context, env *environment.EnvironmentFrame, stx syntax.SyntaxValue, resolver compilation.FileResolver, inlineThreshold int, maxExpandDepth int) (q *machine.NativeTemplate, rerr error) {
	// Containment boundary for the compile verb. This is the single funnel for Eval,
	// EvalMultiple, and Compile, and the only caller of tpl.Optimize(), so the
	// peephole optimizer is covered too.
	//
	// Placed here rather than in compilation.ExpandAndCompile for the same reason as
	// the parse boundary: a compile-time panic is a Wile bug, and containing it
	// deeper would hide our defects from `go test ./pkg/machine/...`. A host driving
	// pkg/machine directly is unprotected — narrow, documented, and the
	// ExpandAndCompile seam stays available if someone asks for it.
	//
	// After the frame-slot guard (compile_closure.go), no valid input reaches a panic
	// here. This is a backstop for future bugs, not a fix for a live one.
	defer func() {
		r := recover()
		if r == nil {
			return
		}
		q = nil
		rerr = wrapCompilationError("compile error", werr.RecoverAsError(r, werr.ErrInternal, "compile"))
	}()

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
		p.armBreakPrompt(mc, cc)
	}

	err := mc.RunWithEscapeHandling()
	p.lastCounters = mc.Counters()
	val := mc.GetValue()
	if err != nil {
		return nil, p.wrapRuntimeError(err)
	}
	return wrapValue(val), nil
}

// armBreakPrompt installs the debugger's break boundary on mc when — and only
// when — a suspension handler is registered AND the debugger has something that
// could stop this run. Otherwise a break falls back to the render-only callback
// and no prompt frame is pushed.
//
// Both conditions are load-bearing, because the boundary is not free: it is a
// real continuation frame, so it costs one unit of the call-depth budget for the
// whole run and changes the tail-call shape at the top level. The REPL registers
// a suspension handler for its entire session, so without the second condition
// every evaluation typed at the prompt paid for a debugger nobody had armed.
//
// The install and the handler are two calls because the handler closes over the
// tag the install mints.
func (p *Engine) armBreakPrompt(mc *machine.MachineContext, cc *CompiledCode) {
	if p.debugger.onBreakSuspend == nil {
		return
	}
	if !p.debugger.canStop() {
		return
	}
	closureEnv := cc.env.MutableRuntimeOrNil()
	if closureEnv == nil {
		closureEnv = cc.env
	}
	tag := mc.InstallBreakPrompt(nil)
	mc.SetBreakHandler(p.debugger.breakHandler(closureEnv, tag))
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
//
// Two provenance sources are consulted, mirroring the two layers that produce
// these errors: the compiler attaches *compilation.SourcedError, while the
// parser (a lower layer that never carries a SourcedError) attaches the
// offending token on *parser.ParserError. Without the second lookup a parse
// error's file:line:col is dropped on the floor even though the parser knew it
// — see REVIEW.md "Error Chain Losslessness".
//
// The location is also what Cause's text renders, but only when the INNERMOST
// SourcedError is the one carrying it: SourcedError.Error prefixes its location
// only if no deeper SourcedError exists. So the same walk that finds the location
// decides whether Error() should prefix it again — see CompilationError.structured.
//
// Condition is the same object a Scheme guard catches, built by the single
// converter in pkg/machine rather than assembled here, so the compile-time and
// runtime representations cannot drift. It is populated unconditionally: a nil
// Condition then means "no cause to convert" and never "this path skipped it",
// which is the polarity nil carries everywhere else in the API.
func wrapCompilationError(msg string, cause error) *CompilationError {
	ce := &CompilationError{Message: msg, Cause: cause}
	var se *compilation.SourcedError
	for err := cause; errors.As(err, &se); err = se.Cause {
		loc := se.Source.Location()
		ce.structured = loc != ""
		if loc != "" {
			ce.Source = loc
		}
	}
	// Parse errors carry location on their token rather than a SourcedError.
	// Consult them only when the compiler chain yielded nothing, so a genuine
	// compiler location (more specific) always wins when both are present. The
	// ParserError renders its position in its own format ("at index N, line L,
	// …"), not as file:line:col, so this location is not a duplicate and keeps
	// its prefix.
	if ce.Source == "" {
		var pe *parser.ParserError
		if errors.As(cause, &pe) {
			ce.Source = pe.Location()
		}
	}
	if cause == nil {
		return ce
	}
	ce.Condition = wrapValue(machine.ConditionFromError(cause))
	// ConditionFromError stamps the innermost location off the SourcedError chain,
	// which a parse error does not have. Hand it the one this funnel recovered so
	// the two halves of the same error agree, and only when the condition has none
	// — a condition a primitive built and returned already knows its own site.
	ne, ok := ce.Condition.Internal().(*values.NativeError)
	if ok && ne.SourceLocation() == "" && ce.Source != "" {
		ne.SetSourceLocation(ce.Source)
	}
	return ce
}

// wrapRuntimeError creates a RuntimeError from a VM execution error, extracting
// source location, stack trace, and condition value from ErrExceptionEscape when
// present. Falls back to a plain RuntimeError for non-exception errors.
func (p *Engine) wrapRuntimeError(err error) *RuntimeError {
	structured := p.wrapSchemeError(err)
	if structured != nil {
		return structured
	}
	var ee *machine.ErrExceptionEscape
	if errors.As(err, &ee) {
		// Pull the bare condition text into Message and the location/trace into
		// the structured fields, then mark structured so Error() renders each
		// exactly once. The generic "runtime error" prefix is dropped here: the
		// condition text (e.g. "car: expected pair, got 5") is the real message,
		// and the surface (REPL "Exception:", CLI "Error:") supplies the category.
		re := &RuntimeError{
			Message:    ee.ConditionText(),
			Cause:      err,
			Condition:  wrapValue(ee.Condition),
			structured: true,
		}
		re.Source = ee.Source.Location()
		if len(ee.StackTrace) > 0 {
			re.StackTrace = ee.StackTrace.String()
		}
		return re
	}
	return newRuntimeErrorWithCause("runtime error", err)
}

// wrapSchemeError unpacks a *machine.SchemeError that terminated the
// evaluation, or returns nil when err carries none.
//
// This covers every error the VM raises that is deliberately NOT a Scheme
// condition, and there are two such classes: an authorizer denial (see
// applyCallableError) and a recovered host-fault panic (see
// MachineContext.RunResumable). Both travel as a *machine.SchemeError rather
// than as the exception-escape carrier the branch above unpacks, and the VM has
// already stamped each with a source location and a continuation-chain trace.
//
// Without this arm those survive only flattened into the cause's text:
// RuntimeError.Source and .StackTrace came back empty and Message was the
// generic "runtime error" — for the two error classes an embedder most needs to
// localise, since neither is catchable from Scheme and the Go-side diagnostic is
// therefore the only channel it has. Condition stays nil — there is no
// condition, which is the whole point — so IsSchemeException correctly reports
// false for both.
func (*Engine) wrapSchemeError(err error) *RuntimeError {
	var se *machine.SchemeError
	if !errors.As(err, &se) {
		return nil
	}
	return &RuntimeError{
		Message:    se.Message,
		Cause:      err,
		Source:     se.Source.Location(),
		StackTrace: se.StackTrace,
		structured: true,
	}
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

// trackTemplateTree registers tpl and every *machine.NativeTemplate
// reachable via its literals pool with the given collector. Sub-templates
// (lambda bodies, etc.) appear as *NativeTemplate values in each parent
// template's literals pool; OpMakeClosure reads them from there at runtime.
// BFS with a visited set cuts cycles (possible via self-referencing closures).
func trackTemplateTree(col *coverage.Collector, root *machine.NativeTemplate) {
	if col == nil || root == nil {
		return
	}
	visited := values.NewMapSet[*machine.NativeTemplate](0)
	queue := []*machine.NativeTemplate{root}
	for len(queue) > 0 {
		tpl := queue[0]
		queue = queue[1:]
		done := visited.Get(tpl)
		if done {
			continue
		}
		visited.Set(tpl)
		col.Track(tpl)
		for _, lit := range tpl.Literals() {
			child, ok := lit.(*machine.NativeTemplate)
			if !ok {
				continue
			}
			seen := visited.Get(child)
			if !seen {
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
func makeDocRegistrationObserver(libReg *compilation.LibraryRegistry, reg *registry.PrimitiveRegistry) func(compilation.LibraryImportEvent) {
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

			// Resolve exactly as the import path does. A bare-name lookup into
			// lib.Env agrees only while a name has one slot per frame, so it can
			// document a different binding than the one the import installed.
			bnd, found := lib.ExportedBinding(internalName)
			if !found || bnd.BindingType() != environment.BindingTypeVariable {
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
func registerSchemeDocstrings(env *environment.EnvironmentFrame, reg *registry.PrimitiveRegistry) {
	ns := env.Namespace()
	if ns == nil {
		return
	}

	// Skip library env frames (NewChildRuntime): they share the root namespace, so
	// ns.Store() here is the ROOT's store — already indexed at root bootstrap, and
	// AddDocOnlyPrimitive dedups the re-add anyway. applyBaseEnvironment runs once per
	// (import ...), so without this guard every import re-parses ~all root docstrings for
	// nothing (E3).
	if !env.IsNamespaceRuntime() {
		return
	}

	// Scheme-defined bootstrap procedures with structured docstrings live in the
	// SEALED tier, not the mutable one (which is empty at bootstrap time). The rank
	// filter is the whole query; enumerating the mutable tier too would index
	// nothing extra at bootstrap and would drift into indexing user defines later
	// (G2).
	//
	// The sealed tier is no longer only the startup set: imports install at
	// (ExactPhase(0), sealed) so a user define shadows them rather than assigning
	// through them (compilation.installImportedBinding). SealedSlots() therefore
	// yields imported bindings too, and since this runs once per applyBaseEnvironment
	// an imported Scheme procedure's structured docstring now gets a doc-only entry.
	// That is wanted, not incidental — the alternative is documented procedures
	// going missing from ,doc the moment they arrive by import — and G2 still holds
	// because a user DEFINE is on the mutable tier and remains excluded.
	//
	// SealedSlots() is every sealed slot at EVERY phase (the fold merged the
	// per-phase seal frames into one store), where the pre-fold walk visited only
	// the phase-0 seal's own slots. The BindingTypeVariable filter below therefore
	// does NOT confine the walk to phase 0: registry.Apply's phaseTargets seals its
	// expand-phase copies too, and those are BindingTypeVariable holding a
	// ForeignClosure (apply.go), so the loop reaches every dual-phase primitive
	// twice — 146 names, measured on a bootstrapped KitchenSink namespace.
	//
	// Harmless by construction, not by luck: AddDocOnlyPrimitive returns early on
	// any name already in reg.primitives (primitive_registry.go), and every name
	// this walk sees twice is a registry primitive, so the second visit adds
	// nothing. The cost is the duplicate Doc()/ParseDocstring pair, once per
	// applyBaseEnvironment — which the IsNamespaceRuntime guard above already
	// limits to the root.
	for _, slot := range ns.Store().SealedSlots() {
		sym := slot.Name
		bnd := slot.Binding
		if bnd.BindingType() != environment.BindingTypeVariable {
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

// Load-path API
//
// The load path lives on the CONTEXT, not on the engine. A load chain — an
// outer (load …), the files it includes, the files those load — shares one
// stack, and two chains never share one. That is what makes concurrent loading
// correct: before this, a single stack hung off the Namespace was shared by
// every SRFI-18 thread, so two threads loading files in different directories
// interleaved their pushes and one compiled the other's file.
//
// It also removes the balanced-push/pop problem. There is nothing to pop: a
// derived context's reach IS the extent of the push, so an early return, an
// error or a panic cannot leave the stack dirty.

// ContextWithLoadPath returns a copy of ctx that names filePath as the file
// currently being loaded, so a directory-relative (include …) or (load …) in
// code evaluated under it resolves against filePath's directory, and
// (current-load-path) reports it.
//
// Returns an error if filePath is empty.
//
// Example:
//
//	loadCtx, err := engine.ContextWithLoadPath(ctx, "/app/scripts/main.scm")
//	if err != nil {
//	    return err
//	}
//	// (load "helper.scm") resolves relative to /app/scripts/
//	_, err = engine.EvalMultiple(loadCtx, `(load "helper.scm")`)
//
// The returned context carries its OWN stack, seeded from ctx's if there is
// one, so the caller cannot leak a push into the context it was handed.
func (p *Engine) ContextWithLoadPath(ctx context.Context, filePath string) (context.Context, error) {
	if filePath == "" {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidLoadPath, "ContextWithLoadPath: path must not be empty")
	}
	stack := sourceload.NewLoadStack()
	prev := sourceload.LoadStackFromContext(ctx)
	if prev != nil {
		for _, q := range prev.Paths() {
			stack.Push(q)
		}
	}
	stack.Push(filePath)
	return sourceload.WithLoadStack(ctx, stack), nil
}

// CurrentLoadPath returns the path of the file ctx names as currently loading,
// or "" if ctx is not inside a load.
func (p *Engine) CurrentLoadPath(ctx context.Context) string {
	stack := sourceload.LoadStackFromContext(ctx)
	if stack == nil {
		return ""
	}
	return stack.Current()
}

// CurrentLoadDirectory returns the directory of the file ctx names as currently
// loading, or "" if ctx is not inside a load.
func (p *Engine) CurrentLoadDirectory(ctx context.Context) string {
	stack := sourceload.LoadStackFromContext(ctx)
	if stack == nil {
		return ""
	}
	return stack.CurrentDir()
}

// Close releases the resources this engine holds, from two sources: extensions
// implementing registry.Closeable have their Close method called, and per-engine
// hooks registered via PrimitiveRegistry.AddCloser are run against this engine's
// runtime frame. The shipped threads and process extensions use the latter —
// closing an engine terminates the SRFI-18 threads it started and kills the
// children it spawned, and only those: the hooks find their trackers on this
// engine's Namespace, so an engine sharing a registry with another (WithRegistry,
// where every engine binds the first engine's primitives) still reaps its own.
// Errors from individual closers are collected and returned via errors.Join.
// Calling Close on an already-closed engine returns ErrEngineClosed.
//
// An engine built with WithNamespace has NO closers: that path reuses a
// pre-built namespace and never runs buildRegistry, which is where both sources
// are collected. That is pre-existing — it was already true of registry.Closeable
// — and unchanged here.
func (p *Engine) Close() error {
	if p.closed {
		return werr.WrapForeignErrorf(ErrEngineClosed, "engine: already closed")
	}
	p.closed = true

	var errs []error
	for _, c := range p.closers {
		err := c(p.env)
		if err != nil {
			errs = append(errs, err)
		}
	}
	return errors.Join(errs...)
}
