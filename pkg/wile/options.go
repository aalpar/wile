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
	"io/fs"
	"maps"

	"github.com/aalpar/wile/coverage"
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/werr"
)

// LibraryImportEvent records what happened when a library was imported.
// See compilation.LibraryImportEvent for field documentation.
type LibraryImportEvent = compilation.LibraryImportEvent

// Phase is a typed enum identifying a stage of compilation/evaluation.
// Re-exported from environment for embedder convenience; use the
// PhaseRuntime/PhaseExpand/PhaseCompile/PhaseTemplate constants below.
type Phase = environment.Phase

// Phase constants for LibraryImportEvent.Phase and other phase-keyed APIs.
// Re-exported from environment for embedder convenience.
const (
	PhaseTemplate = environment.PhaseTemplate
	PhaseRuntime  = environment.PhaseRuntime
	PhaseExpand   = environment.PhaseExpand
	PhaseCompile  = environment.PhaseCompile
)

// newEngineConfig returns an engineConfig with the option-independent defaults that
// must be set BEFORE options apply. immutableTopLevel defaults to true (the
// layered-environment default): the sealed-base carve makes redefines of sealed names
// shadows and scopes enforcement to the user runtime, so default immutability no longer
// breaks the stdlib (R1/R2 dissolved). WithMutableTopLevel() opts back out.
func newEngineConfig() *engineConfig {
	return &engineConfig{
		immutableTopLevel: true,
	}
}

type engineConfig struct {
	registry           *registry.Registry
	extensions         []registry.Extension
	maxCallDepth       int
	callDepthSet       bool // true if WithMaxCallDepth was explicitly called
	maxContDepth       int
	contDepthSet       bool // true if WithMaxContinuationDepth was explicitly called
	maxParseDepth      int
	parseDepthSet      bool // true if WithMaxParseDepth was explicitly called
	maxExpandDepth     int
	expandDepthSet     bool // true if WithMaxExpandDepth was explicitly called
	maxStackSize       uint64
	inlineThreshold    int
	inlineThresholdSet bool // true if WithInlineThreshold was explicitly called
	libraryPaths       []string
	libraryEnabled     bool // true when WithLibraryPaths was called
	importObserver     func(LibraryImportEvent)

	// Authorizer accumulation is split across three intent-specific fields
	// resolved once, order-independently, by resolveAuthorizer. A single
	// shared field would be order-dependent because WithProfile/WithAuthorizer
	// assign while WithSandbox composes — two non-commutative write semantics.
	profileAuthorizer     security.Authorizer // set by WithProfile (last non-nil wins)
	explicitAuthorizer    security.Authorizer // set by WithAuthorizer (nil is meaningful: "open")
	explicitAuthorizerSet bool                // true once WithAuthorizer was called, even with nil
	sandboxAuthorizer     security.Authorizer // set by WithSandbox (always composed last)

	namespace         *environment.Namespace // pre-built namespace (via WithNamespace)
	resolverFactories []resolverFactory      // source file resolver chain (via WithSourceFS, WithSourceOS)
	envMap            map[string]string      // virtual env vars (via WithEnv, WithEnvMap)

	// contractEnforcement installs type-checking validators on primitives
	// whose specs declare ParamTypes. Enabled via WithContractEnforcement.
	contractEnforcement bool

	// lossyConversionsAllowed permits FFI converters to silently truncate
	// Scheme numerics into fixed-precision Go types (float64, complex128).
	// Enabled via WithLossyConversionsAllowed. Captured into each FFI
	// closure at RegisterFunc time.
	lossyConversionsAllowed bool

	// coverageCollector, when non-nil, receives every NativeTemplate produced
	// by the compiler so per-s-expression execution can be tracked.
	coverageCollector *coverage.Collector

	// immutableTopLevel enables top-level-define immutability in the user program: a
	// defined-once, never-set!-in-unit top-level define becomes rebind-stable and a later
	// set! of it is rejected. ON by default (set in newEngineConfig) — the sealed-base
	// carve makes redefines of sealed names shadows and scopes enforcement to the user
	// runtime, so it no longer breaks the stdlib. WithMutableTopLevel() opts out;
	// propagated to the Namespace in NewEngine/NewNamespace.
	immutableTopLevel bool
}

// resolverFactory creates a FileResolver given the runtime environment.
// Used internally by WithSourceFS and WithSourceOS to build the resolver chain.
type resolverFactory func(*environment.EnvironmentFrame) compilation.FileResolver

// EngineOption configures an Engine.
type EngineOption func(*engineConfig)

// WithRegistry uses a custom registry instead of the default.
// When set, core primitives are NOT automatically added.
func WithRegistry(r *registry.Registry) EngineOption {
	return func(cfg *engineConfig) {
		cfg.registry = r
	}
}

// WithExtension adds an extension to the engine.
func WithExtension(ext registry.Extension) EngineOption {
	return func(cfg *engineConfig) {
		cfg.extensions = append(cfg.extensions, ext)
	}
}

// WithExtensions adds multiple extensions to the engine.
func WithExtensions(exts ...registry.Extension) EngineOption {
	return func(cfg *engineConfig) {
		cfg.extensions = append(cfg.extensions, exts...)
	}
}

// WithContractEnforcement enables runtime type validation for primitives
// that declare ParamTypes contracts. When enabled, each contracted primitive
// validates its arguments against declared types before calling the
// implementation and returns a typed error on mismatch.
//
// Disabled by default. Intended as a correctness-verification aid for
// extension authors — production extensions should perform their own
// argument checks (e.g., via helpers.RequireArg) rather than depend on
// this option, since enabling it adds a per-call validator invocation.
func WithContractEnforcement() EngineOption {
	return func(cfg *engineConfig) {
		cfg.contractEnforcement = true
	}
}

// WithLossyConversionsAllowed permits FFI converters to silently
// truncate when converting Scheme numerics to fixed-precision Go
// types (float64, complex128). When set, *BigFloat with magnitude
// exceeding float64 range converts to ±math.Inf(0) without error;
// *Rational with non-representable denominators rounds via
// (*big.Rat).Float64 with the loss bit discarded; *BigComplex
// imaginary/real components each may truncate independently.
//
// Default (option not set): the FFI converter returns
// werr.ErrLossyConversion (wrapped, with direction info) when any
// precision loss would occur. This is the "fail loud" discipline —
// opt-in is required to suppress.
//
// The option is per-engine; the flag is captured into each FFI
// closure at RegisterFunc time, so calling
// WithLossyConversionsAllowed after some functions have already
// registered does NOT change their behavior.
func WithLossyConversionsAllowed() EngineOption {
	return func(cfg *engineConfig) {
		cfg.lossyConversionsAllowed = true
	}
}

// WithMaxCallDepth sets the maximum recursion depth for the VM.
// When the continuation stack exceeds this depth, ErrCallDepthExceeded is returned.
// A value of 0 means unlimited (no depth check). Negative values are clamped
// to 0 and therefore also mean unlimited (matches WithInlineThreshold). When
// not called, the engine uses DefaultMaxCallDepth (10000).
func WithMaxCallDepth(n int) EngineOption {
	return func(cfg *engineConfig) {
		if n < 0 {
			n = 0
		}
		cfg.maxCallDepth = n
		cfg.callDepthSet = true
	}
}

// WithMaxContinuationDepth sets the maximum nesting depth for re-invoking
// captured continuations. This is SEPARATE from WithMaxCallDepth: a captured
// continuation re-invoked in a non-converging call/cc loop nests Go-stack
// frames, which would otherwise overflow the goroutine stack and abort the
// process; this bound surfaces the pathology as a catchable ErrCallDepthExceeded.
// The Go stack tolerates far deeper nesting than the eval-stack call-depth
// budget, and the default is correspondingly higher (DefaultMaxContinuationDepth).
// A value of 0 means unlimited (no depth check). Negative values are clamped
// to 0 and therefore also mean unlimited. When not called, the engine uses
// DefaultMaxContinuationDepth.
func WithMaxContinuationDepth(n int) EngineOption {
	return func(cfg *engineConfig) {
		if n < 0 {
			n = 0
		}
		cfg.maxContDepth = n
		cfg.contDepthSet = true
	}
}

// WithMaxParseDepth sets the maximum structural nesting depth the parser
// will accept. When input nests deeper, ErrParseDepthExceeded is returned
// instead of crashing with a fatal Go stack overflow. A value of 0 means
// unlimited (negative values are clamped to 0). When not called, the parser
// uses DefaultMaxParseDepth (10000).
//
// Scope: this bound is threaded onto the engine's own parse entry points —
// Parse, ParseWithSource, ReadExpression, ReadExpressions, EvalMultiple,
// EvalProgram, and file/-e execution. Parsing triggered from within running
// Scheme — (read ...), (read-syntax ...), (eval ...), (compile ...) — uses
// DefaultMaxParseDepth regardless of this option, because the primitive layer
// has no channel to the engine's configured value (mirrors WithMaxExpandDepth).
// Those paths are still protected from the fatal stack overflow by the default;
// they are simply not retunable per-engine.
func WithMaxParseDepth(n int) EngineOption {
	return func(cfg *engineConfig) {
		if n < 0 {
			n = 0
		}
		cfg.maxParseDepth = n
		cfg.parseDepthSet = true
	}
}

// WithMaxExpandDepth sets the maximum structural recursion depth the macro
// expander will accept. The parser already bounds nesting in textual input
// (see WithMaxParseDepth); this bounds programmatically-constructed deep syntax
// — macro output, datum->syntax, and quasiquote — which reaches the expander
// without passing through the parser. When expansion nests deeper,
// ErrExpandDepthExceeded is returned instead of crashing with a fatal Go stack
// overflow. A value of 0 means unlimited (negative values are clamped to 0).
// When not called, the expander uses DefaultMaxExpandDepth (50000).
//
// Scope: this bound applies to expansion of top-level program text run through
// the engine. Expansion triggered from within running Scheme — (eval ...),
// (load ...), (compile ...), (expand ...) — always uses DefaultMaxExpandDepth
// regardless of this option, because the primitive layer has no channel to the
// engine's configured value. Those paths are still protected from the fatal
// stack overflow (by the default); they are simply not retunable per-engine.
func WithMaxExpandDepth(n int) EngineOption {
	return func(cfg *engineConfig) {
		if n < 0 {
			n = 0
		}
		cfg.maxExpandDepth = n
		cfg.expandDepthSet = true
	}
}

// WithMaxStackSize sets the maximum eval stack size for the VM.
// When the eval stack exceeds this size, ErrStackOverflow is returned.
// This is opt-in: a value of 0 means unlimited (no stack size check).
// There is no default — when not called, the stack is unlimited.
func WithMaxStackSize(n uint64) EngineOption {
	return func(cfg *engineConfig) {
		cfg.maxStackSize = n
	}
}

// WithInlineThreshold sets the maximum body length (in top-level expressions)
// for procedure inlining. Procedures with bodies longer than this threshold
// are not inlined. A value of 0 disables inlining entirely. When not called,
// the engine uses compilation.DefaultInlineThreshold (5).
func WithInlineThreshold(n int) EngineOption {
	return func(cfg *engineConfig) {
		if n < 0 {
			n = 0
		}
		cfg.inlineThreshold = n
		cfg.inlineThresholdSet = true
	}
}

// WithLibraryPaths enables the R7RS library system (define-library / import)
// and configures directories to search for .sld library files.
//
// Without this option, (import ...) raises a configuration error.
//
// Paths are searched in order: user-supplied paths first, then the default
// ("."). The embedded standard library is served by the FileResolver chain
// (e.g. WithSourceFS(stdlib.FS)), not a search path. An empty call
// WithLibraryPaths() enables library support with the default only.
//
// Example:
//
//	eng, err := wile.NewEngine(ctx,
//	    wile.WithLibraryPaths("/app/libs", "./vendor"),
//	)
//	// search order: /app/libs, ./vendor, .
func WithLibraryPaths(paths ...string) EngineOption {
	return func(cfg *engineConfig) {
		cfg.libraryEnabled = true
		cfg.libraryPaths = paths
	}
}

// WithImportObserver sets a callback that is invoked each time a library is
// imported. The observer is read-only — it cannot influence the import.
// Requires WithLibraryPaths to be effective (no libraries loaded without it).
func WithImportObserver(obs func(LibraryImportEvent)) EngineOption {
	return func(cfg *engineConfig) {
		cfg.importObserver = obs
	}
}

// WithoutCore creates an engine with an empty registry — no core primitives
// (arithmetic, pairs, control flow, etc.) are added. Extensions added via
// WithExtension are still applied.
//
// This is useful for building minimal engines where only specific extensions
// are needed, or for testing extension isolation.
func WithoutCore() EngineOption {
	return func(cfg *engineConfig) {
		cfg.registry = registry.NewRegistry()
	}
}

// WithAuthorizer sets the Authorizer for the engine. The authorizer is
// injected into every context passed to Eval, Compile, Run, and Call,
// gating runtime primitives and compile-time code loading.
//
// An explicit WithAuthorizer takes precedence over any profile's built-in
// authorizer regardless of option order: WithAuthorizer(a) and WithProfile(p)
// resolve to a (then intersected with any WithSandbox layer) no matter which
// is written first. Passing nil is meaningful — it opens the engine, overriding
// a profile authorizer (symmetric with WithEnvMap(nil)).
//
// Without this option, all operations are allowed (open by default) unless a
// profile or sandbox supplies an authorizer. The authorizer is immutable after
// engine construction.
//
// Example:
//
//	eng, err := wile.NewEngine(ctx,
//	    wile.WithAuthorizer(security.ReadOnly()),
//	)
func WithAuthorizer(auth security.Authorizer) EngineOption {
	return func(cfg *engineConfig) {
		cfg.explicitAuthorizer = auth
		cfg.explicitAuthorizerSet = true
	}
}

// resolveAuthorizer computes the engine's effective authorizer from the three
// intent-specific config fields, order-independently. Precedence: an explicit
// WithAuthorizer (even nil) overrides a profile's built-in authorizer; a
// WithSandbox layer is always intersected on top via security.All. Returns nil
// when no authorizer applies (open by default).
func (p *engineConfig) resolveAuthorizer() security.Authorizer {
	base := p.profileAuthorizer
	if p.explicitAuthorizerSet {
		base = p.explicitAuthorizer
	}
	if p.sandboxAuthorizer == nil {
		return base
	}
	if base == nil {
		return p.sandboxAuthorizer
	}
	return security.All(base, p.sandboxAuthorizer)
}

// WithNamespace uses a pre-built namespace instead of building one from
// extension options. When set, registry/extension/core options are ignored
// by NewEngine (they were already applied when the namespace was created).
//
// This enables sharing a namespace across engines or pre-configuring
// namespaces with specific capabilities.
func WithNamespace(ns *environment.Namespace) EngineOption {
	return func(cfg *engineConfig) {
		cfg.namespace = ns
	}
}

// WithSourceFS adds a virtual filesystem layer to the source file
// resolver chain. Multiple calls add layers searched in call order.
// When no resolver options are used, the engine defaults to the OS
// filesystem. Once any resolver option is used (WithSourceFS or
// WithSourceOS), only the explicitly configured resolvers are active.
//
// Bootstrap macros are unaffected — they always load from the embedded
// bootstrap filesystem.
//
// Example:
//
//	//go:embed scheme
//	var schemeFS embed.FS
//
//	eng, err := wile.NewEngine(ctx,
//	    wile.WithSourceFS(schemeFS),  // searched first
//	    wile.WithSourceOS(),          // OS filesystem searched last
//	)
func WithSourceFS(fsys fs.FS) EngineOption {
	if fsys == nil {
		panic(werr.WrapForeignErrorf(werr.ErrEngineInit, "WithSourceFS: fsys must not be nil"))
	}
	return func(cfg *engineConfig) {
		cfg.resolverFactories = append(cfg.resolverFactories, func(env *environment.EnvironmentFrame) compilation.FileResolver {
			return compilation.NewFSFileResolver(fsys, env)
		})
	}
}

// WithSourceOS adds the OS filesystem to the source file resolver chain.
// This is typically called last so that virtual filesystems are searched
// first. When no resolver options are used, the engine defaults to the
// OS filesystem; WithSourceOS is only needed when building an explicit
// chain with WithSourceFS.
//
// Example:
//
//	eng, err := wile.NewEngine(ctx,
//	    wile.WithSourceFS(embedFS),  // virtual FS first
//	    wile.WithSourceOS(),         // OS fallback last
//	)
func WithSourceOS() EngineOption {
	return func(cfg *engineConfig) {
		cfg.resolverFactories = append(cfg.resolverFactories, func(env *environment.EnvironmentFrame) compilation.FileResolver {
			return compilation.NewOSFileResolver(env)
		})
	}
}

// WithEnv adds a single virtual environment variable.
// When any virtual env var is set, the envvars extension reads from
// the virtual map instead of os.Getenv.
func WithEnv(key, value string) EngineOption {
	return func(cfg *engineConfig) {
		if cfg.envMap == nil {
			cfg.envMap = make(map[string]string)
		}
		cfg.envMap[key] = value
	}
}

// WithCoverage enables Scheme-side line coverage collection. After each
// compilation, the engine registers the resulting top-level template
// and every sub-template reachable via its literals pool with the
// given collector. Per-s-expression execution is then aggregated into
// the collector's Entries.
//
// Zero hot-path cost when not set (nil check in VM dispatch).
func WithCoverage(c *coverage.Collector) EngineOption {
	return func(cfg *engineConfig) {
		cfg.coverageCollector = c
	}
}

// WithImmutableTopLevel selects top-level-define immutability in the user program. A
// top-level define that is defined-once and never set! within its compilation unit is
// marked rebind-stable (BindingMeta.Stable), and a subsequent set! of such a binding is
// rejected with ErrImmutableBinding. This is now the DEFAULT (newEngineConfig); the
// option remains as an explicit, redundant selector for source compatibility.
//
// This is a documented deviation from strict R7RS §4.1.6/§5.3 (which permit top-level
// set!/redefinition); it unlocks the frame-reclamation optimizer's top-level payoff
// (sibling escape-gated plan) — the "compile for speed" contract used by sealed-module
// Schemes. Use WithMutableTopLevel() to opt out.
//
// Enforcement is scoped to the engine's own user runtime global (the layered-environment
// sealed-base carve): a re-define of a sealed primitive/stdlib name is a child-frame
// shadow rather than a rejected rebind, and user-loaded LIBRARIES stay mutable — a
// library's cross-form (define x)/(set! x) is permitted. See docs/reference/r7rs-differences.md.
func WithImmutableTopLevel() EngineOption {
	return func(cfg *engineConfig) {
		cfg.immutableTopLevel = true
	}
}

// WithMutableTopLevel selects strict R7RS mutable/redefinable top-level bindings,
// the inverse of WithImmutableTopLevel. It exists as an explicit opt-out so callers
// can request mutable semantics independent of the engine default (which Phase 4 of
// the layered-environment work flips to immutable). Opting out forfeits the
// frame-reclaim GC win for user recursion.
func WithMutableTopLevel() EngineOption {
	return func(cfg *engineConfig) {
		cfg.immutableTopLevel = false
	}
}

// WithEnvMap sets the complete virtual environment variable map.
// Replaces any previously set virtual env vars.
//
// Passing nil clears the virtual env map so envvars primitives fall back to
// os.Getenv (still gated by the authorizer). This is symmetric with
// WithAuthorizer(nil): the zero value means "no restriction", not "empty
// sandbox". To explicitly sandbox with no visible env, pass an empty map.
//
// Note: when combined with WithProfile(Console) or WithProfile(ConsoleWithLoad),
// option order matters. WithProfile fills in an empty map only if envMap is
// currently nil; a later WithEnvMap(nil) re-nils it and opens the sandbox.
func WithEnvMap(m map[string]string) EngineOption {
	return func(cfg *engineConfig) {
		if m == nil {
			cfg.envMap = nil
			return
		}
		cfg.envMap = make(map[string]string, len(m))
		maps.Copy(cfg.envMap, m)
	}
}
