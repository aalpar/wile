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

// strictLevel is the monotone narrowing ladder for the engine's VISIBLE
// top-level surface. Higher means narrower, and every option in the family
// combines by max, so ordering options can never un-narrow a surface.
//
// The type is unexported because no caller outside this package names a level:
// each level has its own option constructor, and the CLI selects between those.
type strictLevel int

const (
	// strictLevelOff binds the whole registry at the top level. Default.
	strictLevelOff strictLevel = iota
	// strictLevelCore binds only the core primitives and core bootstrap macros.
	// Selected by WithStrictNamespace.
	strictLevelCore
	// strictLevelNoBindings binds nothing. Selected by WithoutAmbientBindings.
	strictLevelNoBindings
)

// Phase is a level in one owner's macro tower, counted RELATIVE to that owner's
// own runtime (0) rather than as an absolute stage of compilation. The named
// constants below are the levels the top level occupies, not the range: the
// tower climbs past them whenever a transformer body defines a macro of its own.
// Re-exported from environment for embedder convenience; see environment.Phase
// for the full model.
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
	registry           *registry.PrimitiveRegistry
	extensions         []registry.Extension
	maxCallDepth       int
	callDepthSet       bool // true if WithMaxCallDepth was explicitly called
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

	// profileSet records that WithProfile was called AT ALL, independent of
	// what the profile writes. Tiny registers no extensions and no authorizer,
	// so it leaves no other trace — without this flag rejectNamespaceConsumedOptions
	// would refuse WithProfile(Console) and wave WithProfile(Tiny) through.
	profileSet bool

	resolverFactories []resolverFactory // source file resolver chain (via WithSourceFS, WithSourceOS)
	envMap            map[string]string // virtual env vars (via WithEnv, WithEnvMap)
	// envMapSet records that WithEnv/WithEnvMap was called. Distinct from
	// envMap != nil because WithEnvMap(nil) deliberately re-nils the map,
	// which is a REQUEST (open the sandbox), not an absence of one.
	envMapSet bool

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
	// topLevelMutabilitySet records that WithImmutableTopLevel or
	// WithMutableTopLevel was called. The field above cannot report it:
	// its default is true, so an explicit WithImmutableTopLevel() is
	// indistinguishable from never asking.
	topLevelMutabilitySet bool

	// strictLevel selects how much of the registry the VISIBLE top-level surface
	// is bound from. The profile's registry (REGISTERED) is unchanged at every
	// level and remains the security boundary; strictness only decides how much
	// of it is pre-bound versus reachable via (import …).
	//
	//	strictLevelOff        VISIBLE = REGISTERED (default)
	//	strictLevelCore       core primitives + core bootstrap macros, so the
	//	                      visible surface equals a Tiny engine's and
	//	                      (import (scheme r5rs)) layers over a core baseline
	//	strictLevelNoBindings nothing bound; only phase handlers remain reachable
	//	                      (lambda/let/if/define/import — see WithoutAmbientBindings)
	//
	// Set by WithStrictNamespace and WithoutAmbientBindings, which combine by
	// MAX so narrowing stays commutative and monotone.
	strictLevel strictLevel

	// dialect, when non-nil, customizes the per-engine forms registry at engine
	// origin (bootstrapNamespace): the engine forks the R7RS-default clone and
	// hands it to the dialect's InstallForms to add/remove/rename special forms.
	// Set by WithDialect (last wins). See dialect.go.
	dialect Dialect
}

// resolverFactory creates a FileResolver given the runtime environment.
// Used internally by WithSourceFS and WithSourceOS to build the resolver chain.
type resolverFactory func(*environment.EnvironmentFrame) compilation.FileResolver

// EngineOption configures an Engine. The method is unexported, so the set of
// implementations is closed to this package — as it already effectively was,
// since engineConfig is unexported.
type EngineOption interface {
	applyEngine(*engineConfig)
}

// EngineOnlyOption is the subset of EngineOption that NewEngine reads AFTER the
// namespace exists, so it applies equally to a pre-built one. It is the element
// type of NewEngineWithNamespace's variadic: a namespace-consumed option does
// not implement it, so passing one there is a compile error rather than a
// silent drop.
//
// Embedding EngineOption keeps every engine-only option passable to NewEngine
// and to a []EngineOption literal.
type EngineOnlyOption interface {
	EngineOption
	engineOnly()
}

// namespaceConsumedOption adapts a config mutator that only bootstrapNamespace
// can apply. It deliberately does NOT implement EngineOnlyOption.
type namespaceConsumedOption func(*engineConfig)

func (f namespaceConsumedOption) applyEngine(cfg *engineConfig) {
	f(cfg)
}

// engineOnlyOption adapts a config mutator NewEngine reads after the namespace
// exists.
type engineOnlyOption func(*engineConfig)

func (f engineOnlyOption) applyEngine(cfg *engineConfig) {
	f(cfg)
}

func (engineOnlyOption) engineOnly() {
}

// WithRegistry uses a custom registry instead of the default.
// When set, core primitives are NOT automatically added.
func WithRegistry(r *registry.PrimitiveRegistry) EngineOption {
	return namespaceConsumedOption(func(cfg *engineConfig) {
		cfg.registry = r
	})
}

// WithExtension adds an extension to the engine.
func WithExtension(ext registry.Extension) EngineOption {
	return namespaceConsumedOption(func(cfg *engineConfig) {
		cfg.extensions = append(cfg.extensions, ext)
	})
}

// WithExtensions adds multiple extensions to the engine.
func WithExtensions(exts ...registry.Extension) EngineOption {
	return namespaceConsumedOption(func(cfg *engineConfig) {
		cfg.extensions = append(cfg.extensions, exts...)
	})
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
func WithContractEnforcement() EngineOnlyOption {
	return engineOnlyOption(func(cfg *engineConfig) {
		cfg.contractEnforcement = true
	})
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
func WithLossyConversionsAllowed() EngineOnlyOption {
	return engineOnlyOption(func(cfg *engineConfig) {
		cfg.lossyConversionsAllowed = true
	})
}

// WithMaxCallDepth sets the maximum recursion depth for the VM.
// When the continuation stack exceeds this depth, ErrCallDepthExceeded is returned.
// A value of 0 means unlimited (no depth check). Negative values are clamped
// to 0 and therefore also mean unlimited (matches WithInlineThreshold). When
// not called, the engine uses DefaultMaxCallDepth (10000).
func WithMaxCallDepth(n int) EngineOnlyOption {
	return engineOnlyOption(func(cfg *engineConfig) {
		if n < 0 {
			n = 0
		}
		cfg.maxCallDepth = n
		cfg.callDepthSet = true
	})
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
func WithMaxParseDepth(n int) EngineOnlyOption {
	return engineOnlyOption(func(cfg *engineConfig) {
		if n < 0 {
			n = 0
		}
		cfg.maxParseDepth = n
		cfg.parseDepthSet = true
	})
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
func WithMaxExpandDepth(n int) EngineOnlyOption {
	return engineOnlyOption(func(cfg *engineConfig) {
		if n < 0 {
			n = 0
		}
		cfg.maxExpandDepth = n
		cfg.expandDepthSet = true
	})
}

// WithMaxStackSize sets the maximum eval stack size for the VM.
// When the eval stack exceeds this size, ErrStackOverflow is returned.
// This is opt-in: a value of 0 means unlimited (no stack size check).
// There is no default — when not called, the stack is unlimited.
func WithMaxStackSize(n uint64) EngineOnlyOption {
	return engineOnlyOption(func(cfg *engineConfig) {
		cfg.maxStackSize = n
	})
}

// WithInlineThreshold sets the maximum body length (in top-level expressions)
// for procedure inlining. Procedures with bodies longer than this threshold
// are not inlined. A value of 0 disables inlining entirely. When not called,
// the engine uses compilation.DefaultInlineThreshold (5).
func WithInlineThreshold(n int) EngineOnlyOption {
	return engineOnlyOption(func(cfg *engineConfig) {
		if n < 0 {
			n = 0
		}
		cfg.inlineThreshold = n
		cfg.inlineThresholdSet = true
	})
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
func WithLibraryPaths(paths ...string) EngineOnlyOption {
	return engineOnlyOption(func(cfg *engineConfig) {
		cfg.libraryEnabled = true
		cfg.libraryPaths = paths
	})
}

// WithImportObserver sets a callback that is invoked each time a library is
// imported. The observer is read-only — it cannot influence the import.
// Requires WithLibraryPaths to be effective (no libraries loaded without it).
func WithImportObserver(obs func(LibraryImportEvent)) EngineOnlyOption {
	return engineOnlyOption(func(cfg *engineConfig) {
		cfg.importObserver = obs
	})
}

// WithoutCore creates an engine with an empty registry — no core primitives
// (arithmetic, pairs, control flow, etc.) are added. Extensions added via
// WithExtension are still applied.
//
// This is useful for building minimal engines where only specific extensions
// are needed, or for testing extension isolation.
func WithoutCore() EngineOption {
	return namespaceConsumedOption(func(cfg *engineConfig) {
		cfg.registry = registry.NewRegistry()
	})
}

// WithAuthorizer sets the Authorizer for the engine. The authorizer is
// recorded on the engine's namespace at construction and consulted by gate
// sites via MachineContext.Authorizer(), gating runtime primitives and
// compile-time code loading.
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
	return namespaceConsumedOption(func(cfg *engineConfig) {
		cfg.explicitAuthorizer = auth
		cfg.explicitAuthorizerSet = true
	})
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

// WithSourceFS adds a virtual filesystem layer to the source file
// resolver chain. Multiple calls add layers searched in call order.
// When no resolver options are used, the engine defaults to the OS
// filesystem. Once any resolver option is used (WithSourceFS or
// WithSourceOS), only the explicitly configured resolvers are active.
//
// Bootstrap macros are unaffected — they always load from the embedded
// bootstrap filesystem.
//
// WithSourceFS panics if fsys is nil: a nil filesystem is a programming error,
// not a runtime condition an embedder can recover from.
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
func WithSourceFS(fsys fs.FS) EngineOnlyOption {
	if fsys == nil {
		panic(werr.WrapForeignErrorf(werr.ErrEngineInit, "WithSourceFS: fsys must not be nil"))
	}
	return engineOnlyOption(func(cfg *engineConfig) {
		cfg.resolverFactories = append(cfg.resolverFactories, func(env *environment.EnvironmentFrame) compilation.FileResolver {
			return compilation.NewFSFileResolver(fsys, env)
		})
	})
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
func WithSourceOS() EngineOnlyOption {
	return engineOnlyOption(func(cfg *engineConfig) {
		cfg.resolverFactories = append(cfg.resolverFactories, func(env *environment.EnvironmentFrame) compilation.FileResolver {
			return compilation.NewOSFileResolver(env)
		})
	})
}

// WithEnv adds a single virtual environment variable.
// When any virtual env var is set, the envvars extension reads from
// the virtual map instead of os.Getenv.
func WithEnv(key, value string) EngineOption {
	return namespaceConsumedOption(func(cfg *engineConfig) {
		cfg.envMapSet = true
		if cfg.envMap == nil {
			cfg.envMap = make(map[string]string)
		}
		cfg.envMap[key] = value
	})
}

// WithCoverage enables Scheme-side line coverage collection. After each
// compilation, the engine registers the resulting top-level template
// and every sub-template reachable via its literals pool with the
// given collector. Per-s-expression execution is then aggregated into
// the collector's Entries.
//
// Zero hot-path cost when not set (nil check in VM dispatch).
func WithCoverage(c *coverage.Collector) EngineOnlyOption {
	return engineOnlyOption(func(cfg *engineConfig) {
		cfg.coverageCollector = c
	})
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
	return namespaceConsumedOption(func(cfg *engineConfig) {
		cfg.immutableTopLevel = true
		cfg.topLevelMutabilitySet = true
	})
}

// WithMutableTopLevel selects strict R7RS mutable/redefinable top-level bindings,
// the inverse of WithImmutableTopLevel. It exists as an explicit opt-out so callers
// can request mutable semantics independent of the engine default (which Phase 4 of
// the layered-environment work flips to immutable). Opting out forfeits the
// frame-reclaim GC win for user recursion.
func WithMutableTopLevel() EngineOption {
	return namespaceConsumedOption(func(cfg *engineConfig) {
		cfg.immutableTopLevel = false
		cfg.topLevelMutabilitySet = true
	})
}

// WithStrictNamespace narrows the engine's visible top-level surface to CORE
// ONLY: the profile's extension primitives are REGISTERED (so libraries can
// import them) but are NOT pre-bound at the top level. Reach them with
// (import …), which needs WithLibraryPaths — off by default, and without it
// every import fails with werr.ErrLibraryConfiguration. The precondition is
// identical at both levels; it bites harder at level 2, where there is no
// ambient surface to fall back to (see WithoutAmbientBindings' first cost).
// Core primitives and core syntax remain visible, so the visible
// surface matches a Tiny engine's; the full profile registry is still used for
// library environments, so a layered import such as (import (scheme r5rs))
// resolves over the core-only baseline.
//
// Orthogonal to WithProfile/WithSandbox/WithAuthorizer; composes commutatively.
// The profile (or explicit WithExtension set) remains the security boundary —
// strict mode never widens what is reachable, it only withholds it from the top
// level until imported. Off by default.
//
// The visible surface is carved when the namespace is built, so strictness must
// be set at namespace-creation time. Like WithRegistry/WithExtension/WithoutCore,
// this option is namespace-consumed and so cannot be passed to
// NewEngineWithNamespace at all (a pre-built namespace is authoritative for its
// own top level) — bake strictness in at NewNamespace.
// It is incompatible with WithRegistry/WithoutCore (which supply a custom or
// coreless registry): strict mode derives its visible surface from the default
// core registry, so the combination is rejected at construction.
func WithStrictNamespace() EngineOption {
	return namespaceConsumedOption(func(cfg *engineConfig) {
		cfg.strictLevel = max(cfg.strictLevel, strictLevelCore)
	})
}

// WithoutAmbientBindings narrows the engine's visible top-level surface all the
// way: NOTHING is pre-bound. It is one step past WithStrictNamespace, which
// still pre-binds the core surface, and it composes with it by max — applying
// both in either order yields this level.
//
// What survives is not the empty set: exactly 41 names, whatever the profile,
// pinned name-by-name by TestNoAmbientBindingsBoundSet. Two mechanisms put them
// there. 38 are PHASE HANDLERS, not bindings: registered by the compiler, living
// in sealed frames that ordinary value resolution never sees, and
// never sourced from a registry — so withholding the registry cannot withhold
// them. The other three (unless, guard, guard-aux) are syntax-rules definitions
// in core.LateBootstrapMacroSource, which LoadBootstrapCore loads
// unconditionally rather than through Registry.MacroSources(); that, and not any
// phase-handler property, is why when and unless land on opposite sides of this
// floor.
//
// BOUND and USABLE are different partitions, and they cross in both directions:
// syntax-rules is usable and is NOT one of the 41 (define-syntax recognizes it
// inline), while guard is one of the 41 and is unusable. The usability
// partition, by shape here and name-by-name in docs/embedding/api-design.md:
//
//   - Usable: the value forms (lambda, if, quote, define, begin, set!, the
//     let and define-syntax families, cond-expand, case-lambda), the library
//     forms (define-library, library, import), the expand-phase family (syntax,
//     syntax-case, er-macro-transformer, begin-for-syntax, define-for-syntax,
//     eval-when, meta), plus syntax-error and with-continuation-mark. A CONSTANT
//     quasiquote or quasisyntax template belongs here too.
//   - Resolves, unusable: a form whose expansion CALLS a primitive resolves and
//     then fails at the call. quasiquote is the one to know — `(1 2) works, but
//     the moment an unquote appears the expansion emits list and dies.
//     quasisyntax with an unsyntax (needs datum->syntax), with-syntax (list),
//     unless (not) and guard (call-with-exit) are the same shape.
//   - Auxiliary keywords, bound so the expander recognizes them positionally and
//     an error standalone: unquote, unsyntax, their -splicing forms, export,
//     guard-aux, with-binding-scope.
//   - include and include-ci resolve, and want a FileResolver rather than a
//     registry — a different axis from this option.
//
// Everything else the registry supplies — the core primitives and the bootstrap
// macros cond, case, when, and, or, do, define-record-type, let-values, delay,
// parameterize — is simply unbound. This option is strict for PROCEDURES AND
// DERIVED SYNTAX; do not read it as "R7RS-strict" unqualified.
//
// Nothing is withheld permanently: import is a phase handler, and REGISTERED is
// untouched, so a program reaches any part of the profile it wants by importing
// it, starting with (import (scheme base)). What this option buys is that the
// program must SAY SO — no ambient dependency goes undeclared. It is an
// explicitness property (a CI or portability-audit property), not confinement;
// the profile remains the security boundary at every level.
//
// Three costs, all real:
//
//   - The import route exists only if WithLibraryPaths was called. It is off by
//     default, so WithProfile(Small) plus this option alone — the shortest
//     spelling of level 2, and the one the ladder table shows — constructs
//     without error and then fails every (import …) with
//     werr.ErrLibraryConfiguration. That is confinement, reached by the most
//     likely invocation rather than by the two exotic ones below, and the
//     configuration is silent even though the failure is loud. Reaching the
//     stdlib further needs a resolver that serves it (WithSourceFS(stdlib.FS)).
//     The pair is deliberately not rejected at construction: Engine.Define and
//     Engine.RegisterPrimitive make "empty top level, no library system, host
//     supplies every binding from Go" a legitimate DSL host.
//   - A library environment is engine-sized, so every import is expensive:
//     (import (scheme base)) alone measures ~9.4 ms, against ~3.9 ms to build a
//     whole Small engine. A program on this level pays that at least once, and
//     an eight-library R7RS preamble costs ~59 ms / ~7.9 MB heap.
//   - It is usable on Small and KitchenSink only. Tiny cannot import
//     (scheme base) (64 of its exports are unregistered there) and
//     Console/ConsoleWithLoad are denied code:load on the stdlib path. Both
//     failures pre-date this option and reproduce without it, but at this level
//     there is no ambient surface to fall back to. WithSandbox denies code:load
//     for the same reason: combined with this option it leaves a program on the
//     phase-handler floor with no route off it, which IS confinement — a real
//     configuration, but not the one the paragraph above describes.
//
// Carries the same constraints as WithStrictNamespace: set it at
// namespace-creation time (it cannot be passed to NewEngineWithNamespace), and
// it is incompatible with WithRegistry/WithoutCore. Off by default.
func WithoutAmbientBindings() EngineOption {
	return namespaceConsumedOption(func(cfg *engineConfig) {
		cfg.strictLevel = max(cfg.strictLevel, strictLevelNoBindings)
	})
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
	return namespaceConsumedOption(func(cfg *engineConfig) {
		cfg.envMapSet = true
		if m == nil {
			cfg.envMap = nil
			return
		}
		cfg.envMap = make(map[string]string, len(m))
		maps.Copy(cfg.envMap, m)
	})
}
