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
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine/compilation"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/werr"
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

type engineConfig struct {
	registry           *registry.Registry
	extensions         []registry.Extension
	maxCallDepth       uint64
	callDepthSet       bool // true if WithMaxCallDepth was explicitly called
	maxStackSize       uint64
	inlineThreshold    int
	inlineThresholdSet bool // true if WithInlineThreshold was explicitly called
	libraryPaths       []string
	libraryEnabled     bool // true when WithLibraryPaths was called
	importObserver     func(LibraryImportEvent)
	authorizer         security.Authorizer
	namespace          *environment.Namespace // pre-built namespace (via WithNamespace)
	resolverFactories  []resolverFactory      // source file resolver chain (via WithSourceFS, WithSourceOS)
	envMap             map[string]string      // virtual env vars (via WithEnv, WithEnvMap)

	// contractEnforcement installs type-checking validators on primitives
	// whose specs declare ParamTypes. Enabled via WithContractEnforcement.
	contractEnforcement bool

	// coverageCollector, when non-nil, receives every NativeTemplate produced
	// by the compiler so per-s-expression execution can be tracked.
	coverageCollector *coverage.Collector
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

// WithMaxCallDepth sets the maximum recursion depth for the VM.
// When the continuation stack exceeds this depth, ErrCallDepthExceeded is returned.
// A value of 0 means unlimited (no depth check). When not called, the engine
// uses DefaultMaxCallDepth (10000).
func WithMaxCallDepth(n uint64) EngineOption {
	return func(cfg *engineConfig) {
		cfg.maxCallDepth = n
		cfg.callDepthSet = true
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
// Paths are searched in order: user-supplied paths first, then the defaults
// ("." and "./stdlib/lib"). An empty call WithLibraryPaths() enables library support
// with defaults only.
//
// Example:
//
//	eng, err := wile.NewEngine(ctx,
//	    wile.WithLibraryPaths("/app/libs", "./vendor"),
//	)
//	// search order: /app/libs, ./vendor, ., ./stdlib/lib
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
// Without this option, all operations are allowed (open by default).
// The authorizer is immutable after engine construction.
//
// Example:
//
//	eng, err := wile.NewEngine(ctx,
//	    wile.WithAuthorizer(security.ReadOnly()),
//	)
func WithAuthorizer(auth security.Authorizer) EngineOption {
	return func(cfg *engineConfig) {
		cfg.authorizer = auth
	}
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
