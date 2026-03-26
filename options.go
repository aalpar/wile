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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/extensions/files"
	"github.com/aalpar/wile/extensions/gointerop"
	"github.com/aalpar/wile/extensions/introspection"
	"github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/extensions/process"
	"github.com/aalpar/wile/extensions/system"
	"github.com/aalpar/wile/extensions/threads"
	"github.com/aalpar/wile/internal/extensions/all"
	exteval "github.com/aalpar/wile/internal/extensions/eval"
	"github.com/aalpar/wile/internal/extensions/io"
	nsext "github.com/aalpar/wile/internal/extensions/namespace"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/werr"
)

// LibraryImportEvent records what happened when a library was imported.
// See machine.LibraryImportEvent for field documentation.
type LibraryImportEvent = machine.LibraryImportEvent

// Phase constants for LibraryImportEvent.Phase.
// Re-exported from environment for embedder convenience.
const (
	PhaseExpand  = environment.PhaseExpand
	PhaseCompile = environment.PhaseCompile
)

type engineConfig struct {
	registry          *registry.Registry
	extensions        []registry.Extension
	maxCallDepth      uint64
	callDepthSet      bool // true if WithMaxCallDepth was explicitly called
	libraryPaths      []string
	libraryEnabled    bool // true when WithLibraryPaths was called
	importObserver    func(LibraryImportEvent)
	authorizer        security.Authorizer
	namespace         *environment.Namespace // pre-built namespace (via WithNamespace)
	resolverFactories []resolverFactory      // source file resolver chain (via WithSourceFS, WithSourceOS)
}

// resolverFactory creates a FileResolver given the runtime environment.
// Used internally by WithSourceFS and WithSourceOS to build the resolver chain.
type resolverFactory func(*environment.EnvironmentFrame) machine.FileResolver

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
		cfg.resolverFactories = append(cfg.resolverFactories, func(env *environment.EnvironmentFrame) machine.FileResolver {
			return machine.NewFSFileResolver(fsys, env)
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
		cfg.resolverFactories = append(cfg.resolverFactories, func(env *environment.EnvironmentFrame) machine.FileResolver {
			return machine.NewOSFileResolver(env)
		})
	}
}

// SafeExtensions returns engine options that add extensions suitable for
// sandboxed engines: io, exceptions, math, introspection, and the safe
// subset of all (records, promises, strings, characters).
//
// These provide R7RS functionality without filesystem, eval, system, Go
// interop, or threading access. Core primitives are still added by default
// unless WithoutCore is also used.
//
// Principle of Least Authority (Saltzer & Schroeder 1975).
//
//	authority(engine) = ∪ { caps(ext) : ext ∈ extensions }
//
//	SafeExtensions() ⊂ AllExtensions() — the safe set excludes
//	filesystem, eval, system, Go interop, and threading capabilities.
//	WithoutCore() produces authority(engine) = ∅.
//
//	Invariant: absent capabilities produce compile-time errors, not
//	  runtime checks. If a name has no binding, compilation fails.
//	  No runtime check can be bypassed because no runtime check exists.
//	Constrains: NewEngine (applies the registry), LibraryEnvFactory
//	  (closes over registry — transitive confinement per Lampson 1973).
//	Constrained by: Registry.Without/WithoutCategory (capability
//	  attenuation — derived registries never gain authority).
//
// See BIBLIOGRAPHY.md "Saltzer & Schroeder".
//
// Example:
//
//	eng, err := wile.NewEngine(ctx,
//	    append(wile.SafeExtensions(),
//	        wile.WithLibraryPaths("./stdlib/lib"),
//	    )...,
//	)
func SafeExtensions() []EngineOption {
	return []EngineOption{
		WithExtension(io.Extension),
		WithExtension(math.Extension),
		WithExtension(introspection.Extension),
		WithExtension(all.SafeExtension),
	}
}

// WithSafeExtensions adds the safe extension set to the engine.
// This is a convenience wrapper around SafeExtensions for the common case
// where no additional options need to be appended.
//
// Example:
//
//	eng, err := wile.NewEngine(ctx, wile.WithSafeExtensions())
func WithSafeExtensions() EngineOption {
	return func(cfg *engineConfig) {
		for _, opt := range SafeExtensions() {
			opt(cfg)
		}
	}
}

// AllExtensions returns the complete set of engine options that add every
// available extension. This matches the extension set loaded by the CLI
// binary (io, files, math, introspection, eval, namespace, threads,
// gointerop, all, system, process).
//
// Use [WithAllExtensions] when no additional options need to be appended.
//
// Example:
//
//	eng, err := wile.NewEngine(ctx,
//	    append(wile.AllExtensions(),
//	        wile.WithLibraryPaths("./stdlib/lib"),
//	    )...,
//	)
func AllExtensions() []EngineOption {
	return []EngineOption{
		WithExtension(io.Extension),
		WithExtension(files.Extension),
		WithExtension(math.Extension),
		WithExtension(introspection.Extension),
		WithExtension(exteval.Extension),
		WithExtension(nsext.Extension),
		WithExtension(threads.Extension),
		WithExtension(gointerop.Extension),
		WithExtension(all.Extension),
		WithExtension(system.Extension),
		WithExtension(process.Extension),
	}
}

// WithAllExtensions adds every available extension to the engine.
// This is a convenience wrapper around AllExtensions for the common case
// where no additional options need to be appended.
//
// Example:
//
//	eng, err := wile.NewEngine(ctx, wile.WithAllExtensions())
func WithAllExtensions() EngineOption {
	return func(cfg *engineConfig) {
		for _, opt := range AllExtensions() {
			opt(cfg)
		}
	}
}
