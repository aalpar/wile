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
	"github.com/aalpar/wile/extensions/exceptions"
	"github.com/aalpar/wile/extensions/introspection"
	"github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/internal/extensions/all"
	"github.com/aalpar/wile/internal/extensions/io"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/security"
)

// LibraryImportEvent records what happened when a library was imported.
// See machine.LibraryImportEvent for field documentation.
type LibraryImportEvent = machine.LibraryImportEvent

type engineConfig struct {
	registry       *registry.Registry
	extensions     []registry.Extension
	maxCallDepth   uint64
	libraryPaths   []string
	libraryEnabled bool // true when WithLibraryPaths was called
	skipCore       bool // true when WithoutCore was called
	importObserver func(LibraryImportEvent)
	authorizer     security.Authorizer
}

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
// A value of 0 means unlimited (the default).
func WithMaxCallDepth(n uint64) EngineOption {
	return func(cfg *engineConfig) {
		cfg.maxCallDepth = n
	}
}

// WithLibraryPaths enables the R7RS library system (define-library / import)
// and configures directories to search for .sld library files.
//
// Without this option, (import ...) raises a configuration error.
//
// Paths are searched in order: user-supplied paths first, then the defaults
// ("." and "./lib"). An empty call WithLibraryPaths() enables library support
// with defaults only.
//
// Example:
//
//	eng, err := wile.NewEngine(ctx,
//	    wile.WithLibraryPaths("/app/libs", "./vendor"),
//	)
//	// search order: /app/libs, ./vendor, ., ./lib
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
		cfg.skipCore = true
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

// SafeExtensions returns engine options that add extensions suitable for
// sandboxed engines: io, exceptions, math, introspection, and the safe
// subset of all (records, promises, strings, characters).
//
// These provide R7RS functionality without filesystem, eval, system, Go
// interop, or threading access. Core primitives are still added by default
// unless WithoutCore is also used.
//
// Example:
//
//	eng, err := wile.NewEngine(ctx,
//	    append(wile.SafeExtensions(),
//	        wile.WithLibraryPaths("./lib"),
//	    )...,
//	)
func SafeExtensions() []EngineOption {
	return []EngineOption{
		WithExtension(io.Extension),
		WithExtension(exceptions.Extension),
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
