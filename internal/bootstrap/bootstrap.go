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

// Package runtime provides the runtime environment initialization for the Scheme interpreter.
//
// This package is responsible for:
//   - Creating and initializing the top-level environment with all R7RS primitives
//   - Loading bootstrap macros (and, or, let, let*, letrec, cond, when, unless)
//
// # Architecture
//
// The runtime creates a three-phase environment hierarchy:
//
//	TopLevel (Runtime) -> Expand -> Compile
//
// Primitives are registered via the registry pattern from registry/core and extensions/*.
package bootstrap

import (
	"context"
	"errors"
	"io"
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/extensions/algebragraph"
	"github.com/aalpar/wile/extensions/charsets"
	"github.com/aalpar/wile/extensions/eval"
	"github.com/aalpar/wile/extensions/files"
	"github.com/aalpar/wile/extensions/gointerop"
	"github.com/aalpar/wile/extensions/introspection"
	"github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/extensions/process"
	"github.com/aalpar/wile/extensions/sat"
	"github.com/aalpar/wile/extensions/system"
	"github.com/aalpar/wile/extensions/threads"
	"github.com/aalpar/wile/internal/extensions/all"
	"github.com/aalpar/wile/internal/extensions/envvars"
	ioext "github.com/aalpar/wile/internal/extensions/io"
	nsext "github.com/aalpar/wile/internal/extensions/namespace"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/machine/compilation"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/registry/core"
	"github.com/aalpar/wile/werr"
)

// allExtensions is the KitchenSink-equivalent — every extension registered.
// Retained as a package-level var so tests can override (see bootstrap_test.go)
// and so internal callers (NewLibraryEnvironmentFrame, NewTopLevelWithRegistry)
// do not re-resolve through the string-keyed ProfileExtensions table.
//
// ADDING A NEW EXTENSION requires updates in these locations:
//
//  1. extensions/<name>/             — new package implementing registry.Extension
//  2. extensions/<name>/register.go  — Builder + Extension var, AddToRegistry
//  3. internal/bootstrap/bootstrap.go — add to allExtensions AND ProfileExtensions("kitchen-sink")
//  4. internal/bootstrap/bootstrap.go — add to ProfileExtensions("small") if safe
//  5. CLAUDE.md                      — update extension count and list
//  6. TODO.md                        — update extension count in project status
var allExtensions = []registry.Extension{
	ioext.Extension,
	files.Extension,
	math.Extension,
	introspection.Extension,
	eval.Extension,
	nsext.Extension,
	threads.Extension,
	gointerop.Extension,
	all.Extension,
	system.Extension,
	process.Extension,
	sat.Extension,
	charsets.Extension,
	envvars.Extension,
	algebragraph.Extension,
}

// ProfileExtensions returns the extension set for a named profile. It is the
// single source of truth for profile→extensions mapping; callers in the
// public wile package (profile.go Profile.extensions) and in extensions/eval
// (tryWileProfile, for Scheme-level (environment '(wile <name>))) both
// dispatch through here.
//
// Valid names: "tiny", "console", "console-with-load", "small", "kitchen-sink".
// An unknown name returns ErrUnknownProfile wrapped with the offending string.
func ProfileExtensions(name string) ([]registry.Extension, error) {
	switch name {
	case "tiny":
		return nil, nil
	case "console":
		return []registry.Extension{
			ioext.Extension,
			files.Extension,
			math.Extension,
			all.SafeExtension,
			charsets.Extension,
			envvars.Extension,
		}, nil
	case "console-with-load":
		return []registry.Extension{
			ioext.Extension,
			files.Extension,
			math.Extension,
			all.SafeExtension,
			eval.Extension,
			charsets.Extension,
			envvars.Extension,
		}, nil
	case "small":
		return []registry.Extension{
			ioext.Extension,
			files.Extension,
			math.Extension,
			introspection.Extension,
			eval.Extension,
			all.Extension,
			charsets.Extension,
			system.Extension,
			envvars.Extension,
		}, nil
	case "kitchen-sink":
		// Return a copy so callers cannot mutate the underlying slice.
		q := make([]registry.Extension, len(allExtensions))
		copy(q, allExtensions)
		return q, nil
	default:
		return nil, werr.WrapForeignErrorf(ErrUnknownProfile,
			"ProfileExtensions: unknown profile name %q", name)
	}
}

// ErrUnknownProfile is returned by ProfileExtensions when given a name outside
// the known set (tiny, console, console-with-load, small, kitchen-sink).
var ErrUnknownProfile = werr.NewStaticError("unknown profile")

// initializeEnvironmentWithRegistry is the shared initialization sequence for environment creation.
// It creates a registry, adds the specified extensions, applies primitives, registers
// compilers/expanders, loads bootstrap macros, and returns the populated registry.
// The caller must pass the exact extension set to load; a nil slice registers no
// extensions beyond core primitives (this is deliberate — the public Engine
// path now drives extension selection through Profile).
func initializeEnvironmentWithRegistry(ctx context.Context, env *environment.EnvironmentFrame, exts []registry.Extension) (*registry.Registry, error) {
	// Create registry with core primitives
	reg := registry.NewRegistry()
	err := core.AddToRegistry(reg)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "error adding core to registry")
	}

	// Add extensions
	for _, ext := range exts {
		err := ext.AddToRegistry(reg)
		if err != nil {
			return nil, werr.WrapForeignErrorf(err, "error adding extension %s to registry", ext.Name())
		}
	}

	// Apply registry to environment. Runtime primitives are routed to the sealed-base
	// frame (immutable) for a namespace-owning runtime env; a flat library env receives
	// them in its own frame. SealedBaseTarget() picks the right frame for each case.
	runtimeTarget := env.SealedBaseTarget()
	err = reg.Apply(ctx, env, registry.WithRuntimeTarget(runtimeTarget))
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "error applying registry to environment")
	}

	// Store registry on namespace so runtime primitives (apropos, doc-topic,
	// doc-topics) can access it via mc.EnvironmentFrame().Namespace().Registry().
	env.Namespace().SetRegistry(reg)

	// Register syntax compilers (compile env) and primitive expanders (expand env).
	err = compilation.RegisterAllPhaseHandlers(env)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "error registering phase handlers")
	}

	// Load bootstrap macros into the mutable expand frame FIRST: bootstrap procedures
	// use bootstrap macros (let/and), so the macros must exist before the procedures
	// compile. Procedures then load into the sealed-base frame (runtimeTarget), after
	// macros. Both resolve macros through the shared phase registry.
	bootstrapResolver := compilation.NewEmbedFileResolver(core.BootstrapFS)
	err = loadBootstrapMacros(ctx, env, reg.MacroSources(), bootstrapResolver)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "error loading bootstrap macros")
	}
	err = loadBootstrapProcedures(ctx, runtimeTarget, reg.ProcedureSources(), bootstrapResolver)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "error loading bootstrap procedures")
	}

	// Set the default file resolver for runtime include/load operations,
	// but only if no resolver has been configured (e.g., by WithSourceFS).
	// Invariant: if FileResolver() is nil here after WithSourceFS was called,
	// the WithSourceFS configuration was lost — that's a bug in the caller.
	// This must happen after bootstrap (which uses EmbedFileResolver).
	if env.Namespace().FileResolver() == nil {
		env.SetFileResolver(compilation.NewOSFileResolver(env))
	}

	return reg, nil
}

// initializeEnvironment is the shared initialization sequence for environment creation.
// It creates a registry, adds all extensions, applies primitives, registers compilers/expanders,
// and loads bootstrap macros.
func initializeEnvironment(ctx context.Context, env *environment.EnvironmentFrame) error {
	_, err := initializeEnvironmentWithRegistry(ctx, env, allExtensions)
	return err
}

// NewNamespaceFrame creates and initializes a complete Scheme runtime environment.
//
// This function:
//  1. Creates a registry with core primitives
//  2. Adds all extensions (io, files, math, introspection, eval, threads, gointerop, all, system)
//  3. Creates a new Namespace with per-instance symbol interning
//  4. Applies the registry to register all primitives
//  5. Registers primitive compilers in the compile environment
//  6. Loads bootstrap macros (and, or, let, let*, letrec, cond, when, unless, parameterize)
//
// The resulting environment is ready for parsing, expanding, compiling, and executing
// Scheme programs.
func NewNamespaceFrame(ctx context.Context) (*environment.EnvironmentFrame, error) {
	env, _, err := NewTopLevelWithRegistry(ctx)
	if err != nil {
		return nil, err
	}
	return env, nil
}

// NewTopLevelWithRegistry creates a top-level environment and returns both
// the environment frame and the primitive registry for doc introspection.
func NewTopLevelWithRegistry(ctx context.Context) (*environment.EnvironmentFrame, *registry.Registry, error) {
	// Create Namespace (per-instance symbol interning)
	topLevel := environment.NewNamespace()
	env := topLevel.Runtime()

	// Initialize with shared sequence, keeping the registry
	reg, err := initializeEnvironmentWithRegistry(ctx, env, allExtensions)
	if err != nil {
		return nil, nil, err
	}

	return env, reg, nil
}

// NewProfileEnvironment creates a new *environment.Namespace initialized with the
// given extension set. It shares symbol interning with callerNS but has its own
// bindings so the caller's top-level definitions do not leak in. Intended for
// Scheme-level (environment '(wile <profile>)) construction.
func NewProfileEnvironment(ctx context.Context, callerNS *environment.Namespace, exts []registry.Extension) (*environment.Namespace, error) {
	newNS := callerNS.NewChildNamespace()
	env := newNS.Runtime()
	_, err := initializeEnvironmentWithRegistry(ctx, env, exts)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "NewProfileEnvironment: initialization failed")
	}
	return newNS, nil
}

// init wires the ProfileFactory callback in extensions/eval so that
// (environment '(wile <name>)) can construct a namespace for a named profile.
// This indirection exists because bootstrap imports eval (for allExtensions)
// and eval cannot reciprocally import bootstrap without an import cycle.
func init() {
	eval.ProfileFactory = func(ctx context.Context, callerNS *environment.Namespace, profileName string) (*environment.Namespace, error) {
		exts, err := ProfileExtensions(profileName)
		if err != nil {
			return nil, werr.WrapForeignErrorf(err,
				"ProfileFactory: unknown wile profile %q", profileName)
		}
		return NewProfileEnvironment(ctx, callerNS, exts)
	}
}

// NewLibraryEnvironmentFrame creates a new environment for a library that shares
// the Namespace with the caller. This ensures symbol identity is preserved
// across library boundaries per R7RS §6.5: (eq? 'foo (string->symbol "foo")) must be #t.
//
// The library gets its own:
//   - GlobalEnvironmentFrame for bindings (isolates library definitions)
//   - PhaseRegistry for expand/compile phases
//
// But shares with caller:
//   - Namespace (symbol and syntax interning)
//   - LibraryRegistry (for nested imports)
func NewLibraryEnvironmentFrame(ctx context.Context, callerEnv *environment.EnvironmentFrame, _ []string) (*environment.EnvironmentFrame, error) {
	// Create a new environment that shares the caller's Namespace
	// (for symbol identity per R7RS §6.5) but with isolated bindings.
	libEnv := callerEnv.Namespace().NewChildRuntime()

	// Initialize with shared sequence
	err := initializeEnvironment(ctx, libEnv)
	if err != nil {
		return nil, err
	}

	return libEnv, nil
}

// loadBootstrapMacros parses and executes the bootstrap macro definitions.
// This loads the derived expression forms (and, or, let, let*, letrec, cond, when, unless)
// from the registry's macro sources.
//
// Each macro definition is:
//  1. Parsed from the macro source string
//  2. Macro-expanded (which is a no-op for define-syntax at top level)
//  3. Compiled to bytecode
//  4. Executed to register the syntax transformer
func loadBootstrapMacros(ctx context.Context, env *environment.EnvironmentFrame, sources []string, resolver compilation.FileResolver) error {
	return loadBootstrapSources(ctx, env, sources, resolver, "macro")
}

// loadBootstrapProcedures loads runtime-procedure sources (define forms) into the given
// target frame — the sealed base for an engine root/profile, or a flat library frame.
// `define` forms write to the target frame's own global; the target's Expand()/Compile()
// resolve through the shared phase registry, so the macros loaded earlier are visible.
// MUST run after loadBootstrapMacros (procedures use bootstrap macros).
func loadBootstrapProcedures(ctx context.Context, target *environment.EnvironmentFrame, sources []string, resolver compilation.FileResolver) error {
	return loadBootstrapSources(ctx, target, sources, resolver, "procedure")
}

// loadBootstrapSources is the shared parse → expand/compile → execute pipeline for
// bootstrap sources. The macro and procedure loaders differ only in their target frame
// and the source kind named in error context.
func loadBootstrapSources(ctx context.Context, env *environment.EnvironmentFrame, sources []string, resolver compilation.FileResolver, kind string) error {
	for _, source := range sources {
		rdr := strings.NewReader(source)
		p := parser.NewParser(env, true, rdr)

		for {
			stx, err := p.ReadSyntax(ctx)
			if errors.Is(err, io.EOF) {
				break
			}
			if err != nil {
				return werr.WrapForeignErrorf(err, "error parsing bootstrap %s", kind)
			}

			tpl, err := compilation.ExpandAndCompile(ctx, env, stx, resolver, compilation.DefaultInlineThreshold, compilation.DefaultMaxExpandDepth)
			if err != nil {
				return werr.WrapForeignErrorf(err, "error expanding/compiling bootstrap %s", kind)
			}

			cont := machine.NewMachineContinuation(nil, tpl, env)
			mc := machine.NewMachineContext(ctx, cont)
			err = mc.Run()
			if err != nil {
				return werr.WrapForeignErrorf(err, "error running bootstrap %s", kind)
			}
		}
	}
	return nil
}
