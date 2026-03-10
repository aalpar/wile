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
	"github.com/aalpar/wile/extensions/exceptions"
	"github.com/aalpar/wile/extensions/files"
	"github.com/aalpar/wile/extensions/gointerop"
	"github.com/aalpar/wile/extensions/introspection"
	"github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/extensions/system"
	"github.com/aalpar/wile/extensions/threads"
	"github.com/aalpar/wile/internal/extensions/all"
	"github.com/aalpar/wile/internal/extensions/eval"
	ioext "github.com/aalpar/wile/internal/extensions/io"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/registry/core"
	"github.com/aalpar/wile/werr"
)

// allExtensions returns all available extensions for the full runtime environment.
var allExtensions = []registry.Extension{
	ioext.Extension,
	files.Extension,
	math.Extension,
	introspection.Extension,
	eval.Extension,
	exceptions.Extension,
	threads.Extension,
	gointerop.Extension,
	all.Extension,
	system.Extension,
}

// initializeEnvironmentWithRegistry is the shared initialization sequence for environment creation.
// It creates a registry, adds the specified extensions, applies primitives, registers
// compilers/expanders, loads bootstrap macros, and returns the populated registry.
// If exts is nil, all extensions are loaded (backward compatible).
func initializeEnvironmentWithRegistry(ctx context.Context, env *environment.EnvironmentFrame, exts []registry.Extension) (*registry.Registry, error) {
	if exts == nil {
		exts = allExtensions
	}

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

	// Apply registry to environment
	err = reg.Apply(ctx, env)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "error applying registry to environment")
	}

	// Register syntax compilers in the compile environment
	err = machine.RegisterSyntaxCompilers(env)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "error registering syntax compilers")
	}

	// Register primitive expanders in the expand environment
	err = machine.RegisterPrimitiveExpanders(env)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "error registering primitive expanders")
	}

	// Load bootstrap macros from registry
	bootstrapResolver := machine.NewEmbedFileResolver(core.BootstrapFS)
	err = loadBootstrapMacros(ctx, env, reg.MacroSources(), bootstrapResolver)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "error loading bootstrap macros")
	}

	// Set the default file resolver for runtime include/load operations.
	// This must happen after bootstrap (which uses EmbedFileResolver).
	env.SetFileResolver(machine.NewOSFileResolver(env))

	return reg, nil
}

// initializeEnvironment is the shared initialization sequence for environment creation.
// It creates a registry, adds all extensions, applies primitives, registers compilers/expanders,
// and loads bootstrap macros.
func initializeEnvironment(ctx context.Context, env *environment.EnvironmentFrame) error {
	_, err := initializeEnvironmentWithRegistry(ctx, env, nil)
	return err
}

// NewTopLevelEnvironmentFrameTiny creates and initializes a complete Scheme runtime environment.
//
// This function:
//  1. Creates a registry with core primitives
//  2. Adds all extensions (io, files, math, eval, exceptions, threads, gointerop, all, system)
//  3. Creates a new TopLevelEnvironment with per-instance symbol interning
//  4. Applies the registry to register all primitives
//  5. Registers primitive compilers in the compile environment
//  6. Loads bootstrap macros (and, or, let, let*, letrec, cond, when, unless, parameterize)
//
// The resulting environment is ready for parsing, expanding, compiling, and executing
// Scheme programs.
func NewTopLevelEnvironmentFrameTiny(ctx context.Context) (*environment.EnvironmentFrame, error) {
	env, _, err := NewTopLevelWithRegistry(ctx)
	if err != nil {
		return nil, err
	}
	return env, nil
}

// NewTopLevelWithRegistry creates a top-level environment and returns both
// the environment frame and the primitive registry for doc introspection.
func NewTopLevelWithRegistry(ctx context.Context) (*environment.EnvironmentFrame, *registry.Registry, error) {
	// Create TopLevelEnvironment (per-instance symbol interning)
	topLevel := environment.NewTopLevelEnvironment()
	env := topLevel.Runtime()

	// Initialize with shared sequence, keeping the registry
	reg, err := initializeEnvironmentWithRegistry(ctx, env, nil)
	if err != nil {
		return nil, nil, err
	}

	return env, reg, nil
}

// NewLibraryEnvironmentFrame creates a new environment for a library that shares
// the TopLevelEnvironment with the caller. This ensures symbol identity is preserved
// across library boundaries per R7RS §6.5: (eq? 'foo (string->symbol "foo")) must be #t.
//
// The library gets its own:
//   - GlobalEnvironmentFrame for bindings (isolates library definitions)
//   - PhaseRegistry for expand/compile phases
//
// But shares with caller:
//   - TopLevelEnvironment (symbol and syntax interning)
//   - LibraryRegistry (for nested imports)
func NewLibraryEnvironmentFrame(ctx context.Context, callerEnv *environment.EnvironmentFrame, _ []string) (*environment.EnvironmentFrame, error) {
	// Create a new environment that shares the caller's TopLevelEnvironment
	// (for symbol identity per R7RS §6.5) but with isolated bindings.
	libEnv := callerEnv.TopLevelEnv().NewChildRuntime()

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
func loadBootstrapMacros(ctx context.Context, env *environment.EnvironmentFrame, sources []string, resolver machine.FileResolver) error {
	for _, source := range sources {
		rdr := strings.NewReader(source)
		p := parser.NewParser(env, true, rdr)

		for {
			stx, err := p.ReadSyntax(ctx)
			if errors.Is(err, io.EOF) {
				break
			}
			if err != nil {
				return werr.WrapForeignErrorf(err, "error parsing bootstrap macros")
			}

			// Expand the syntax
			expanded, err := machine.NewExpanderTimeContinuation(ctx, env).ExpandExpression(stx)
			if err != nil {
				return werr.WrapForeignErrorf(err, "error expanding bootstrap macro")
			}

			// Compile and run
			tpl := machine.NewNativeTemplate(0, 0, false)
			// Use inTail=false for top-level expressions
			cctx := machine.NewCompileTimeCallContext(ctx, false, true)
			compiler := machine.NewCompiletimeContinuation(tpl, env)
			if resolver != nil {
				compiler.SetFileResolver(resolver)
			}
			err = compiler.CompileExpression(cctx, expanded)
			if err != nil {
				return werr.WrapForeignErrorf(err, "error compiling bootstrap macro")
			}

			cont := machine.NewMachineContinuation(nil, tpl, env)
			mc := machine.NewMachineContext(ctx, cont)
			err = mc.Run()
			if err != nil {
				return werr.WrapForeignErrorf(err, "error running bootstrap macro")
			}
		}
	}
	return nil
}
