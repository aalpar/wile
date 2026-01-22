// Copyright 2025 Aaron Alpar
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
package runtime

import (
	"context"
	"io"
	"strings"

	"wile/environment"
	"wile/extensions/all"
	"wile/extensions/eval"
	"wile/extensions/exceptions"
	"wile/extensions/files"
	"wile/extensions/gointerop"
	ioext "wile/extensions/io"
	"wile/extensions/math"
	"wile/extensions/system"
	"wile/extensions/threads"
	"wile/machine"
	"wile/parser"
	"wile/registry"
	"wile/registry/core"
	"wile/values"
)

// allExtensions returns all available extensions for the full runtime environment.
var allExtensions = []registry.Extension{
	ioext.Extension,
	files.Extension,
	math.Extension,
	eval.Extension,
	exceptions.Extension,
	threads.Extension,
	gointerop.Extension,
	all.Extension,
	system.Extension,
}

// NewTopLevelEnvironmentFrameTiny creates and initializes a complete Scheme runtime environment.
//
// This function:
//  1. Creates a registry with core primitives
//  2. Adds all extensions (io, files, math, eval, exceptions, threads, gointerop, all, system)
//  3. Creates a new top-level environment frame
//  4. Applies the registry to register all primitives
//  5. Registers primitive compilers in the compile environment
//  6. Loads bootstrap macros (and, or, let, let*, letrec, cond, when, unless, parameterize)
//
// The resulting environment is ready for parsing, expanding, compiling, and executing
// Scheme programs.
func NewTopLevelEnvironmentFrameTiny(ctx context.Context) (*environment.EnvironmentFrame, error) {
	// Create registry with core primitives
	reg := registry.NewRegistry()
	err := core.AddToRegistry(reg)
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "error adding core to registry")
	}

	// Add all extensions
	for _, ext := range allExtensions {
		err := ext.AddToRegistry(reg)
		if err != nil {
			return nil, values.WrapForeignErrorf(err, "error adding extension %s to registry", ext.Name())
		}
	}

	// Create environment
	env := environment.NewTopLevelEnvironmentFrame()

	// Apply registry to environment
	err = reg.Apply(ctx, env)
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "error applying registry to environment")
	}

	// Register syntax compilers in the compile environment
	err = machine.RegisterSyntaxCompilers(env)
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "error registering syntax compilers")
	}

	// Register primitive expanders in the expand environment
	err = machine.RegisterPrimitiveExpanders(env)
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "error registering primitive expanders")
	}

	// Load bootstrap macros from registry
	err = loadBootstrapMacros(ctx, env, reg.MacroSources())
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "error loading bootstrap macros")
	}

	return env, nil
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
func loadBootstrapMacros(ctx context.Context, env *environment.EnvironmentFrame, sources []string) error {
	for _, source := range sources {
		rdr := strings.NewReader(source)
		p := parser.NewParser(env, true, rdr)

		for {
			stx, err := p.ReadSyntax(ctx)
			if err == io.EOF {
				break
			}
			if err != nil {
				return values.WrapForeignErrorf(err, "error parsing bootstrap macros")
			}

			// Expand the syntax
			ectx := machine.NewExpandTimeCallContext()
			expanded, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
			if err != nil {
				return values.WrapForeignErrorf(err, "error expanding bootstrap macro")
			}

			// Compile and run
			tpl := machine.NewNativeTemplate(0, 0, false)
			// Use inTail=false for top-level expressions
			cctx := machine.NewCompileTimeCallContext(false, true, env)
			err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
			if err != nil {
				return values.WrapForeignErrorf(err, "error compiling bootstrap macro")
			}

			cont := machine.NewMachineContinuation(nil, tpl, env)
			mc := machine.NewMachineContext(ctx, cont)
			err = mc.Run()
			if err != nil {
				return values.WrapForeignErrorf(err, "error running bootstrap macro")
			}
		}
	}
	return nil
}
