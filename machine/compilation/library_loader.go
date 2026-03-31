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

package compilation

// library_loader.go implements R7RS library loading.
//
// When an (import ...) is encountered, this module:
// 1. Finds the library file (.sld or .scm)
// 2. Parses the define-library form
// 3. Compiles the library to bytecode
// 4. Executes the library to populate its environment
// 5. Registers the library in the global registry
// 6. Returns the CompiledLibrary for import binding

import (
	"bufio"
	"context"
	"errors"
	"io"
	"strings"

	"github.com/aalpar/wile/machine"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/werr"
)

// LoadLibrary loads a library by name, compiling and executing it if not already loaded.
// Returns the CompiledLibrary which can be used to import bindings.
//
// The function:
// 1. Checks if already loaded (returns cached library)
// 2. Checks for circular dependencies
// 3. Resolves and opens the library file via FileResolver
// 4. Parses and compiles the define-library form
// 5. Executes the library to create runtime bindings
// 6. Registers the library in the registry
func LoadLibrary(ctx context.Context, name LibraryName, env *environment.EnvironmentFrame, evaluator machine.MacroEvaluator) (*CompiledLibrary, error) {
	registryAny := env.LibraryRegistry()
	if registryAny == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryConfiguration, "load-library: no library registry configured")
	}
	reg, ok := registryAny.(*LibraryRegistry)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryConfiguration, "load-library: invalid library registry type")
	}

	lib := reg.Lookup(name)
	if lib != nil {
		return lib, nil
	}

	if reg.IsLoading(name) {
		return nil, werr.WrapForeignErrorf(werr.ErrCircularDependency,
			"circular dependency detected while loading %s", name.SchemeString())
	}
	reg.StartLoading(name)
	defer reg.FinishLoading(name)

	// Resolve and open via FileResolver (supports both OS and virtual FS).
	resolver, ok := env.FileResolver().(FileResolver)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryConfiguration,
			"load-library: no file resolver configured")
	}

	// Try .sld first, then .scm.
	// Only fall through to .scm on file-not-found; propagate other
	// errors (e.g. security denial) immediately.
	sldPath := name.ToFSPath()
	f, filePath, err := resolver.ResolveAndOpen(ctx, sldPath)
	if err != nil {
		if !errors.Is(err, werr.ErrFileNotFound) {
			return nil, werr.WrapForeignErrorf(err,
				"could not load library %s", name.SchemeString())
		}
		scmPath := strings.TrimSuffix(sldPath, ".sld") + ".scm"
		f, filePath, err = resolver.ResolveAndOpen(ctx, scmPath)
		if err != nil {
			return nil, werr.WrapForeignErrorf(err,
				"could not find library %s", name.SchemeString())
		}
	}
	defer f.Close() //nolint:errcheck

	lib, err = loadLibraryFromReader(ctx, f, filePath, name, env, evaluator)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err,
			"error loading library %s from %s", name.SchemeString(), filePath)
	}

	err = reg.Register(lib)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err,
			"error registering library %s", name.SchemeString())
	}

	return lib, nil
}

// loadLibraryFromReader parses, compiles, and executes a library from an open reader.
func loadLibraryFromReader(ctx context.Context, r io.Reader, filePath string, expectedName LibraryName, callerEnv *environment.EnvironmentFrame, evaluator machine.MacroEvaluator) (*CompiledLibrary, error) {
	// Push to stack after successful open, pop on exit.
	stack := callerEnv.LoadPathStack()
	if stack != nil {
		pushErr := stack.Push(filePath)
		if pushErr != nil {
			return nil, werr.WrapForeignErrorf(pushErr, "load library %s: push load path", expectedName.SchemeString())
		}
		defer stack.Pop()
	}

	factory := callerEnv.Namespace().LibraryEnvFactory()
	if factory == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryConfiguration, "LibraryEnvFactory not configured")
	}
	libEnv, err := factory(ctx, callerEnv, expectedName.Parts)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "could not create library environment")
	}

	libEnv.SetLibraryRegistry(callerEnv.LibraryRegistry())

	reader := bufio.NewReader(r)
	p := parser.NewParserWithFile(libEnv, true, reader, filePath)

	stx, err := p.ReadSyntax(ctx)
	if err != nil {
		if errors.Is(err, io.EOF) {
			return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed, "library file is empty")
		}
		return nil, werr.WrapForeignErrorf(err, "could not parse library file")
	}

	pair, ok := stx.(*syntax.SyntaxPair)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed, "expected define-library form, got %T", stx)
	}

	carStx := pair.SyntaxCar()
	carSym, ok := carStx.(*syntax.SyntaxSymbol)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed, "expected define-library, got %T", carStx)
	}

	symName := carSym.Sym.Key
	if symName != "define-library" && symName != "library" {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed, "expected define-library, got %s", symName)
	}

	lib, err := compileAndExecuteLibrary(ctx, stx, expectedName, libEnv, filePath, evaluator)
	if err != nil {
		return nil, err
	}

	return lib, nil
}

// compileAndExecuteLibrary compiles a define-library form and executes it.
func compileAndExecuteLibrary(ctx context.Context, stx syntax.SyntaxValue, expectedName LibraryName, libEnv *environment.EnvironmentFrame, filePath string, evaluator machine.MacroEvaluator) (*CompiledLibrary, error) {
	// Create a template for the top-level compilation (will be empty after define-library)
	tpl := machine.NewNativeTemplate(0, 0, false)

	// Expand the form
	expanded, err := NewExpanderTimeContinuation(ctx, libEnv, evaluator).ExpandExpression(stx)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "error expanding library")
	}

	// Compile the form
	// Use inTail=false for top-level expressions
	cctx := NewCompileTimeCallContext(ctx, false)
	compiler := NewCompileTimeContinuation(tpl, libEnv, evaluator)

	// Set up to capture the compiled library
	var compiledLib *CompiledLibrary
	compiler.SetLibraryCallback(func(lib *CompiledLibrary) {
		compiledLib = lib
	})

	err = compiler.CompileExpression(cctx, expanded)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "error compiling library")
	}

	if compiledLib == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryConfiguration, "library was not produced by compilation")
	}

	// Verify the library name matches what was expected
	if compiledLib.Name.Key() != expectedName.Key() {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryNameMismatch, "library name mismatch: expected %s, got %s",
			expectedName.SchemeString(), compiledLib.Name.SchemeString())
	}

	// Execute the library's compiled template to populate bindings
	// The library's code (begin blocks, defines) is in compiledLib.Template
	if compiledLib.Template != nil && compiledLib.Template.CodeLen() > 0 {
		_, err = evaluator.EvalTemplate(ctx, compiledLib.Template, compiledLib.Env)
		if err != nil {
			return nil, werr.WrapForeignErrorf(err, "error executing library")
		}
	}

	// Record the source file for error messages
	compiledLib.SourceFile = filePath

	return compiledLib, nil
}
