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

package machine

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
	"os"

	"wile/environment"
	"wile/parser"
	"wile/syntax"
	"wile/values"
)

// LibraryEnvFactory is a function that creates a new top-level environment for a library.
// This avoids import cycles between machine and runtime packages.
// The function should return a fresh environment with primitives registered.
var LibraryEnvFactory func() (*environment.EnvironmentFrame, error)

// LoadLibrary loads a library by name, compiling and executing it if not already loaded.
// Returns the CompiledLibrary which can be used to import bindings.
//
// The function:
// 1. Checks if already loaded (returns cached library)
// 2. Checks for circular dependencies
// 3. Finds the library file on the search path
// 4. Parses and compiles the define-library form
// 5. Executes the library to create runtime bindings
// 6. Registers the library in the registry
func LoadLibrary(ctx context.Context, name LibraryName, env *environment.EnvironmentFrame) (*CompiledLibrary, error) {
	// Get the library registry from the environment
	registryAny := env.LibraryRegistry()
	if registryAny == nil {
		return nil, values.NewForeignErrorf("load-library: no library registry configured")
	}
	registry, ok := registryAny.(*LibraryRegistry)
	if !ok {
		return nil, values.NewForeignErrorf("load-library: invalid library registry type")
	}

	// Already loaded?
	if lib := registry.Lookup(name); lib != nil {
		return lib, nil
	}

	// Cycle detection
	if registry.IsLoading(name) {
		return nil, values.WrapForeignErrorf(ErrCircularDependency,
			"circular dependency detected while loading %s", name.SchemeString())
	}
	registry.StartLoading(name)
	defer registry.FinishLoading(name)

	// Find the library file
	filePath, err := registry.FindLibraryFile(name)
	if err != nil {
		return nil, values.WrapForeignErrorf(err,
			"could not find library %s", name.SchemeString())
	}

	// Load the library from file
	lib, err := loadLibraryFromFile(ctx, filePath, name, env)
	if err != nil {
		return nil, values.WrapForeignErrorf(err,
			"error loading library %s from %s", name.SchemeString(), filePath)
	}

	// Register the library
	if err := registry.Register(lib); err != nil {
		return nil, values.WrapForeignErrorf(err,
			"error registering library %s", name.SchemeString())
	}

	return lib, nil
}

// loadLibraryFromFile parses, compiles, and executes a library file.
func loadLibraryFromFile(ctx context.Context, filePath string, expectedName LibraryName, callerEnv *environment.EnvironmentFrame) (*CompiledLibrary, error) {
	// Open the file
	file, err := os.Open(filePath)
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "could not open file")
	}
	defer file.Close() //nolint:errcheck

	// Create a fresh top-level environment for the library
	// This isolates the library from the caller's environment
	if LibraryEnvFactory == nil {
		return nil, values.NewForeignErrorf("LibraryEnvFactory not configured")
	}
	libEnv, err := LibraryEnvFactory()
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "could not create library environment")
	}

	// Share the library registry with the new environment
	// so that nested imports work correctly
	libEnv.SetLibraryRegistry(callerEnv.LibraryRegistry())

	// Parse the file
	reader := bufio.NewReader(file)
	p := parser.NewParserWithFile(libEnv, reader, filePath)

	// Read the first form - should be define-library
	stx, err := p.ReadSyntax(nil)
	if err != nil {
		if errors.Is(err, io.EOF) {
			return nil, values.NewForeignErrorf("library file is empty")
		}
		return nil, values.WrapForeignErrorf(err, "could not parse library file")
	}

	// Verify it's a define-library form
	pair, ok := stx.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.NewForeignErrorf("expected define-library form, got %T", stx)
	}

	carStx := pair.SyntaxCar()
	carSym, ok := carStx.(*syntax.SyntaxSymbol)
	if !ok {
		return nil, values.NewForeignErrorf("expected define-library, got %T", carStx)
	}

	symName := carSym.Key
	if symName != "define-library" && symName != "library" {
		return nil, values.NewForeignErrorf("expected define-library, got %s", symName)
	}

	// Compile and execute the library
	lib, err := compileAndExecuteLibrary(ctx, stx, expectedName, libEnv, filePath)
	if err != nil {
		return nil, err
	}

	return lib, nil
}

// compileAndExecuteLibrary compiles a define-library form and executes it.
func compileAndExecuteLibrary(ctx context.Context, stx syntax.SyntaxValue, expectedName LibraryName, libEnv *environment.EnvironmentFrame, filePath string) (*CompiledLibrary, error) {
	// Create a template for the top-level compilation (will be empty after define-library)
	tpl := NewNativeTemplate(0, 0, false)

	// Expand the form
	ectx := NewExpandTimeCallContext()
	expanded, err := NewExpanderTimeContinuation(libEnv).ExpandExpression(ectx, stx)
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "error expanding library")
	}

	// Compile the form
	// Use inTail=false for top-level expressions
	cctx := NewCompileTimeCallContext(false, true, libEnv)
	compiler := NewCompiletimeContinuation(tpl, libEnv)

	// Set up to capture the compiled library
	var compiledLib *CompiledLibrary
	compiler.SetLibraryCallback(func(lib *CompiledLibrary) {
		compiledLib = lib
	})

	err = compiler.CompileExpression(cctx, expanded)
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "error compiling library")
	}

	if compiledLib == nil {
		return nil, values.NewForeignErrorf("library was not produced by compilation")
	}

	// Verify the library name matches what was expected
	if compiledLib.Name.Key() != expectedName.Key() {
		return nil, values.NewForeignErrorf("library name mismatch: expected %s, got %s",
			expectedName.SchemeString(), compiledLib.Name.SchemeString())
	}

	// Execute the library's compiled template to populate bindings
	// The library's code (begin blocks, defines) is in compiledLib.Template
	if compiledLib.Template != nil && len(compiledLib.Template.operations) > 0 {
		cont := NewMachineContinuation(nil, compiledLib.Template, compiledLib.Env)
		mc := NewMachineContext(cont)
		if err := mc.Run(ctx); err != nil {
			return nil, values.WrapForeignErrorf(err, "error executing library")
		}
	}

	// Record the source file for error messages
	compiledLib.SourceFile = filePath

	return compiledLib, nil
}
