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

import (
	"bufio"
	"context"
	"errors"
	"io"

	"github.com/aalpar/wile/machine"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// CompileDefineLibrary handles (define-library (lib-name) <library-declaration> ...).
//
// R7RS library syntax:
//
//	(define-library <library-name>
//	  <library-declaration> ...)
//
//	<library-declaration> =
//	  | (export <export-spec> ...)
//	  | (import <import-set> ...)
//	  | (begin <command-or-definition> ...)
//	  | (include <filename> ...)
//	  | (include-ci <filename> ...)
//
// This creates an isolated environment for the library, processes declarations
// in order, and registers the compiled library in the registry.
func (p *CompileTimeContinuation) CompileDefineLibrary(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// expr is ((lib-name) <declaration> ...) - args after 'define-library' keyword
	rest, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAPair, "define-library: expected library name and declarations")
	}

	// Parse library name: (lib-name) is a list of identifiers
	libNameExpr := rest.SyntaxCar()
	libName, err := ParseLibraryNameFromDatum(ctctx.ctx, libNameExpr.UnwrapAll())
	if err != nil {
		return werr.WrapForeignErrorf(err, "define-library: invalid library name")
	}

	// Create isolated library environment with primitives
	// The library gets its own bindings but shares the Namespace for syntax interning
	var libEnv *environment.EnvironmentFrame
	factory := p.env.Namespace().LibraryEnvFactory()
	if factory != nil {
		libEnv, err = factory(ctctx.ctx, p.env, libName.Parts)
		if err != nil {
			return werr.WrapForeignErrorf(err, "define-library: could not create library environment")
		}
		// Share the library registry with the new environment so nested imports work
		libEnv.SetLibraryRegistry(p.env.LibraryRegistry())
	} else {
		// Fallback for tests that don't set up the factory
		libEnv = environment.NewNamespace().Runtime()
	}

	lib := NewCompiledLibrary(libName, libEnv)

	// Create a unique library scope for cross-library macro hygiene.
	// When a macro defined in this library references an unexported binding,
	// the library scope enables the compiler to redirect lookup to this
	// library's environment via the TLE's scope registry.
	libScope := syntax.NewScopeWithLabel("library:" + libName.SchemeString())
	p.env.Namespace().RegisterLibraryScope(libScope, libEnv)

	// Process library declarations
	declsExpr := rest.SyntaxCdr()
	// Handle empty declarations list (just (define-library (name)))
	if syntax.IsSyntaxEmptyList(declsExpr) {
		// Empty library is valid - just call the callback
		lib.Template = machine.NewNativeTemplate(0, 0, false)
		if p.libraryCallback != nil {
			p.libraryCallback(lib)
		}
		return nil
	}

	decls, ok := declsExpr.(*syntax.SyntaxPair)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAPair, "define-library: expected list of declarations")
	}

	// Create a compiler for the library environment
	libTemplate := machine.NewNativeTemplate(0, 0, false)
	libCompiler := NewCompileTimeContinuation(libTemplate, libEnv, p.evaluator)
	libCompiler.libraryScope = libScope

	// Process each declaration
	_, err = syntax.SyntaxForEach(ctctx.ctx, decls, func(_ context.Context, _ int, _ bool, decl syntax.SyntaxValue) error {
		return libCompiler.processLibraryDeclaration(ctctx, lib, decl)
	})
	if err != nil {
		return werr.WrapForeignErrorf(err, "define-library: error processing declarations")
	}

	// Peephole optimization on the library template.
	libTemplate.Optimize()

	// Store the compiled template in the library
	lib.Template = libTemplate

	// Call the library callback if set (used by LoadLibrary)
	if p.libraryCallback != nil {
		p.libraryCallback(lib)
	}

	// Library compilation is complete - no runtime operations in the main template
	return nil
}

// processLibraryDeclaration handles a single library declaration.
func (p *CompileTimeContinuation) processLibraryDeclaration(ctctx CompileTimeCallContext, lib *CompiledLibrary, decl syntax.SyntaxValue) error {
	declPair, ok := decl.(*syntax.SyntaxPair)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAPair, "library declaration must be a list")
	}

	// Get the declaration keyword
	keywordExpr := declPair.SyntaxCar()
	keywordSym, ok := keywordExpr.(*syntax.SyntaxSymbol)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "library declaration must start with symbol")
	}

	keyword := keywordSym.Unwrap().(*values.Symbol).Key

	// Get the rest of the declaration (arguments)
	argsExpr := declPair.SyntaxCdr()
	switch keyword {
	case "export":
		return p.processLibraryExport(ctctx.ctx, lib, argsExpr)
	case "import":
		return p.processLibraryImport(ctctx, lib, argsExpr)
	case "begin":
		// Compile the begin body in the library environment with letrec* semantics.
		// R7RS §5.3.2: All defined names are visible to all initializers.
		beginPair, ok := argsExpr.(*syntax.SyntaxPair)
		if !ok {
			if syntax.IsSyntaxEmptyList(argsExpr) {
				return nil // empty begin is valid
			}
			return werr.WrapForeignErrorf(werr.ErrNotAPair, "begin: expected list of expressions")
		}
		return p.compileLibraryBegin(ctctx, beginPair)
	case "include":
		return p.CompileInclude(ctctx, argsExpr)
	case "include-ci":
		return p.CompileIncludeCi(ctctx, argsExpr)
	case "include-library-declarations":
		return p.processIncludeLibraryDeclarations(ctctx, lib, argsExpr)
	case "cond-expand":
		return p.processCondExpand(ctctx, lib, argsExpr)
	case "description":
		return p.processLibraryDescription(lib, argsExpr)
	default:
		return werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "unknown library declaration: %s", keyword)
	}
}

// processLibraryExport handles (export <export-spec> ...) within a library.
func (p *CompileTimeContinuation) processLibraryExport(ctx context.Context, lib *CompiledLibrary, args syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(args) {
		return nil // empty export is valid
	}

	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAPair, "export: expected list of export specs")
	}

	_, err := syntax.SyntaxForEach(ctx, argsPair, func(_ context.Context, _ int, _ bool, spec syntax.SyntaxValue) error {
		return parseExportSpec(lib, spec)
	})
	return err
}

// processLibraryDescription handles (description <string>) within a library.
func (p *CompileTimeContinuation) processLibraryDescription(lib *CompiledLibrary, args syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(args) {
		return werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "description: expected a string argument")
	}
	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAPair, "description: expected a string argument")
	}
	strExpr := argsPair.SyntaxCar()
	str, ok := strExpr.UnwrapAll().(*values.String)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAString, "description: argument must be a string")
	}
	if !syntax.IsSyntaxEmptyList(argsPair.SyntaxCdr()) {
		return werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "description: expected exactly one string argument")
	}
	// Last-writer-wins: multiple description declarations are allowed;
	// the last one takes effect.
	lib.Description = str.Value
	return nil
}

// parseExportSpec parses a single export spec and adds it to the library.
// Export specs can be:
//   - <identifier>              : export with same internal and external name
//   - (rename <internal> <external>) : export with different names
func parseExportSpec(lib *CompiledLibrary, spec syntax.SyntaxValue) error {
	switch s := spec.(type) {
	case *syntax.SyntaxComment, *syntax.SyntaxDatumComment:
		// Skip comments in export lists
		return nil

	case *syntax.SyntaxSymbol:
		// Simple export: symbol name
		name := s.Unwrap().(*values.Symbol).Key
		lib.AddExport(name, name)
		return nil

	case *syntax.SyntaxPair:
		// Could be (rename internal external)
		carExpr := s.SyntaxCar()
		carSym, ok := carExpr.(*syntax.SyntaxSymbol)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "export: expected symbol")
		}

		if carSym.Unwrap().(*values.Symbol).Key == "rename" {
			// (rename internal external)
			cdrExpr, ok := s.SyntaxCdr().(*syntax.SyntaxPair)
			if !ok {
				return werr.WrapForeignErrorf(werr.ErrNotAPair, "export rename: expected internal and external names")
			}

			internalExpr := cdrExpr.SyntaxCar()
			internalSym, ok := internalExpr.(*syntax.SyntaxSymbol)
			if !ok {
				return werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "export rename: internal name must be symbol")
			}

			cdrCdr, ok := cdrExpr.SyntaxCdr().(*syntax.SyntaxPair)
			if !ok {
				return werr.WrapForeignErrorf(werr.ErrNotAPair, "export rename: expected external name")
			}

			externalExpr := cdrCdr.SyntaxCar()
			externalSym, ok := externalExpr.(*syntax.SyntaxSymbol)
			if !ok {
				return werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "export rename: external name must be symbol")
			}

			internalName := internalSym.Unwrap().(*values.Symbol).Key
			externalName := externalSym.Unwrap().(*values.Symbol).Key
			lib.AddExport(externalName, internalName)
			return nil
		}

		return werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "export: invalid spec form")

	default:
		return werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "export: expected symbol or rename form")
	}
}

// CompileExport handles top-level (export <export-spec> ...).
//
// This is only valid within a library definition. At top-level, it's an error.
func (p *CompileTimeContinuation) CompileExport(_ CompileTimeCallContext, _ syntax.SyntaxValue) error {
	return werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "export: only valid within define-library")
}

// processIncludeLibraryDeclarations handles (include-library-declarations <string> ...) within a library.
// It reads each file and processes its contents as library declarations.
func (p *CompileTimeContinuation) processIncludeLibraryDeclarations(ctctx CompileTimeCallContext, lib *CompiledLibrary, args syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(args) {
		return nil // Empty is valid (no-op)
	}

	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAPair, "include-library-declarations: expected list of filenames")
	}

	// Process each filename
	_, err := syntax.SyntaxForEach(ctctx.ctx, argsPair, func(ctx context.Context, _ int, _ bool, v syntax.SyntaxValue) error {
		fn, ok := v.Unwrap().(*values.String)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAString, "include-library-declarations: expected string filename")
		}

		// Find and open the file
		file, filePath, err := findFile(p, ctctx, fn.Value)
		if err != nil {
			return werr.WrapForeignErrorf(err, "include-library-declarations: failed to find file %q", fn.Value)
		}
		if file == nil {
			return werr.WrapForeignErrorf(werr.ErrFileNotFound, "include-library-declarations: file not found: %q", fn.Value)
		}
		defer file.Close() //nolint:errcheck

		// Create parser for the file
		reader := bufio.NewReader(file)
		fileParser := parser.NewParserWithFile(p.env, true, reader, filePath)

		// Read and process all forms from the file as library declarations
		for {
			stx, readErr := fileParser.ReadSyntax(ctx)
			if readErr != nil {
				if errors.Is(readErr, io.EOF) {
					break
				}
				return werr.WrapForeignErrorf(readErr, "include-library-declarations: error reading %q", fn.Value)
			}

			// Process the form as a library declaration
			err := p.processLibraryDeclaration(ctctx, lib, stx)
			if err != nil {
				return werr.WrapForeignErrorf(err, "include-library-declarations: error processing declaration from %q", fn.Value)
			}
		}

		return nil
	})
	if err != nil {
		return err
	}
	return nil
}
