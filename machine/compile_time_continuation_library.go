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

package machine

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
)

// compileLibraryBegin compiles a library begin body with letrec* semantics.
//
// R7RS §5.3.2: Internal definitions use letrec* semantics where all defined
// variables are in scope at the start of the body. This enables forward
// references like (define any ...(every pair? lol)...) where every is
// defined later in the body.
//
// R7RS §5.3: Internal define-syntax forms must be processed before expanding
// subsequent body expressions so that locally-defined macros are visible.
//
// This function performs two passes:
//  1. Expansion pass: Expand all forms, compiling define-syntax as encountered
//  2. Compilation pass: Pre-declare define bindings, then compile all expressions
func (p *CompileTimeContinuation) compileLibraryBegin(ctctx CompileTimeCallContext, expr *syntax.SyntaxPair) error {
	if !expr.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list of expressions, got %T", expr)
	}

	// Collect forms into a slice
	var forms []syntax.SyntaxValue
	_, err := syntax.SyntaxForEach(ctctx.ctx, expr, func(_ context.Context, _ int, _ bool, v syntax.SyntaxValue) error {
		forms = append(forms, v)
		return nil
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "failed to collect library body forms")
	}

	// Pass 1: Expand all forms, compiling define-syntax as encountered
	expander := NewExpanderTimeContinuation(p.env)
	expandedForms, err := expander.ExpandBodyWithDefineSyntax(ctctx.ctx, forms)
	if err != nil {
		return values.WrapForeignErrorf(err, "library: error expanding forms")
	}

	// Pre-declare all define bindings for letrec* semantics
	for _, expanded := range expandedForms {
		p.predeclareDefineBinding(expanded)
	}

	// Pass 2: Compile all expanded expressions
	for i, expanded := range expandedForms {
		ctctx0 := ctctx
		if i < len(expandedForms)-1 {
			ctctx0 = ctctx.NotInTail()
		}
		compileErr := p.CompileExpression(ctctx0, expanded)
		if compileErr != nil {
			return values.WrapForeignErrorf(compileErr, "library: error compiling form")
		}
	}

	return nil
}

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
		return values.WrapForeignErrorf(values.ErrNotAPair, "define-library: expected library name and declarations")
	}

	// Parse library name: (lib-name) is a list of identifiers
	libNameExpr := rest.SyntaxCar()
	libName, err := parseLibraryName(ctctx.ctx, libNameExpr)
	if err != nil {
		return values.WrapForeignErrorf(err, "define-library: invalid library name")
	}

	// Create isolated library environment with primitives
	// The library gets its own bindings but shares the TopLevelEnvironment for symbol interning
	var libEnv *environment.EnvironmentFrame
	factory := p.env.TopLevelEnv().LibraryEnvFactory()
	if factory != nil {
		libEnv, err = factory(ctctx.ctx, p.env)
		if err != nil {
			return values.WrapForeignErrorf(err, "define-library: could not create library environment")
		}
		// Share the library registry with the new environment so nested imports work
		libEnv.SetLibraryRegistry(p.env.LibraryRegistry())
	} else {
		// Fallback for tests that don't set up the factory
		libEnv = environment.NewTopLevelEnvironment().Runtime()
	}

	lib := NewCompiledLibrary(libName, libEnv)

	// Process library declarations
	declsExpr := rest.SyntaxCdr()
	// Handle empty declarations list (just (define-library (name)))
	if syntax.IsSyntaxEmptyList(declsExpr) {
		// Empty library is valid - just call the callback
		lib.Template = NewNativeTemplate(0, 0, false)
		if p.libraryCallback != nil {
			p.libraryCallback(lib)
		}
		return nil
	}

	decls, ok := declsExpr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "define-library: expected list of declarations")
	}

	// Create a compiler for the library environment
	libTemplate := NewNativeTemplate(0, 0, false)
	libCompiler := NewCompiletimeContinuation(libTemplate, libEnv)

	// Process each declaration
	_, err = syntax.SyntaxForEach(ctctx.ctx, decls, func(_ context.Context, _ int, _ bool, decl syntax.SyntaxValue) error {
		return libCompiler.processLibraryDeclaration(ctctx, lib, decl)
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "define-library: error processing declarations")
	}

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
		return values.WrapForeignErrorf(values.ErrNotAPair, "library declaration must be a list")
	}

	// Get the declaration keyword
	keywordExpr := declPair.SyntaxCar()
	keywordSym, ok := keywordExpr.(*syntax.SyntaxSymbol)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "library declaration must start with symbol")
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
			return values.WrapForeignErrorf(values.ErrNotAPair, "begin: expected list of expressions")
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
	default:
		return values.WrapForeignErrorf(values.ErrInvalidSyntax, "unknown library declaration: %s", keyword)
	}
}

// processLibraryExport handles (export <export-spec> ...) within a library.
func (p *CompileTimeContinuation) processLibraryExport(ctx context.Context, lib *CompiledLibrary, args syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(args) {
		return nil // empty export is valid
	}

	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "export: expected list of export specs")
	}

	_, err := syntax.SyntaxForEach(ctx, argsPair, func(_ context.Context, _ int, _ bool, spec syntax.SyntaxValue) error {
		return parseExportSpec(lib, spec)
	})
	return err
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
			return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "export: expected symbol")
		}

		if carSym.Unwrap().(*values.Symbol).Key == "rename" {
			// (rename internal external)
			cdrExpr, ok := s.SyntaxCdr().(*syntax.SyntaxPair)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAPair, "export rename: expected internal and external names")
			}

			internalExpr := cdrExpr.SyntaxCar()
			internalSym, ok := internalExpr.(*syntax.SyntaxSymbol)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "export rename: internal name must be symbol")
			}

			cdrCdr, ok := cdrExpr.SyntaxCdr().(*syntax.SyntaxPair)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAPair, "export rename: expected external name")
			}

			externalExpr := cdrCdr.SyntaxCar()
			externalSym, ok := externalExpr.(*syntax.SyntaxSymbol)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "export rename: external name must be symbol")
			}

			internalName := internalSym.Unwrap().(*values.Symbol).Key
			externalName := externalSym.Unwrap().(*values.Symbol).Key
			lib.AddExport(externalName, internalName)
			return nil
		}

		return values.WrapForeignErrorf(values.ErrInvalidSyntax, "export: invalid spec form")

	default:
		return values.WrapForeignErrorf(values.ErrInvalidSyntax, "export: expected symbol or rename form")
	}
}

// processLibraryImport handles (import <import-set> ...) within a library.
func (p *CompileTimeContinuation) processLibraryImport(ctctx CompileTimeCallContext, lib *CompiledLibrary, args syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(args) {
		return nil // empty import is valid
	}

	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "import: expected list of import sets")
	}

	// Process each import set
	_, err := syntax.SyntaxForEach(ctctx.ctx, argsPair, func(ctx context.Context, _ int, _ bool, importSetExpr syntax.SyntaxValue) error {
		importSet, err := parseImportSet(ctx, importSetExpr)
		if err != nil {
			return err
		}

		// Load the library
		// Note: p.env is the library's environment, which has the registry via SetLibraryRegistry
		importedLib, err := LoadLibrary(ctx, importSet.LibraryName, p.env)
		if err != nil {
			return values.WrapForeignErrorf(err, "import: failed to load library %s",
				importSet.LibraryName.SchemeString())
		}

		// Apply import modifiers to get final bindings
		bindings, err := importSet.ApplyToExports(importedLib)
		if err != nil {
			return values.WrapForeignErrorf(err, "import: error applying modifiers for %s",
				importSet.LibraryName.SchemeString())
		}

		// Bind the imported names in the library's environment (lib.Env)
		for localName, externalName := range bindings {
			internalName := importedLib.GetInternalName(externalName)
			if internalName == "" {
				internalName = externalName
			}

			// Get the binding from the imported library's environment
			// First check the runtime environment, then the expand environment for syntax bindings,
			// then the compile environment for auxiliary syntax (else, =>)
			libSym := importedLib.Env.InternSymbol(values.NewSymbol(internalName))
			importedBinding := importedLib.Env.GetBinding(libSym)
			if importedBinding == nil {
				// Syntax bindings (define-syntax) are stored in the expand environment
				expandEnv := importedLib.Env.Expand()
				if expandEnv != nil {
					importedBinding = expandEnv.GetBinding(libSym)
				}
			}
			if importedBinding == nil {
				// Auxiliary syntax (else, =>) are stored in the compile environment
				compileEnv := importedLib.Env.Compile()
				if compileEnv != nil {
					importedBinding = compileEnv.GetBinding(libSym)
				}
			}
			if importedBinding == nil {
				return values.WrapForeignErrorf(values.ErrNoSuchBinding, "import: %s exports %q but binding not found",
					importSet.LibraryName.SchemeString(), internalName)
			}

			// Create binding in the importing library's environment
			localSym := lib.Env.InternSymbol(values.NewSymbol(localName))
			_, _ = lib.Env.MaybeCreateOwnGlobalBinding(localSym, importedBinding.BindingType())
			globalIdx := lib.Env.GetGlobalIndex(localSym)
			if globalIdx != nil {
				err := lib.Env.SetOwnGlobalValue(globalIdx, importedBinding.Value())
				if err != nil {
					return values.WrapForeignErrorf(err, "import: failed to set binding for %s", localName)
				}
			}

			// If it's a syntax binding, also copy to expand phase
			if importedBinding.BindingType() == environment.BindingTypeSyntax {
				expandEnv := lib.Env.Expand()
				_, _ = expandEnv.MaybeCreateOwnGlobalBinding(localSym, environment.BindingTypeSyntax)
				expandIdx := expandEnv.GetGlobalIndex(localSym)
				if expandIdx != nil {
					_ = expandEnv.SetOwnGlobalValue(expandIdx, importedBinding.Value())
				}
			}
		}

		return nil
	})
	return err
}

// parseImportSet parses an import set with optional modifiers.
// Import sets can be:
//   - (<library-name>)              : import all exports
//   - (only <import-set> <id> ...)  : import only specified identifiers
//   - (except <import-set> <id> ...): import all except specified
//   - (prefix <import-set> <prefix>): add prefix to all imported names
//   - (rename <import-set> (<old> <new>) ...): rename specific imports
//   - (for-syntax <import-set>)     : import at phase +1 (macro expansion)
//   - (for-template <import-set>)   : import at phase -1
//   - (for-meta <n> <import-set>)   : import at phase +n
func parseImportSet(ctx context.Context, expr syntax.SyntaxValue) (*ImportSet, error) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "import set must be a list")
	}

	// Check if first element is a modifier keyword
	carExpr := pair.SyntaxCar()
	carSym, ok := carExpr.(*syntax.SyntaxSymbol)
	if ok {
		keyword := carSym.Unwrap().(*values.Symbol).Key

		switch keyword {
		case "only":
			return parseImportSetOnly(ctx, pair)
		case "except":
			return parseImportSetExcept(ctx, pair)
		case "prefix":
			return parseImportSetPrefix(ctx, pair)
		case "rename":
			return parseImportSetRename(ctx, pair)
		case "for-syntax":
			return parseImportSetForSyntax(ctx, pair)
		case "for-template":
			return parseImportSetForTemplate(ctx, pair)
		case "for-meta":
			return parseImportSetForMeta(ctx, pair)
		}
	}

	// Not a modifier, must be a library name
	libName, err := parseLibraryName(ctx, expr)
	if err != nil {
		return nil, err
	}
	return NewImportSet(libName), nil
}

// parseImportSetOnly parses (only <import-set> <id> ...)
func parseImportSetOnly(ctx context.Context, pair *syntax.SyntaxPair) (*ImportSet, error) {
	cdrExpr, ok := pair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "only: expected import-set and identifiers")
	}

	// Get nested import set
	nestedExpr := cdrExpr.SyntaxCar()
	importSet, err := parseImportSet(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}

	// Get identifiers
	idsExpr, ok := cdrExpr.Cdr().(syntax.SyntaxValue)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "only: expected identifiers")
	}

	ids, err := parseIdentifierList(ctx, idsExpr)
	if err != nil {
		return nil, err
	}

	importSet.Only = ids
	return importSet, nil
}

// parseImportSetExcept parses (except <import-set> <id> ...)
func parseImportSetExcept(ctx context.Context, pair *syntax.SyntaxPair) (*ImportSet, error) {
	cdrExpr, ok := pair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "except: expected import-set and identifiers")
	}

	// Get nested import set
	nestedExpr := cdrExpr.SyntaxCar()
	importSet, err := parseImportSet(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}

	// Get identifiers
	idsExpr := cdrExpr.SyntaxCdr()
	ids, err := parseIdentifierList(ctx, idsExpr)
	if err != nil {
		return nil, err
	}

	importSet.Except = ids
	return importSet, nil
}

// parseImportSetPrefix parses (prefix <import-set> <prefix>)
func parseImportSetPrefix(ctx context.Context, pair *syntax.SyntaxPair) (*ImportSet, error) {
	cdrExpr, ok := pair.Cdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "prefix: expected import-set and prefix")
	}

	// Get nested import set
	nestedExpr := cdrExpr.SyntaxCar()
	importSet, err := parseImportSet(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}

	// Get prefix
	prefixPair, ok := cdrExpr.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "prefix: expected prefix identifier")
	}

	prefixExpr := prefixPair.SyntaxCar()
	prefixSym, ok := prefixExpr.(*syntax.SyntaxSymbol)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "prefix: prefix must be a symbol")
	}

	importSet.Prefix = prefixSym.Unwrap().(*values.Symbol).Key
	return importSet, nil
}

// parseImportSetRename parses (rename <import-set> (<old> <new>) ...)
func parseImportSetRename(ctx context.Context, pair *syntax.SyntaxPair) (*ImportSet, error) {
	cdrExpr, ok := pair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "rename: expected import-set and rename pairs")
	}

	// Get nested import set
	nestedExpr := cdrExpr.SyntaxCar()
	importSet, err := parseImportSet(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}

	// Get rename pairs
	renamesExpr := cdrExpr.SyntaxCdr()
	if syntax.IsSyntaxEmptyList(renamesExpr) {
		return importSet, nil
	}

	renamesPair, ok := renamesExpr.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "rename: expected list of rename pairs")
	}

	_, err = syntax.SyntaxForEach(ctx, renamesPair, func(_ context.Context, _ int, _ bool, renamePairExpr syntax.SyntaxValue) error {
		renamePair, ok := renamePairExpr.(*syntax.SyntaxPair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "rename: expected (old new) pair")
		}

		oldExpr := renamePair.SyntaxCar()
		oldSym, ok := oldExpr.(*syntax.SyntaxSymbol)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "rename: old name must be symbol")
		}

		newPair, ok := renamePair.SyntaxCdr().(*syntax.SyntaxPair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "rename: expected new name")
		}

		newExpr := newPair.SyntaxCar()
		newSym, ok := newExpr.(*syntax.SyntaxSymbol)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "rename: new name must be symbol")
		}

		oldName := oldSym.Unwrap().(*values.Symbol).Key
		newName := newSym.Unwrap().(*values.Symbol).Key
		importSet.Renames[oldName] = newName
		return nil
	})

	return importSet, err
}

// parseImportSetForSyntax parses (for-syntax <import-set>)
// Adds +1 to the phase shift of the nested import set.
func parseImportSetForSyntax(ctx context.Context, pair *syntax.SyntaxPair) (*ImportSet, error) {
	cdrExpr, ok := pair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "for-syntax: expected import-set")
	}

	// Get nested import set
	nestedExpr := cdrExpr.SyntaxCar()
	importSet, err := parseImportSet(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}

	// Add +1 to phase shift (composable)
	importSet.PhaseShift++
	return importSet, nil
}

// parseImportSetForTemplate parses (for-template <import-set>)
// Adds -1 to the phase shift of the nested import set.
func parseImportSetForTemplate(ctx context.Context, pair *syntax.SyntaxPair) (*ImportSet, error) {
	cdrExpr, ok := pair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "for-template: expected import-set")
	}

	// Get nested import set
	nestedExpr := cdrExpr.SyntaxCar()
	importSet, err := parseImportSet(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}

	// Add -1 to phase shift (composable)
	importSet.PhaseShift--
	return importSet, nil
}

// parseImportSetForMeta parses (for-meta <n> <import-set>)
// Adds n to the phase shift of the nested import set.
func parseImportSetForMeta(ctx context.Context, pair *syntax.SyntaxPair) (*ImportSet, error) {
	cdrExpr, ok := pair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "for-meta: expected phase level and import-set")
	}

	// Get phase level (integer)
	phaseExpr := cdrExpr.SyntaxCar()
	phaseInt, ok := phaseExpr.Unwrap().(*values.Integer)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAnInteger, "for-meta: expected integer phase level")
	}

	// Get nested import set
	importSetPair, ok := cdrExpr.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "for-meta: expected import-set after phase level")
	}

	nestedExpr := importSetPair.SyntaxCar()
	importSet, err := parseImportSet(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}

	// Add n to phase shift (composable)
	importSet.PhaseShift += int(phaseInt.Value)
	return importSet, nil
}

// parseIdentifierList parses a list of identifiers into a string slice.
func parseIdentifierList(ctx context.Context, expr syntax.SyntaxValue) ([]string, error) {
	if syntax.IsSyntaxEmptyList(expr) {
		return nil, nil
	}

	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "expected list of identifiers")
	}

	var ids []string
	_, err := syntax.SyntaxForEach(ctx, pair, func(_ context.Context, _ int, _ bool, idExpr syntax.SyntaxValue) error {
		idSym, ok := idExpr.(*syntax.SyntaxSymbol)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "expected identifier symbol")
		}
		ids = append(ids, idSym.Unwrap().(*values.Symbol).Key)
		return nil
	})
	if err != nil {
		return nil, err
	}
	return ids, nil
}

// parseLibraryName extracts a LibraryName from a syntax expression like (scheme base).
func parseLibraryName(ctx context.Context, expr syntax.SyntaxValue) (LibraryName, error) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return LibraryName{}, values.WrapForeignErrorf(values.ErrNotAPair, "library name must be a list")
	}

	var parts []string
	_, err := syntax.SyntaxForEach(ctx, pair, func(_ context.Context, _ int, _ bool, partExpr syntax.SyntaxValue) error {
		sym, ok := partExpr.(*syntax.SyntaxSymbol)
		if ok {
			parts = append(parts, sym.Unwrap().(*values.Symbol).Key)
			return nil
		}
		// Could be a number (for versioned library names)
		num, ok := partExpr.Unwrap().(*values.Integer)
		if ok {
			parts = append(parts, fmt.Sprintf("%d", num.Value))
			return nil
		}
		return values.WrapForeignErrorf(values.ErrInvalidSyntax, "library name part must be identifier or integer")
	})
	if err != nil {
		return LibraryName{}, err
	}
	if len(parts) == 0 {
		return LibraryName{}, values.WrapForeignErrorf(values.ErrInvalidSyntax, "library name cannot be empty")
	}
	return NewLibraryName(parts...), nil
}

// CompileImport handles top-level (import <import-set> ...).
//
// This is for top-level imports outside of a library definition.
// It loads the specified libraries and binds their exports in the current environment.
//
// Supports Racket-style phased imports:
//   - (import (scheme base))                    ; Phase 0 (runtime)
//   - (import (for-syntax (scheme base)))       ; Phase 1 (expand)
//   - (import (for-template (scheme base)))     ; Phase -1
//   - (import (for-meta 2 (scheme base)))       ; Phase 2
func (p *CompileTimeContinuation) CompileImport(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// expr is (<import-set> ...) - args after 'import' keyword
	if syntax.IsSyntaxEmptyList(expr) {
		return nil // empty import is valid
	}

	importSets, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "import: expected list of import sets")
	}

	// Process each import set
	v, err := syntax.SyntaxForEach(ctctx.ctx, importSets, func(ctx context.Context, _ int, _ bool, importSetExpr syntax.SyntaxValue) error {
		importSet, err := parseImportSet(ctx, importSetExpr)
		if err != nil {
			return err
		}

		// Load the library
		lib, err := LoadLibrary(ctx, importSet.LibraryName, p.env)
		if err != nil {
			return values.WrapForeignErrorf(err, "import: failed to load library %s",
				importSet.LibraryName.SchemeString())
		}

		// Apply import modifiers (only, except, prefix, rename) to get final bindings
		bindings, err := importSet.ApplyToExports(lib)
		if err != nil {
			return values.WrapForeignErrorf(err, "import: error applying modifiers for %s",
				importSet.LibraryName.SchemeString())
		}

		// Copy bindings to the target phase
		err = CopyLibraryBindingsToEnvAtPhase(lib, bindings, p.env, importSet.PhaseShift)
		if err != nil {
			return values.WrapForeignErrorf(err, "import: error copying bindings from %s",
				importSet.LibraryName.SchemeString())
		}

		return nil
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "import: error processing import sets")
	}
	if !syntax.IsSyntaxEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "import: unexpected return value")
	}
	return nil
}

// CompileExport handles top-level (export <export-spec> ...).
//
// This is only valid within a library definition. At top-level, it's an error.
func (p *CompileTimeContinuation) CompileExport(_ CompileTimeCallContext, _ syntax.SyntaxValue) error {
	return values.WrapForeignErrorf(values.ErrInvalidSyntax, "export: only valid within define-library")
}

// CompileDefineSyntax handles (define-syntax keyword transformer-expr).
//
// This is the compile-time handler for R7RS define-syntax. Unlike most
// definitions, define-syntax is processed entirely at compile time:
//
//  1. Parse the form: (define-syntax keyword (syntax-rules ...))
//  2. Compile the syntax-rules transformer to a MachineClosure
//  3. Store the closure in the environment with BindingTypeSyntax
//  4. Emit NO runtime operations (the binding is already established)
//
// The BindingTypeSyntax marker is crucial: when the expander encounters
// a symbol, it checks if that symbol is bound to a syntax transformer.
// If so, it invokes the transformer closure to expand the macro.
//
// This is how derived expressions like 'let' work: they're defined as
// macros using define-syntax, and expand to lambda expressions:
//
//	(define-syntax let
//	  (syntax-rules ()
//	    ((let ((name val) ...) body)
//	     ((lambda (name ...) body) val ...))))
//
// Reference: R7RS Section 5.4 (Syntax definitions)
func (p *CompileTimeContinuation) CompileDefineSyntax(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	err := p.ensureState("define-syntax")
	if err != nil {
		return err
	}
	// expr is (keyword transformer-expr) - the args after 'define-syntax' has been stripped
	argsPair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxPair, "define-syntax: expected keyword and transformer")
	}
	// Get the keyword to bind
	keywordStx := argsPair.SyntaxCar()
	if keywordStx == nil {
		return values.WrapForeignErrorf(values.ErrUnexpectedNil, "define-syntax: missing keyword")
	}
	keywordSym, ok := keywordStx.(*syntax.SyntaxSymbol)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "define-syntax: keyword must be a symbol")
	}
	keyword := keywordSym.Unwrap().(*values.Symbol)
	// Get the transformer expression
	transformerCdr := argsPair.Cdr()
	if transformerCdr == nil {
		return values.WrapForeignErrorf(values.ErrUnexpectedNil, "define-syntax: missing transformer expression")
	}
	transformerPair, ok := transformerCdr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxPair, "define-syntax: expected transformer expression")
	}
	transformerExpr := transformerPair.SyntaxCar()
	if transformerExpr == nil {
		return values.WrapForeignErrorf(values.ErrUnexpectedNil, "define-syntax: missing transformer expression")
	}

	// Compile the transformer (supports syntax-rules and lambda)
	closure, err := compileTransformerToMachineClosure(ctctx.ctx, p.env, transformerExpr)
	if err != nil {
		return values.WrapForeignErrorf(err, "could not compile transformer")
	}

	// Store the transformer in the expand phase environment with BindingTypeSyntax
	// R7RS requires syntax bindings to live in the expand phase, separate from runtime bindings
	expandEnv := p.env.Expand()
	globalIndex, created := expandEnv.MaybeCreateOwnGlobalBinding(keyword, environment.BindingTypeSyntax)
	if !created {
		// Update existing binding
		globalIndex = expandEnv.GetGlobalIndex(keyword)
	}
	if globalIndex != nil {
		// Set scopes from the keyword symbol for hygiene
		// This ensures local define-syntax bindings have correct scopes for lookup
		symbolScopes := keywordSym.Scopes()
		binding := expandEnv.GetGlobalBinding(globalIndex)
		if binding != nil && symbolScopes != nil {
			binding.SetScopes(symbolScopes)
		}

		err = expandEnv.SetOwnGlobalValue(globalIndex, closure)
		if err != nil {
			return err
		}
	}

	// define-syntax is compile-time only, emit no runtime operations
	return nil
}

// CompileCondExpand compiles a cond-expand expression.
// cond-expand is evaluated at compile-time and expands to the body of the first
// clause whose feature requirement is satisfied.
//
// Syntax: (cond-expand <clause> ...)
// where <clause> is (<feature-requirement> <expression> ...)
//
// Example:
//
//	(cond-expand
//	  (r7rs (display "R7RS"))
//	  (else (display "other")))
func (p *CompileTimeContinuation) CompileCondExpand(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(expr) {
		return values.WrapForeignErrorf(values.ErrNoMatchingClause, "cond-expand: no clauses")
	}

	argsPair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "cond-expand: expected list of clauses")
	}

	// Get the library registry for checking library availability
	var registry *LibraryRegistry
	regAny := p.env.LibraryRegistry()
	if regAny != nil {
		registry, _ = regAny.(*LibraryRegistry)
	}

	// Find the first matching clause
	var matchedClause syntax.SyntaxValue
	_, err := syntax.SyntaxForEach(ctctx.ctx, argsPair, func(_ context.Context, _ int, _ bool, clause syntax.SyntaxValue) error {
		if matchedClause != nil {
			return nil // Already found a match
		}

		clausePair, ok := clause.(*syntax.SyntaxPair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "cond-expand: clause must be a list")
		}

		// Get the feature requirement (car of clause)
		reqExpr := clausePair.SyntaxCar()
		// Parse and evaluate the feature requirement
		req, err := parseFeatureRequirement(ctctx.ctx, reqExpr)
		if err != nil {
			return values.WrapForeignErrorf(err, "cond-expand: invalid feature requirement")
		}

		if req.IsSatisfied(registry) {
			matchedClause = clausePair
		}

		return nil
	})
	if err != nil {
		return err
	}

	if matchedClause == nil {
		return values.WrapForeignErrorf(values.ErrNoMatchingClause, "cond-expand: no matching clause")
	}

	// Compile the expressions in the matched clause
	matchedPair := matchedClause.(*syntax.SyntaxPair)
	bodyExpr := matchedPair.SyntaxCdr()
	if syntax.IsSyntaxEmptyList(bodyExpr) {
		// Empty body - emit void
		voidIdx := p.template.MaybeAppendLiteral(values.Void)
		p.AppendOperations(
			NewOperationLoadLiteralByLiteralIndexImmediate(voidIdx),
		)
		return nil
	}

	bodyPair, ok := bodyExpr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "cond-expand: expected list of expressions")
	}

	// Expand and compile each body expression
	// (since cond-expand is not expanded, we must expand the body here)
	_, err = syntax.SyntaxForEach(ctctx.ctx, bodyPair, func(_ context.Context, _ int, hasNext bool, expr syntax.SyntaxValue) error {
		// Expand the expression
		expanded, expandErr := NewExpanderTimeContinuation(p.env).ExpandExpression(ctctx.ctx, expr)
		if expandErr != nil {
			return values.WrapForeignErrorf(expandErr, "cond-expand: error expanding body expression")
		}

		// Compile the expanded expression
		// Use the appropriate context for tail position (only last expression is in tail position)
		bodyCtx := ctctx
		if hasNext {
			bodyCtx = ctctx.NotInTail()
		}
		return p.CompileExpression(bodyCtx, expanded)
	})
	return err
}

// processCondExpand handles (cond-expand <clause> ...) within a library.
// Each clause is (<feature-requirement> <library-declaration> ...)
// The first clause whose feature requirement is satisfied has its declarations processed.
func (p *CompileTimeContinuation) processCondExpand(ctctx CompileTimeCallContext, lib *CompiledLibrary, args syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(args) {
		return values.WrapForeignErrorf(values.ErrNoMatchingClause, "cond-expand: no clauses")
	}

	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "cond-expand: expected list of clauses")
	}

	// Get the library registry for checking library availability
	var registry *LibraryRegistry
	regAny := p.env.LibraryRegistry()
	if regAny != nil {
		registry, _ = regAny.(*LibraryRegistry)
	}

	// Iterate through clauses
	var matchedClause syntax.SyntaxValue
	_, err := syntax.SyntaxForEach(ctctx.ctx, argsPair, func(_ context.Context, _ int, _ bool, clause syntax.SyntaxValue) error {
		if matchedClause != nil {
			return nil // Already found a match
		}

		clausePair, ok := clause.(*syntax.SyntaxPair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "cond-expand: clause must be a list")
		}

		// Get the feature requirement (car of clause)
		reqExpr := clausePair.SyntaxCar()
		// Parse and evaluate the feature requirement
		req, err := parseFeatureRequirement(ctctx.ctx, reqExpr)
		if err != nil {
			return values.WrapForeignErrorf(err, "cond-expand: invalid feature requirement")
		}

		if req.IsSatisfied(registry) {
			matchedClause = clausePair
		}

		return nil
	})
	if err != nil {
		return err
	}

	if matchedClause == nil {
		return values.WrapForeignErrorf(values.ErrNoMatchingClause, "cond-expand: no matching clause")
	}

	// Process the declarations in the matched clause
	matchedPair := matchedClause.(*syntax.SyntaxPair)
	declsExpr := matchedPair.SyntaxCdr()
	if syntax.IsSyntaxEmptyList(declsExpr) {
		return nil // Empty clause body is valid
	}

	declsPair, ok := declsExpr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "cond-expand: expected list of declarations")
	}

	// Process each declaration
	_, err = syntax.SyntaxForEach(ctctx.ctx, declsPair, func(_ context.Context, _ int, _ bool, decl syntax.SyntaxValue) error {
		return p.processLibraryDeclaration(ctctx, lib, decl)
	})
	return err
}

// parseFeatureRequirement parses a feature requirement expression.
// Feature requirements can be:
//   - <identifier> - simple feature check
//   - (library <library-name>) - library availability check
//   - (and <req> ...) - all requirements must be satisfied
//   - (or <req> ...) - at least one must be satisfied
//   - (not <req>) - must NOT be satisfied
//   - else - always satisfied (only valid as the last clause)
func parseFeatureRequirement(ctx context.Context, expr syntax.SyntaxValue) (FeatureRequirement, error) {
	switch v := expr.(type) {
	case *syntax.SyntaxSymbol:
		name := v.Unwrap().(*values.Symbol).Key
		if name == "else" {
			return NewElseRequirement(), nil
		}
		return NewFeatureIdentifier(name), nil

	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(v) {
			return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "empty feature requirement")
		}

		carExpr := v.SyntaxCar()
		carSym, ok := carExpr.(*syntax.SyntaxSymbol)
		if !ok {
			return nil, values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "feature requirement must start with symbol")
		}

		keyword := carSym.Unwrap().(*values.Symbol).Key
		argsExpr := v.SyntaxCdr()
		switch keyword {
		case "library":
			// (library <library-name>)
			argsPair, ok := argsExpr.(*syntax.SyntaxPair)
			if !ok || syntax.IsSyntaxEmptyList(argsPair) {
				return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "library: expected library name")
			}
			libNameExpr := argsPair.SyntaxCar()
			libName, err := parseLibraryName(ctx, libNameExpr)
			if err != nil {
				return nil, values.WrapForeignErrorf(err, "library: invalid library name")
			}
			return NewLibraryRequirement(libName), nil

		case "and":
			// (and <req> ...)
			reqs, err := parseFeatureRequirementList(ctx, argsExpr)
			if err != nil {
				return nil, values.WrapForeignErrorf(err, "and: invalid requirements")
			}
			return NewAndRequirement(reqs...), nil

		case "or":
			// (or <req> ...)
			reqs, err := parseFeatureRequirementList(ctx, argsExpr)
			if err != nil {
				return nil, values.WrapForeignErrorf(err, "or: invalid requirements")
			}
			return NewOrRequirement(reqs...), nil

		case "not":
			// (not <req>)
			argsPair, ok := argsExpr.(*syntax.SyntaxPair)
			if !ok || syntax.IsSyntaxEmptyList(argsPair) {
				return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "not: expected one requirement")
			}
			reqExpr := argsPair.SyntaxCar()
			req, err := parseFeatureRequirement(ctx, reqExpr)
			if err != nil {
				return nil, values.WrapForeignErrorf(err, "not: invalid requirement")
			}
			return NewNotRequirement(req), nil

		default:
			return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "unknown feature requirement keyword: %s", keyword)
		}

	default:
		return nil, values.WrapForeignErrorf(values.ErrInvalidArgument, "invalid feature requirement type: %T", expr)
	}
}

// parseFeatureRequirementList parses a list of feature requirements.
func parseFeatureRequirementList(ctx context.Context, expr syntax.SyntaxValue) ([]FeatureRequirement, error) {
	if syntax.IsSyntaxEmptyList(expr) {
		return nil, nil
	}

	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "expected list of requirements")
	}

	var reqs []FeatureRequirement
	_, err := syntax.SyntaxForEach(ctx, pair, func(_ context.Context, _ int, _ bool, v syntax.SyntaxValue) error {
		req, err := parseFeatureRequirement(ctx, v)
		if err != nil {
			return err
		}
		reqs = append(reqs, req)
		return nil
	})
	if err != nil {
		return nil, err
	}
	return reqs, nil
}

// processIncludeLibraryDeclarations handles (include-library-declarations <string> ...) within a library.
// It reads each file and processes its contents as library declarations.
func (p *CompileTimeContinuation) processIncludeLibraryDeclarations(ctctx CompileTimeCallContext, lib *CompiledLibrary, args syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(args) {
		return nil // Empty is valid (no-op)
	}

	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "include-library-declarations: expected list of filenames")
	}

	// Process each filename
	_, err := syntax.SyntaxForEach(ctctx.ctx, argsPair, func(ctx context.Context, _ int, _ bool, v syntax.SyntaxValue) error {
		fn, ok := v.Unwrap().(*values.String)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAString, "include-library-declarations: expected string filename")
		}

		// Find and open the file
		file, filePath, err := findFile(p, ctctx, fn.Value)
		if err != nil {
			return values.WrapForeignErrorf(err, "include-library-declarations: failed to find file %q", fn.Value)
		}
		if file == nil {
			return values.WrapForeignErrorf(values.ErrFileNotFound, "include-library-declarations: file not found: %q", fn.Value)
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
				return values.WrapForeignErrorf(readErr, "include-library-declarations: error reading %q", fn.Value)
			}

			// Process the form as a library declaration
			err := p.processLibraryDeclaration(ctctx, lib, stx)
			if err != nil {
				return values.WrapForeignErrorf(err, "include-library-declarations: error processing declaration from %q", fn.Value)
			}
		}

		return nil
	})
	if err != nil {
		return err
	}
	return nil
}
