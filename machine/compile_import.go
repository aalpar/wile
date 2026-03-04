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
	"context"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

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
		return werr.WrapForeignErrorf(werr.ErrNotAPair, "import: expected list of import sets")
	}

	// Process each import set
	v, err := syntax.SyntaxForEach(ctctx.ctx, importSets, func(ctx context.Context, _ int, _ bool, importSetExpr syntax.SyntaxValue) error {
		return ResolveAndInstallImportSet(ctx, importSetExpr.UnwrapAll(), p.env, environment.PhaseCompile)
	})
	if err != nil {
		return werr.WrapForeignErrorf(err, "import: error processing import sets")
	}
	if !syntax.IsSyntaxEmptyList(v) {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "import: unexpected return value")
	}
	return nil
}

// processLibraryImport handles (import <import-set> ...) within a library.
func (p *CompileTimeContinuation) processLibraryImport(ctctx CompileTimeCallContext, lib *CompiledLibrary, args syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(args) {
		return nil // empty import is valid
	}

	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAPair, "import: expected list of import sets")
	}

	// Process each import set
	_, err := syntax.SyntaxForEach(ctctx.ctx, argsPair, func(ctx context.Context, _ int, _ bool, importSetExpr syntax.SyntaxValue) error {
		importSet, err := ParseImportSetFromDatum(ctx, importSetExpr.UnwrapAll())
		if err != nil {
			return err
		}

		// Load the library
		// Note: p.env is the library's environment, which has the registry via SetLibraryRegistry
		importedLib, err := LoadLibrary(ctx, importSet.LibraryName, p.env)
		if err != nil {
			return werr.WrapForeignErrorf(err, "import: failed to load library %s",
				importSet.LibraryName.SchemeString())
		}

		// Apply import modifiers to get final bindings
		bindings, err := importSet.ApplyToExports(importedLib)
		if err != nil {
			return werr.WrapForeignErrorf(err, "import: error applying modifiers for %s",
				importSet.LibraryName.SchemeString())
		}

		fireImportObserver(p.env, importedLib, bindings, lib.Name.Parts, environment.PhaseCompile)

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
				return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "import: %s exports %q but binding not found",
					importSet.LibraryName.SchemeString(), internalName)
			}

			// Create binding in the importing library's environment
			localSym := lib.Env.InternSymbol(values.NewSymbol(localName))
			_, _ = lib.Env.MaybeCreateOwnGlobalBinding(localSym, importedBinding.BindingType())
			globalIdx := lib.Env.GetGlobalIndex(localSym)
			if globalIdx != nil {
				err := lib.Env.SetOwnGlobalValue(globalIdx, importedBinding.Value())
				if err != nil {
					return werr.WrapForeignErrorf(err, "import: failed to set binding for %s", localName)
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
