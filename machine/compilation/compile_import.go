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
	"context"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
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
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotAPair, "import: expected list of import sets"))
	}

	// Process each import set
	v, err := syntax.SyntaxForEach(ctctx.ctx, importSets, func(ctx context.Context, _ int, _ bool, importSetExpr syntax.SyntaxValue) error {
		return ResolveAndInstallImportSet(ctx, importSetExpr.UnwrapAll(), p.env, environment.PhaseCompile, p.evaluator)
	})
	if err != nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "import: error processing import sets"))
	}
	if !syntax.IsSyntaxEmptyList(v) {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotAList, "import: unexpected return value"))
	}
	return nil
}

// processLibraryImport handles (import <import-set> ...) within a library.
//
// This shares the parse→load→apply prefix with ResolveAndInstallImportSet
// (via resolveImportSet) but diverges at installation: library-internal
// imports install directly into lib.Env via copyLibraryBindingsDirect,
// because lib.Env.AtPhase() routes to the parent's phase registry.
func (p *CompileTimeContinuation) processLibraryImport(ctctx CompileTimeCallContext, lib *CompiledLibrary, args syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(args) {
		return nil // empty import is valid
	}

	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotAPair, "import: expected list of import sets"))
	}

	// Process each import set
	_, err := syntax.SyntaxForEach(ctctx.ctx, argsPair, func(ctx context.Context, _ int, _ bool, importSetExpr syntax.SyntaxValue) error {
		res, err := resolveImportSet(ctx, importSetExpr.UnwrapAll(), p.env, p.evaluator)
		if err != nil {
			return err
		}

		fireImportObserver(p.env, res.Library, res.Bindings, lib.Name, environment.PhaseCompile)

		return copyLibraryBindingsDirect(res.Library, res.Bindings, lib.Env)
	})
	return err
}
