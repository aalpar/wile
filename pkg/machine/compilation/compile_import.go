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

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
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

	// Collect every import set first: the form installs all of them or none.
	var datums []values.Value
	v, err := syntax.SyntaxForEach(ctctx.ctx, importSets, func(_ context.Context, _ int, _ bool, importSetExpr syntax.SyntaxValue) error {
		datums = append(datums, importSetExpr.UnwrapAll())
		return nil
	})
	if err != nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "import: error processing import sets"))
	}
	if !syntax.IsSyntaxEmptyList(v) {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotAList, "import: unexpected return value"))
	}
	err = ResolveAndInstallImportSets(ctctx.ctx, datums, p.env, ImportStageCompile, p.evaluator)
	if err != nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "import: error processing import sets"))
	}
	return nil
}

// processLibraryImport handles (import <import-set> ...) within a library.
//
// This shares the parse→load→apply prefix with ResolveAndInstallImportSets
// (via resolveImportSets) but diverges at installation: library-internal
// imports install directly into lib.Env via planCopyDirect,
// because lib.Env.AtPhase() routes to the parent's phase registry. All or none,
// as at the top level.
func (p *CompileTimeContinuation) processLibraryImport(ctctx CompileTimeCallContext, lib *CompiledLibrary, args syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(args) {
		return nil // empty import is valid
	}

	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotAPair, "import: expected list of import sets"))
	}

	var pending []pendingImport
	_, err := syntax.SyntaxForEach(ctctx.ctx, argsPair, func(ctx context.Context, _ int, _ bool, importSetExpr syntax.SyntaxValue) error {
		wrap := func(err error) error {
			return wrapSourcedError(importSetExpr.SourceContext(), err)
		}
		resolved, err := resolveImportSets(ctx, importSetExpr.UnwrapAll(), p.env, p.evaluator)
		if err != nil {
			return wrap(err)
		}
		for _, res := range resolved {
			pi, err := p.planLibraryImport(lib, res)
			if err != nil {
				return wrap(err)
			}
			pi.wrap = wrap
			pending = append(pending, pi)
		}
		return nil
	})
	if err != nil {
		return err
	}
	return installPendingImports(pending)
}

// planLibraryImport plans one resolved library import into lib.Env for
// processLibraryImport.
func (p *CompileTimeContinuation) planLibraryImport(lib *CompiledLibrary, res *ResolvedImportSet) (pendingImport, error) {
	// Compose the parsed for-syntax/for-meta shift with the library env's own
	// phase, the same composition ResolveAndInstallImportSets performs at top
	// level. Based on lib.Env rather than p.env because lib.Env is the frame
	// the install targets, so the base and the target cannot drift apart.
	targetPhase, err := composePhaseShift("import", lib.Env.PhaseLevel(), res.ImportSet.PhaseShift)
	if err != nil {
		return pendingImport{}, err
	}
	plan, err := planCopyDirect(res.Library, res.Bindings, lib.Env, targetPhase)
	if err != nil {
		return pendingImport{}, err
	}
	env := p.env
	fire := func() {
		fireImportObserver(env, res.Library, res.Bindings, lib.Name, ImportStageCompile)
	}
	return pendingImport{plan: plan, fire: fire}, nil
}
