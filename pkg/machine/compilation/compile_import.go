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
	"github.com/aalpar/wile/pkg/syntax"
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
	err := ResolveAndInstallImportSets(ctctx.ctx, expr, p.env, ImportStageCompile, p.evaluator)
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
	sets, err := importSetForms(ctctx.ctx, args)
	if err != nil {
		return err
	}
	var pending []pendingImport
	for _, setExpr := range sets {
		wrap := func(err error) error {
			return wrapSourcedError(setExpr.SourceContext(), err)
		}
		resolved, err := resolveImportSets(ctctx.ctx, setExpr.UnwrapAll(), p.env, p.evaluator)
		if err != nil {
			return wrap(err)
		}
		for _, res := range resolved {
			pi, err := p.planLibraryImport(lib, res, wrap)
			if err != nil {
				return wrap(err)
			}
			pending = append(pending, pi)
		}
	}
	return installPendingImports(pending)
}

// planLibraryImport plans one resolved library import into lib.Env for
// processLibraryImport; wrap is the context the import's errors carry.
func (p *CompileTimeContinuation) planLibraryImport(lib *CompiledLibrary, res *ResolvedImportSet, wrap func(error) error) (pendingImport, error) {
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
	return pendingImport{plan: plan, fire: fire, wrap: wrap}, nil
}
