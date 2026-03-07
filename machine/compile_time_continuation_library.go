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

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/werr"
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
		return werr.WrapForeignErrorf(werr.ErrNotAList, "expected a list of expressions, got %T", expr)
	}

	// Collect forms into a slice
	var forms []syntax.SyntaxValue
	_, err := syntax.SyntaxForEach(ctctx.ctx, expr, func(_ context.Context, _ int, _ bool, v syntax.SyntaxValue) error {
		forms = append(forms, v)
		return nil
	})
	if err != nil {
		return werr.WrapForeignErrorf(err, "failed to collect library body forms")
	}

	// Pass 1: Expand all forms, compiling define-syntax as encountered
	expander := NewExpanderTimeContinuation(ctctx.ctx, p.env)
	expander.libraryScope = p.libraryScope
	expandedForms, err := expander.ExpandBodyWithDefineSyntax(forms)
	if err != nil {
		return werr.WrapForeignErrorf(err, "library: error expanding forms")
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
			return werr.WrapForeignErrorf(compileErr, "library: error compiling form")
		}
	}

	return nil
}
