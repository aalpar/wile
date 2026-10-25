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

// expander_cond_expand.go splices (cond-expand ...) at expand time, as
// expander_include.go does for include. R7RS §4.2.1: the selected clause's
// forms occur in place of the cond-expand, so the expander must see them as the
// surrounding unit's own forms. Left to the compiler, the body expanded in a
// unit of its own: a define-syntax in it never reached the forms after it, a
// define in it never reached the unit's set!-evidence, and a relative include in
// it resolved after the including file had left the load stack.

import (
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
)

// expandCondExpandForm expands (cond-expand clause ...) to (begin body ...) for
// the body of the first clause whose requirement holds.
//
// A malformed form is an error here, not a bail: nothing downstream validates
// cond-expand any more. resolveCondExpandClause reports an empty clause list, a
// non-list clause, an improper clause list, a bad requirement and the absence of
// any matching clause, each wrapped at the form's location.
//
// EXPRESSION POSITION gets the same body treatment as a statement-position
// form, decided rather than inherited: the body expands through
// ExpandBodyWithDefineSyntax, exactly as expandBeginForm does for (begin ...)
// in any position. A clause body in expression position is one expression in
// practice (graph.scm's (or (cond-expand ...) ...)), for which the body
// treatment predeclares nothing and installs no macro; the alternative, a
// second expansion mode keyed on position, would have to be threaded to every
// caller of ExpandExpression to change nothing observable.
//
// The clause body's forms already carry the enclosing scopes (the scope walk of
// every enclosing expansion reached them), so unlike an included file's forms
// they are not re-stamped. Use-site scopes are NOT pruned here: pruning belongs
// at the transformer boundary (pruneUseSiteScopes treats a cond-expand in macro
// output as a splicing head and prunes each clause body), so a cond-expand a
// macro merely RECEIVES as an argument keeps its use-site scopes and cannot
// capture the macro's own binders. The head is a FRESH-scope begin for the
// reason spliceBegin gives.
//
// No test tells the body treatment from per-form expansion: replacing this
// call with an ExpandExpression per form leaves every suite green, on master
// too (checked 2026-10-25). A define in the clause registers its variable when
// it is expanded, before the form after it, so the pre-scan adds nothing a
// following use can see; the expression-position row that was meant to show
// it now refuses its define.
func (p *ExpanderTimeContinuation) expandCondExpandForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	srcCtx := expr.SourceContext()
	if srcCtx == nil || syntax.IsSyntaxEmptyList(expr) {
		// (cond-expand) has no operands, hence no location of their own.
		srcCtx = sym.SourceContext()
	}
	forms, err := resolveCondExpandClause(p.ctx, p.env, p.evaluator, expr, srcCtx)
	if err != nil {
		return nil, err
	}
	if len(forms) == 0 {
		return spliceBegin(sym, "cond-expand", nil), nil
	}
	expanded, err := p.ExpandBodyWithDefineSyntax(forms)
	if err != nil {
		return nil, wrapSourcedError(srcCtx, werr.WrapForeignErrorf(err, "cond-expand: failed to expand clause body"))
	}
	return spliceBegin(sym, "cond-expand", expanded), nil
}
