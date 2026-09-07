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
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// CompileQuoteSyntax compiles (quote-syntax template). The template is a constant
// carrying the scopes it was read with, and every identifier in it that resolves
// to a global at this frame is stamped with its definition-site pin (design §2.1
// item 1), and inside a define-library body every identifier, resolved or not, is
// stamped with the library scope (item 2). The pin is the same probe at the same
// moment as the Go template producer's
// (collectFreeIdentifiersWithEllipsis): GetGlobalIndexAcrossPhases under the
// identifier's own scopes, at transformer compile time.
//
// The producer's third emission, replacing a definition-site LOCAL's scopes onto
// the identifier, is not reproduced (item 3): a phase view has no lexical
// locals, and it is not needed — the identifier keeps its read scopes, which
// include every binder scope enclosing the template, so subset resolution finds
// a definition-site local without copying, and a same-named use-site binder
// carries a scope the identifier lacks. Pinned by
// TestP03_QuoteSyntaxDefSiteLocalUnderUseSiteLet and the Task 1 guards.
//
// Where a local of the name DOES resolve at this frame (a quote-syntax written
// under a let at phase 0), no global pin is stamped: the compiler's local-before-
// pin order would absorb one, but it would be a wrong pin, and the producer did
// not stamp one either.
//
// The library scope is stamped on every identifier, resolved or not — broader
// than the producer, which stamps it only inside `if globalBinding != nil`
// (syntax_expand.go, the globalBinding arm) and adds none on its nil-pin tail.
// It is not the route into the library env for a template read inside a library
// body: processFormsWithLetrecSemantics stamps every body form with the library
// scope before expansion (compile_time_continuation_include.go, reached for
// begin bodies through compileLibraryBegin), and SyntaxSymbol.AddScope returns
// the receiver once the scope is present (via SourceContext.WithScope's early
// return). Nor is a nil pin the motivation: a helper defined later in the body is
// predeclared before any define-syntax compiles (expander_body.go), so its pin
// is non-nil. What the stamp covers is an identifier introduced into a library
// body by a macro defined outside the library, never read with the body's
// scopes. Three consumers read the library scope off a template identifier:
// lookupMacroBinding's arm 3, GetGlobalIndexFromLibraryScopes
// (compile_time_continuation.go) and referenceReachesBinderDirectly
// (validate.go).
func (p *CompileTimeContinuation) CompileQuoteSyntax(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	template, err := formSingleArg(expr, "quote-syntax")
	if err != nil {
		return p.wrapCompilationError(err)
	}
	var stamp func(node syntax.SyntaxValue) syntax.SyntaxValue
	stamp = func(node syntax.SyntaxValue) syntax.SyntaxValue {
		// MapSyntaxTree recurses into pairs and vectors only; its default arm
		// hands a box to fn whole. Descend, so a #&… template stamps like the
		// producer's, which recurses (collectFreeIdentifiersWithEllipsis).
		box, ok := node.(*syntax.SyntaxBox)
		if ok {
			inner := syntax.MapSyntaxTree(box.Value, stamp)
			if inner == box.Value {
				return box
			}
			return syntax.NewSyntaxBox(inner, box.SourceContext())
		}
		sym, ok := node.(*syntax.SyntaxSymbol)
		if !ok {
			return node
		}
		return p.stampQuoteSyntaxIdentifier(sym)
	}
	stamped := syntax.MapSyntaxTree(template, stamp)
	litIdx := p.template.MaybeAppendLiteral(stamped)
	p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
	return nil
}

// stampQuoteSyntaxIdentifier applies the library scope and the definition-site
// pin to one template identifier; see CompileQuoteSyntax.
func (p *CompileTimeContinuation) stampQuoteSyntaxIdentifier(sym *syntax.SyntaxSymbol) syntax.SyntaxValue {
	symVal, ok := sym.Unwrap().(*values.Symbol)
	if !ok {
		return sym
	}
	scopes := sym.Scopes()
	q := sym
	if p.libraryScope != nil {
		q = q.AddScope(p.libraryScope).(*syntax.SyntaxSymbol)
	}
	if p.env.GetLocalIndex(symVal, syntax.ScopesOf(scopes)) != nil {
		return q
	}
	gi := p.env.GetGlobalIndexAcrossPhases(symVal, scopes)
	if gi == nil {
		return q
	}
	return q.WithResolvedBinding(gi)
}
