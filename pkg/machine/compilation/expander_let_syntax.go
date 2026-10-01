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

// expander_let_syntax.go implements let-syntax and letrec-syntax expansion.
//
// Both forms are fully resolved during the expansion phase: macro bindings
// are compiled, the body is expanded in a child environment, and the
// let-syntax/letrec-syntax wrapper disappears from the output.
//
// Extracted from expander_time_continuation.go.

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// expandLetSyntax fully expands let-syntax during the expansion phase.
// Creates local macro bindings, expands the body, and returns the expanded result.
// The let-syntax wrapper disappears - only the expanded body remains.
//
// R7RS §4.3.1: let-syntax establishes local macro definitions visible only in the body.
func (p *ExpanderTimeContinuation) expandLetSyntax(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.expandLetSyntaxImpl(sym, expr, false)
}

// expandLetrecSyntax fully expands letrec-syntax during the expansion phase.
// Like let-syntax but transformers can reference each other (mutual recursion).
//
// R7RS §4.3.1: letrec-syntax is like let-syntax but with mutual visibility.
func (p *ExpanderTimeContinuation) expandLetrecSyntax(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.expandLetSyntaxImpl(sym, expr, true)
}

// expandLetSyntaxImpl implements both let-syntax and letrec-syntax expansion.
// The recursive parameter controls whether bindings can see each other.
//
// This function:
// 1. Creates a child expand environment with local macro bindings
// 2. Compiles each syntax-rules transformer
// 3. Expands body expressions with the child environment
// 4. Wraps in lambda if body contains defines (for scope isolation)
// 5. Returns the expanded body - the let-syntax wrapper disappears
func (p *ExpanderTimeContinuation) expandLetSyntaxImpl(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue, recursive bool) (syntax.SyntaxValue, error) {
	formName := "let-syntax"
	if recursive {
		formName = "letrec-syntax"
	}
	sc := sym.SourceContext()

	// expr is (<bindings> <body>) - args after keyword
	argsPair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(argsPair) {
		return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s: expected bindings and body", formName))
	}

	// Get the bindings list
	bindingsStx := argsPair.SyntaxCar()
	bindingsEmpty := syntax.IsSyntaxEmptyList(bindingsStx)
	var bindingsPair *syntax.SyntaxPair
	if !bindingsEmpty {
		var pairOk bool
		bindingsPair, pairOk = bindingsStx.(*syntax.SyntaxPair)
		if !pairOk {
			return nil, wrapSourcedError(bindingsStx.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s: expected bindings list", formName))
		}
	}

	// Get the body
	bodyStx := argsPair.SyntaxCdr()
	bodyPair, ok := bodyStx.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(bodyPair) {
		return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s: expected body expressions", formName))
	}

	// Count bindings for local environment allocation
	numBindings := 0
	var current *syntax.SyntaxPair
	if !bindingsEmpty {
		current = bindingsPair
		for !syntax.IsSyntaxEmptyList(current) {
			numBindings++
			cdr := current.SyntaxCdr()
			nextPair, ok := cdr.(*syntax.SyntaxPair)
			if !ok {
				break
			}
			current = nextPair
		}
	}

	// Create child expand environment for macro bindings.
	// Use p.env directly as the parent (not p.env.Expand()) to preserve
	// the environment chain for nested let-syntax. When we have:
	//   (let-syntax ((outer ...))
	//     (let-syntax ((inner ...)) ...))
	// The inner let-syntax's environment must have outer's environment
	// in its parent chain, not the global expand environment.
	localExpandEnv := environment.NewLocalEnvironment(numBindings)
	childExpandEnv := environment.NewEnvironmentFrameWithParent(localExpandEnv, p.env)

	// The binding scope for the keywords and the body. A pattern literal shadowed
	// by one of the keywords is refused by resolving it to that keyword's local
	// binding (literalScopesMatchWithDef, internal/match/syntax_adapter.go), not by
	// the presence of this scope, which every body identifier carries.
	letScope := syntax.NewRebindingScopeWithLabel("let-syntax")

	// For letrec-syntax, pre-register all keywords so transformers can see each other
	if recursive && !bindingsEmpty {
		current = bindingsPair
		for !syntax.IsSyntaxEmptyList(current) {
			bindingStx := current.SyntaxCar()
			bindingPair, ok := bindingStx.(*syntax.SyntaxPair)
			if !ok || syntax.IsSyntaxEmptyList(bindingPair) {
				return nil, wrapSourcedError(bindingStx.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s: invalid binding", formName))
			}
			keywordStx := bindingPair.SyntaxCar()
			keywordSym, ok := keywordStx.(*syntax.SyntaxSymbol)
			if !ok {
				return nil, wrapSourcedError(bindingStx.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotASymbol, "%s: keyword must be a symbol", formName))
			}
			keyword := keywordSym.Unwrap().(*values.Symbol)
			// Bind on the keyword's accumulated scope set plus letScope, not the
			// bare singleton. A nested same-named binder then carries a strict
			// superset of its enclosing binder's scopes and wins resolution by
			// maximality, instead of tying it on cardinality and falling back to
			// No Clone: Scopes is immutable and persistent, so Add returns a new set
			// and cannot corrupt the symbol's hygiene state. The Clone existed
			// because Scopes() handed back a live backing slice; there is no
			// backing slice now, which retires the hazard rather than guarding it.
			keywordScopes := keywordSym.Scopes().Add(letScope)
			_, _ = childExpandEnv.MaybeCreateLocalBinding(keyword, environment.BindingTypeSyntax, keywordScopes, keywordSym.SourceContext())

			cdr := current.SyntaxCdr()
			nextPair, ok := cdr.(*syntax.SyntaxPair)
			if !ok {
				break
			}
			current = nextPair
		}
	}

	// Compile each transformer and store in child expand environment
	if !bindingsEmpty {
		current = bindingsPair
	}
	for !bindingsEmpty && !syntax.IsSyntaxEmptyList(current) {
		bindingStx := current.SyntaxCar()
		bindingPair, ok := bindingStx.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(bindingPair) {
			return nil, wrapSourcedError(bindingStx.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s: invalid binding", formName))
		}

		// Get keyword
		keywordStx := bindingPair.SyntaxCar()
		keywordSym, ok := keywordStx.(*syntax.SyntaxSymbol)
		if !ok {
			return nil, wrapSourcedError(bindingStx.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotASymbol, "%s: keyword must be a symbol", formName))
		}
		keyword := keywordSym.Unwrap().(*values.Symbol)

		// Get transformer expression
		transformerCdr := bindingPair.SyntaxCdr()
		transformerPair, ok := transformerCdr.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(transformerPair) {
			return nil, wrapSourcedError(bindingPair.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s: missing transformer expression", formName))
		}
		transformerExpr := transformerPair.SyntaxCar()

		// R7RS §4.3.1: for letrec-syntax the region of the bindings includes the
		// transformer specs themselves, so a transformer may name its siblings and
		// itself. That region is expressed two ways at once, and both are needed:
		// the keywords are pre-registered in childExpandEnv above, so compile the
		// spec against that frame rather than the outer one, and the spec carries
		// letScope, which the sibling binder also carries atop the keyword's
		// accumulated scopes, so a sibling reference is a superset of that binder
		// and resolves to it. Compiling against p.env with an unscoped spec left
		// free-identifier resolution unable to see the very bindings the
		// pre-registration existed to expose.
		//
		// let-syntax keeps the outer environment and the unscoped spec: its
		// bindings' region is the body only.
		transformerEnv := p.env
		if recursive {
			transformerEnv = childExpandEnv
			transformerExpr = syntax.AddScopeToSyntax(transformerExpr, letScope)
		}

		// Evaluate the transformer expression one phase up (design §2.2). For
		// letrec-syntax the spec carries letScope and compiles against
		// childExpandEnv, so a sibling reference in a template resolves to the
		// pre-registered binder by subset; that resolution now happens at
		// expansion time, not in a definition-site producer arm.
		transformer, err := compileTransformerValue(p.ctx, transformerEnv, transformerExpr, p.libraryScope, p.evaluator)
		if err != nil {
			return nil, wrapSourcedError(transformerExpr.SourceContext(), werr.WrapForeignErrorf(err, "%s: could not compile transformer for %s", formName, keyword.Key))
		}

		// Store on the keyword's accumulated scope set plus letScope (see the
		// pre-registration above for why the bare singleton is wrong). The
		// pre-register and this re-resolve must use the identical set, or
		// MaybeCreateLocalBinding keys a second slot and the transformer lands in
		// the wrong binding.
		keywordScopes := keywordSym.Scopes().Add(letScope)
		localIndex, created := childExpandEnv.MaybeCreateLocalBinding(keyword, environment.BindingTypeSyntax, keywordScopes, keywordSym.SourceContext())
		if !created {
			// letrec-syntax pre-registered this keyword above, under the same set.
			// Re-resolve under that set, not nil: nil is MATCH ANY, which takes the
			// first live slot of the name rather than the one just addressed.
			localIndex = childExpandEnv.GetLocalIndex(keyword, syntax.ScopesOf(keywordScopes))
		}
		if localIndex == nil {
			return nil, wrapSourcedError(keywordSym.SourceContext(), werr.WrapForeignErrorf(
				werr.ErrInvalidSyntax,
				"%s: failed to create binding for %s", formName, keyword.Key,
			))
		}
		err = childExpandEnv.SetLocalValue(localIndex, transformer)
		if err != nil {
			return nil, wrapSourcedError(keywordSym.SourceContext(), werr.WrapForeignErrorf(err, "%s: failed to store transformer for %s", formName, keyword.Key))
		}

		cdr := current.SyntaxCdr()
		nextPair, ok := cdr.(*syntax.SyntaxPair)
		if !ok {
			break
		}
		current = nextPair
	}

	// Add the let-syntax scope to the body
	scopedBody := bodyPair.AddScope(letScope)
	scopedBodyPair, ok := scopedBody.(*syntax.SyntaxPair)
	if !ok {
		return nil, wrapSourcedError(bodyPair.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotAList, "%s: body must be a list", formName))
	}

	// Create expander with child expand environment for body expansion
	childExpander := p.newChildExpander(childExpandEnv)

	// Expand all body expressions and check for defines
	var expandedExprs []syntax.SyntaxValue
	hasDefine := false
	current = scopedBodyPair
	for !syntax.IsSyntaxEmptyList(current) {
		expr := current.SyntaxCar()
		expandedExpr, err := childExpander.ExpandExpression(expr)
		if err != nil {
			return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(err, "%s: failed to expand body expression", formName))
		}
		expandedExprs = append(expandedExprs, expandedExpr)
		if isSyntaxFormWithKeyword(expandedExpr, "define") {
			hasDefine = true
		}
		cdr := current.SyntaxCdr()
		nextPair, ok := cdr.(*syntax.SyntaxPair)
		if ok { //nolint:gocritic // ifElseChain: type assertion + value check, not a switch candidate
			current = nextPair
		} else if !syntax.IsSyntaxEmptyList(cdr) {
			return nil, wrapSourcedError(current.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotAList, "%s: body must be a proper list", formName))
		} else {
			break
		}
	}

	// Build result: (begin body...) or ((lambda () (begin body...))) if has defines
	beginSym := syntax.NewSyntaxSymbol("begin", sc)
	beginBody := syntax.SyntaxList(sc, expandedExprs...)
	beginExpr := syntax.NewSyntaxCons(beginSym, beginBody, sc)

	if hasDefine {
		// Wrap in lambda to create new runtime scope for defines
		lambdaSym := syntax.NewSyntaxSymbol("lambda", sc)
		emptyArgs := syntax.SyntaxEmptyList
		lambdaExpr := syntax.SyntaxList(sc, lambdaSym, emptyArgs, beginExpr)
		return syntax.SyntaxList(sc, lambdaExpr), nil
	}

	return beginExpr, nil
}

// asSyntaxFormWithKeyword returns expr as a *SyntaxPair if it is a non-empty
// pair whose car is a syntax symbol matching keyword. Returns (nil, false)
// otherwise. Use this when the caller needs the pair to walk its cdr;
// use isSyntaxFormWithKeyword when only the boolean test is needed.
func asSyntaxFormWithKeyword(expr syntax.SyntaxValue, keyword string) (*syntax.SyntaxPair, bool) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return nil, false
	}
	sym, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
	if !ok {
		return nil, false
	}
	if sym.Key() != keyword {
		return nil, false
	}
	return pair, true
}

// isSyntaxFormWithKeyword reports whether expr is a non-empty syntax pair
// whose car is a syntax symbol matching keyword.
func isSyntaxFormWithKeyword(expr syntax.SyntaxValue, keyword string) bool {
	_, ok := asSyntaxFormWithKeyword(expr, keyword)
	return ok
}

// asFormDenoting reports whether expr is a non-empty form whose head denotes the
// special form keyword, and returns the pair. It is asSyntaxFormWithKeyword with
// the head resolved through its binding, so a renamed or prefixed import of the
// keyword is recognized; see headFormName for the fallback.
func asFormDenoting(env *environment.EnvironmentFrame, expr syntax.SyntaxValue, keyword string) (*syntax.SyntaxPair, bool) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return nil, false
	}
	sym, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
	if !ok {
		return nil, false
	}
	if headFormName(env, sym) != keyword {
		return nil, false
	}
	return pair, true
}

// headFormName returns the special form a head identifier denotes. A head that
// resolves to no keyword binding answers with its spelling, which keeps every
// pre-existing answer for unbound heads and variables unchanged; only a keyword
// bound under another name answers differently.
//
// An incomparable scope-set tie answers with the spelling too, through
// TryGetBinding rather than GetBinding. This function is asked what a head
// LOOKS LIKE before anything commits to compiling it, so a tie must not raise
// here; the reference's own resolution will raise later if the program really
// does reference it. validate.markerName is the same function on the other side
// of the validate → machine edge and resolves the same way; quasiHeadDepth's
// doc states the agreement as a requirement.
func headFormName(env *environment.EnvironmentFrame, sym *syntax.SyntaxSymbol) string {
	symVal, ok := sym.Unwrap().(*values.Symbol)
	if !ok {
		return ""
	}
	if env == nil {
		return symVal.Key
	}
	bnd, _ := env.TryGetBinding(symVal, syntax.ScopesOf(sym.Scopes()))
	denoted := environment.DenotedForm(bnd)
	if denoted != "" {
		return denoted
	}
	return symVal.Key
}
