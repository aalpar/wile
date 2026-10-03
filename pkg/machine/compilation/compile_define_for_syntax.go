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
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// CompileDefineForSyntax handles (define-for-syntax name expr) or
// (define-for-syntax (name args...) body...).
//
// This form defines a binding in the expand phase environment that is
// available during macro expansion. The expression is compiled and
// evaluated during expansion, and the result is stored one phase up from the
// defining frame (env.NextPhase(); equals env.Expand() at phase 0).
//
// Unlike define-syntax (which stores macro transformers), define-for-syntax
// stores regular values with BindingTypeVariable.
//
// The definition has ALREADY TAKEN EFFECT: expandDefineForSyntax ran it when the
// expander reached the form, so a define-syntax later in the same unit can use it.
// So this only re-checks the shape and emits nothing. Running it here as well
// would re-bind the name to a freshly evaluated value after transformers had
// already used the first one.
func (p *CompileTimeContinuation) CompileDefineForSyntax(_ CompileTimeCallContext, expr syntax.SyntaxValue) error {
	_, _, err := p.defineForSyntaxParts(expr)
	return err
}

// defineForSyntaxParts returns the name a define-for-syntax form binds and the
// expression whose value it binds, building the lambda for the procedure form.
//
// The name comes back as the *syntax.SyntaxSymbol, not its unwrapped
// *values.Symbol, because the binder needs the symbol's SCOPES and not only its
// spelling: a macro-introduced name and a user-written one of the same spelling
// are the same *values.Symbol and differ only there. Unwrapping here is what made
// runDefineForSyntax bind under the empty set, which is I140.
func (p *CompileTimeContinuation) defineForSyntaxParts(expr syntax.SyntaxValue) (*syntax.SyntaxSymbol, syntax.SyntaxValue, error) {
	err := p.ensureState("define-for-syntax")
	if err != nil {
		return nil, nil, err
	}

	// expr is (name expr) or ((name args...) body...) - the args after 'define-for-syntax'
	argsPair, err := formArgs(expr, "define-for-syntax", "name and expression")
	if err != nil {
		return nil, nil, err
	}

	// Get the first element - either a symbol (simple define) or a pair (function define)
	first := argsPair.SyntaxCar()
	if first == nil {
		return nil, nil, p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrUnexpectedNil, "define-for-syntax: missing name"))
	}

	// Get the rest (value expression or body)
	restVal := argsPair.SyntaxCdr()
	restPair, ok := restVal.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(restPair) {
		return nil, nil, p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotASyntaxPair, "define-for-syntax: missing expression"))
	}

	// Check if it's a function definition: (define-for-syntax (name args...) body...)
	firstPair, ok := first.(*syntax.SyntaxPair)
	if ok {
		// Function shorthand - extract name and build lambda
		nameStx := firstPair.SyntaxCar()
		nameSyntaxSym, ok := nameStx.(*syntax.SyntaxSymbol)
		if !ok {
			return nil, nil, p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "define-for-syntax: function name must be a symbol"))
		}

		// Build (lambda (args...) body...)
		params := firstPair.SyntaxCdr()
		lambdaSym := syntax.NewSyntaxSymbol("lambda", nameSyntaxSym.SourceContext())
		lambdaArgs := syntax.NewSyntaxCons(params, restPair, nameSyntaxSym.SourceContext())
		return nameSyntaxSym, syntax.NewSyntaxCons(lambdaSym, lambdaArgs, nameSyntaxSym.SourceContext()), nil
	}

	// Simple definition: (define-for-syntax name expr)
	nameSyntaxSym, ok := first.(*syntax.SyntaxSymbol)
	if !ok {
		return nil, nil, p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "define-for-syntax: name must be a symbol"))
	}
	return nameSyntaxSym, restPair.SyntaxCar(), nil
}

// runDefineForSyntax evaluates a define-for-syntax form's expression one phase up
// and binds the result there. Called by the expander; see expandDefineForSyntax.
func (p *CompileTimeContinuation) runDefineForSyntax(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	nameStx, valueExpr, err := p.defineForSyntaxParts(expr)
	if err != nil {
		return err
	}
	nameSym := nameStx.Unwrap().(*values.Symbol)

	// Expand, compile, and execute the expression one phase up from the defining
	// frame (relative, not the absolute expand phase) so a define-for-syntax inside
	// a transformer body climbs symmetrically with begin-for-syntax and the
	// define-syntax storage/lookup. At phaseLevel 0 NextPhase() == Expand()
	// (level-0 identity).
	//
	// The expander roots at expandEnv for the same reason executeFormsAtCompileTime
	// does: the value expression is phase-(N+1) code, so the keywords it may use are
	// the ones a phase-(N+1) define-syntax deposits, at N+2.
	expandEnv := p.env.NextPhase()
	expander := NewExpanderTimeContinuation(ctctx.ctx, expandEnv, p.evaluator)
	result, err := p.expandCompileExecute(ctctx.ctx, ctctx, valueExpr, expandEnv, expander, "define-for-syntax")
	if err != nil {
		return err
	}

	// Store the result in the expand phase environment with BindingTypeVariable.
	// Create-then-write through the create's own PIN, never a hand-built index: a
	// bare-symbol one resolves wildcard, so over the merged store it would land on
	// the name's first live slot at ANY coordinates — for a name the registry also
	// installs at phase 0 (car), the SEALED one.
	//
	// NOT DefineOwnGlobal, and the difference is the point. That helper now REFUSES
	// a rebind of a Stable binding at matching coordinates, which is what closes the
	// three reflective doors around the compiler's own immutability gate
	// (Engine.Define, Engine.RegisterPrimitive, namespace-define!). This site is
	// INSIDE the compiler, and a phase-1 define is R7RS-legal here whatever the name.
	// The one Stable-stamped phase-1 population is the registry's expand-phase
	// copies, and those live at (1, sealed) (registry.Apply's phaseTargets), so a
	// USER create for the same name lands on a fresh (1, mutable) slot and SHADOWS
	// the copy — the phase-1 twin of a phase-0 define over a sealed primitive.
	// Pinned by TestBindingModelMatrix's three M7 rows.
	//
	// A bootstrap source is the case that is NOT at (1, mutable): it compiles with
	// p.env == the owner's phase-0 seal, so NextPhase() is the sealed expand view
	// and this create shares the registry copies' coordinate. Only there can the
	// helper's Stable refusal fire; see createPhaseBindingUnlessStable.
	// The created flag is the two define-syntax sites' concern (they predeclare
	// before compiling a right-hand side that may fail); this site evaluates
	// first and stores second, so it has nothing to roll back.
	//
	// THE SCOPES ARE THE SYMBOL'S, NOT THE EMPTY SET, and the empty set was I140.
	// A binder at {} is reachable from every scope set, so a name a macro
	// TEMPLATE introduced was visible to any later transformer that happened to
	// spell it the same way — the phase-1 face of the rule in CLAUDE.local.md's
	// "Binding Identity": two identifiers with one spelling are not one binding.
	// Racket and Chez both refuse that reference.
	//
	// This alone is a REGRESSION, measured, which is why it does not ship alone. A
	// macro-SUPPLIED name — one arriving through a pattern variable, so the user
	// wrote it — carries the use-site scope the invocation stamped, and a later
	// reference outside the expansion does not, so it stops resolving. The
	// use-site pruner is what strips that scope back off, and it only prunes forms
	// on its own list: pruneDefinitionBinder (expander_use_site.go) therefore
	// gained define-for-syntax in the same change. Breaking either half reddens
	// TestForSyntaxBinderScopes in the opposite direction from the other.
	gi, _, err := createPhaseBindingUnlessStable(expandEnv, nameSym, environment.BindingTypeVariable, nameStx.Scopes(), "define-for-syntax")
	if err != nil {
		return p.wrapCompilationError(err)
	}
	err = expandEnv.GlobalEnvironment().SetOwnGlobalValue(gi, result)
	if err != nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "define-for-syntax: failed to store value for %s", nameSym.Key))
	}

	return nil
}
