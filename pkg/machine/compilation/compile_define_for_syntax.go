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
// evaluated at compile time, and the result is stored one phase up from the
// defining frame (env.NextPhase(); equals env.Expand() at phase 0).
//
// Unlike define-syntax (which stores macro transformers), define-for-syntax
// stores regular values with BindingTypeVariable.
func (p *CompileTimeContinuation) CompileDefineForSyntax(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	err := p.ensureState("define-for-syntax")
	if err != nil {
		return err
	}

	// expr is (name expr) or ((name args...) body...) - the args after 'define-for-syntax'
	argsPair, err := formArgs(expr, "define-for-syntax", "name and expression")
	if err != nil {
		return err
	}

	// Get the first element - either a symbol (simple define) or a pair (function define)
	first := argsPair.SyntaxCar()
	if first == nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrUnexpectedNil, "define-for-syntax: missing name"))
	}

	// Get the rest (value expression or body)
	restVal := argsPair.SyntaxCdr()
	restPair, ok := restVal.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(restPair) {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotASyntaxPair, "define-for-syntax: missing expression"))
	}

	var nameSym *values.Symbol
	var valueExpr syntax.SyntaxValue

	// Check if it's a function definition: (define-for-syntax (name args...) body...)
	firstPair, ok := first.(*syntax.SyntaxPair)
	if ok {
		// Function shorthand - extract name and build lambda
		nameStx := firstPair.SyntaxCar()
		nameSyntaxSym, ok := nameStx.(*syntax.SyntaxSymbol)
		if !ok {
			return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "define-for-syntax: function name must be a symbol"))
		}
		nameSym = nameSyntaxSym.Unwrap().(*values.Symbol)

		// Build (lambda (args...) body...)
		params := firstPair.SyntaxCdr()
		lambdaSym := syntax.NewSyntaxSymbol("lambda", nameSyntaxSym.SourceContext())
		lambdaArgs := syntax.NewSyntaxCons(params, restPair, nameSyntaxSym.SourceContext())
		valueExpr = syntax.NewSyntaxCons(lambdaSym, lambdaArgs, nameSyntaxSym.SourceContext())
	} else {
		// Simple definition: (define-for-syntax name expr)
		nameSyntaxSym, ok := first.(*syntax.SyntaxSymbol)
		if !ok {
			return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "define-for-syntax: name must be a symbol"))
		}
		nameSym = nameSyntaxSym.Unwrap().(*values.Symbol)

		// Get the value expression
		valueExpr = restPair.SyntaxCar()
	}

	// Expand, compile, and execute the expression one phase up from the defining
	// frame (relative, not the absolute expand phase) so a define-for-syntax inside
	// a transformer body climbs symmetrically with begin-for-syntax and the
	// define-syntax storage/lookup. The expander stays rooted at p.env because its
	// macro lookup already applies NextPhase(), so expander.env.NextPhase() ==
	// expandEnv. At phaseLevel 0 NextPhase() == Expand() (level-0 identity).
	expandEnv := p.env.NextPhase()
	expander := NewExpanderTimeContinuation(ctctx.ctx, p.env, p.evaluator)
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
	gi, err := createPhaseBindingUnlessStable(expandEnv, nameSym, environment.BindingTypeVariable, nil, "define-for-syntax")
	if err != nil {
		return p.wrapCompilationError(err)
	}
	err = expandEnv.GlobalEnvironment().SetOwnGlobalValue(gi, result)
	if err != nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "define-for-syntax: failed to store value for %s", nameSym.Key))
	}

	// define-for-syntax has no runtime effect - don't emit any operations
	return nil
}
