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

// compile_closure.go contains closure compilation infrastructure: parameter
// binding, body compilation with letrec* semantics, and rest-parameter handling.
// These are shared by CompileValidatedLambda, CompileValidatedCaseLambda, and
// CompileValidatedBegin in compile_validated.go.

import (
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/internal/validate"
	"github.com/aalpar/wile/werr"
)

// setScopesOnLastBinding attaches hygiene scopes to the most recently created local binding.
//
// This function is called after EnsureLocalBinding to preserve macro hygiene information.
// EnsureLocalBinding doesn't accept scopes as a parameter, so scopes must be attached
// separately. The scopes track which macro expansion introduced the binding, enabling
// hygienic macro expansion per R6RS/R7RS.
//
// Usage: Called after each parameter binding is created in compileClosure and bindRestParameter.
//
// Example flow:
//
//	lenv.EnsureLocalBinding(param, BindingTypeVariable)  // creates binding without scopes
//	setScopesOnLastBinding(paramScopes, lenv)            // attaches scopes to that binding
//
// If scopes is nil or empty, this is a no-op (the binding came from source code, not a macro).
func setScopesOnLastBinding(scopes []*syntax.Scope, lenv *environment.LocalEnvironmentFrame) {
	if len(scopes) == 0 {
		return
	}
	bindings := lenv.Bindings()
	if len(bindings) == 0 {
		return
	}
	bindings[len(bindings)-1].SetScopes(scopes)
}

// compileClosureBody binds parameters, compiles the body, optimizes, and registers
// the template and environment as literals. Returns the literal indices so the
// caller can emit the appropriate closure opcodes.
//
// Used by compileClosure (lambda, define-fn) and CompileValidatedCaseLambda.
func (p *CompileTimeContinuation) compileClosureBody(
	ctctx CompileTimeCallContext,
	tpl *NativeTemplate,
	lenv *environment.LocalEnvironmentFrame,
	v validate.ValidatedBodyAndParams,
	errContext string,
) (LiteralIndex, LiteralIndex, error) {
	// Phase 1: Bind required parameters to the local environment.
	// Each parameter becomes a local variable slot populated by the VM at call time.
	// Params() is nil for zero-arg case-lambda clauses: (() ...).
	if v.Params() != nil {
		for _, paramSym := range v.Params().Required {
			param := paramSym.Sym
			paramScopes := paramSym.Scopes()

			_, ok := lenv.EnsureLocalBinding(param, environment.BindingTypeVariable)
			if !ok {
				return 0, 0, werr.WrapForeignErrorf(
					werr.ErrDuplicateBinding,
					"duplicate parameter %q in %s", param.Key, errContext,
				)
			}

			// Preserve hygiene scopes from the source — tracks which macro
			// expansion introduced this binding (R7RS sets-of-scopes model).
			setScopesOnLastBinding(paramScopes, lenv)
			tpl.parameterCount++
		}

		// Phase 2: Bind the rest parameter (if any) for variadic functions.
		// For (lambda (a b . rest) ...), binds 'rest' to receive excess args as a list.
		err := bindRestParameter(v, p, lenv, tpl)
		if err != nil {
			return 0, 0, err
		}
	}

	// Phase 3: Create child environment and register literals.
	// lenv must be fully populated before NewEnvironmentFrameWithParent (embeds by value).
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, p.env)
	tpli := p.template.MaybeAppendLiteral(tpl)
	envi := p.template.MaybeAppendLiteral(childEnv)

	// Phase 4: Compile body expressions into child template. The last expression
	// is compiled in tail position for proper tail-call optimization.
	err := p.compileBody(ctctx, v, childEnv, tpl)
	if err != nil {
		return 0, 0, err
	}

	// Phase 5: Peephole optimization.
	tpl.Optimize()

	return tpli, envi, nil
}

// compileClosure compiles a complete closure (lambda or define-fn body).
// Binds parameters, compiles body, and emits MakeClosure operations.
func (p *CompileTimeContinuation) compileClosure(ctctx CompileTimeCallContext, tpl *NativeTemplate, lenv *environment.LocalEnvironmentFrame, v validate.ValidatedProcedure) error {
	tpli, envi, err := p.compileClosureBody(ctctx, tpl, lenv, v, "lambda")
	if err != nil {
		return err
	}

	p.AppendOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(tpli),
		NewOperationPush(),
		NewOperationLoadLiteralByLiteralIndexImmediate(envi),
		NewOperationPush(),
		NewOperationMakeClosure(),
	)

	return nil
}

// compileBody compiles a sequence of body expressions for a lambda or case-lambda clause.
// The last expression is compiled in tail position per R7RS 3.5. Appends RestoreContinuation
// at the end to return from the closure.
//
// R7RS §5.3.2: Internal definitions use letrec* semantics - all defined names are visible
// throughout the body, enabling forward references between defines.
func (p *CompileTimeContinuation) compileBody(ctctx CompileTimeCallContext, clause validate.ValidatedBodyAndParams, childEnv *environment.EnvironmentFrame, tpl *NativeTemplate) error {
	childCompiler := NewCompiletimeContinuation(tpl, childEnv)
	lambdaBodyContext := NewCompileTimeCallContext(ctctx.ctx, true)

	body := clause.Body()

	// R7RS §5.3.2: Internal definitions use letrec* semantics
	// Pass 1: Pre-declare all define bindings so forward references work
	for _, bodyExpr := range body {
		childCompiler.predeclareDefineBindingFromValidated(bodyExpr)
	}

	// Docstring: the validator has already extracted any leading string
	// literal and stripped it from the body. Just read the field.
	doc := clause.Docstring()
	if doc != "" {
		tpl.SetDoc(doc)
	}

	// Pass 2: Compile all expressions (with all bindings now visible)
	err := childCompiler.compileValidatedSequence(lambdaBodyContext, body)
	if err != nil {
		return err
	}

	childCompiler.AppendOperations(NewOperationRestoreContinuation())
	return nil
}

// predeclareDefineBindingFromValidated pre-creates a binding for a validated define form.
// This enables forward references within lambda bodies per R7RS §5.3.2.
// See letrec_semantics.go for the shared pattern documentation.
func (p *CompileTimeContinuation) predeclareDefineBindingFromValidated(expr validate.ValidatedExpr) {
	def, ok := expr.(*validate.ValidatedDefine)
	if !ok {
		return
	}
	sym := def.Name().Sym
	predeclareBinding(p.env, sym, def.Name().Scopes(), def.Name().SourceContext())
}

// bindRestParameter binds the rest parameter (if any) to the local environment.
// For forms like (lambda (a b . rest) ...), this binds 'rest' and marks the template as variadic.
//
// In Scheme, the dotted tail notation indicates a variadic function:
//   - (lambda (a b . rest) ...) takes 2+ args; excess args become a list bound to 'rest'
//   - (lambda args ...) takes 0+ args; all args become a list bound to 'args'
//
// At runtime, the VM collects excess arguments into a list and stores it in the
// rest parameter's local slot, after storing the required arguments in their slots.
func bindRestParameter(v validate.ValidatedBodyAndParams, _ *CompileTimeContinuation, lenv *environment.LocalEnvironmentFrame, tpl *NativeTemplate) error {
	// Early exit if no rest parameter. Most lambdas are fixed-arity.
	if v.Params().Rest == nil {
		return nil
	}

	rest := v.Params().Rest.Sym
	restScopes := v.Params().Rest.Scopes()

	// Create a local binding slot for the rest parameter. This slot comes after
	// all required parameter slots. The VM knows to populate it with a list of
	// excess arguments because tpl.isVariadic is set below.
	_, ok := lenv.EnsureLocalBinding(rest, environment.BindingTypeVariable)
	if !ok {
		// Rest parameter name conflicts with a required parameter (e.g., (lambda (x . x) ...))
		return werr.WrapForeignErrorf(werr.ErrDuplicateBinding, "duplicate rest parameter %q in lambda", rest.Key)
	}

	// Preserve hygiene scopes for the rest parameter, same as required parameters.
	// This ensures macro-introduced rest parameters maintain proper lexical identity.
	setScopesOnLastBinding(restScopes, lenv)

	// The rest parameter counts toward the total parameter count. For (lambda (a b . rest) ...),
	// parameterCount becomes 3 (a, b, rest). The VM uses parameterCount together with
	// isVariadic to determine: minimum required args = parameterCount - 1 when variadic.
	tpl.parameterCount++

	// Mark the template as variadic. This flag tells the VM to:
	//   1. Accept any number of arguments >= (parameterCount - 1)
	//   2. Collect excess arguments into a list
	//   3. Store that list in the last local slot (the rest parameter)
	tpl.isVariadic = true

	return nil
}
