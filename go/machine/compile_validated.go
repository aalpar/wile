// Copyright 2025 Aaron Alpar
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
	"wile/environment"
	"wile/syntax"
	"wile/validate"
	"wile/values"
)

// compileValidated dispatches compilation based on the validated expression type.
// Each ValidatedExpr type has a corresponding compile method that can assume
// the expression structure has already been validated.
func (p *CompileTimeContinuation) compileValidated(ccnt CompileTimeCallContext, expr validate.ValidatedExpr) error {
	switch v := expr.(type) {
	case *validate.ValidatedIf:
		return p.compileValidatedIf(ccnt, v)
	case *validate.ValidatedDefine:
		return p.compileValidatedDefine(ccnt, v)
	case *validate.ValidatedLambda:
		return p.compileValidatedLambda(ccnt, v)
	case *validate.ValidatedCaseLambda:
		return p.compileValidatedCaseLambda(ccnt, v)
	case *validate.ValidatedSetBang:
		return p.compileValidatedSetBang(ccnt, v)
	case *validate.ValidatedQuote:
		return p.compileValidatedQuote(ccnt, v)
	case *validate.ValidatedQuasiquote:
		return p.compileValidatedQuasiquote(ccnt, v)
	case *validate.ValidatedBegin:
		return p.compileValidatedBegin(ccnt, v)
	case *validate.ValidatedCall:
		return p.compileValidatedCall(ccnt, v)
	case *validate.ValidatedSymbol:
		startPC := len(p.template.operations)
		err := p.CompileSymbol(ccnt, v.Symbol)
		if err != nil {
			return err
		}
		p.recordSource(startPC, v.Source())
		return nil
	case *validate.ValidatedLiteral:
		// ValidatedLiteral wraps self-evaluating values AND special forms
		// that pass through validation (like define-syntax, define-library, etc.)
		return p.compileValidatedLiteral(ccnt, v)
	default:
		return values.NewForeignError("unknown validated expression type")
	}
}

// compileValidatedIf compiles a validated (if test conseq [alt]) form.
// The structure is guaranteed to be valid by the validator.
func (p *CompileTimeContinuation) compileValidatedIf(ccnt CompileTimeCallContext, v *validate.ValidatedIf) error {
	startPC := len(p.template.operations)

	// Compile the test condition (not in tail position)
	err := p.compileValidated(ccnt.NotInTail(), v.Test)
	if err != nil {
		return err
	}
	p.template.AppendOperations(NewOperationPush())

	// Set up branch-on-false to skip consequent
	branchOnFalseIndex := p.template.operations.Length()
	p.template.AppendOperations(NewOperationBranchOffsetImmediate(0)) // placeholder

	// Compile consequent (inherits tail position)
	err = p.compileValidated(ccnt, v.Conseq)
	if err != nil {
		return err
	}

	// Set up unconditional branch to skip alternative
	branchToEndIndex := p.template.operations.Length()
	p.template.AppendOperations(NewOperationBranchOffsetImmediate(0)) // placeholder

	// Target for branch-on-false
	altStart := p.template.operations.Length()

	// Compile alternative (or load void if none)
	if v.Alt != nil {
		err = p.compileValidated(ccnt, v.Alt)
		if err != nil {
			return err
		}
	} else {
		// No alternative - return void
		voidIdx := p.template.MaybeAppendLiteral(values.Void)
		p.template.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(voidIdx))
	}

	// Fix up branch targets
	endIndex := p.template.operations.Length()
	p.template.operations[branchOnFalseIndex] = NewOperationBranchOnFalseOffsetImmediate(altStart - branchOnFalseIndex)
	p.template.operations[branchToEndIndex] = NewOperationBranchOffsetImmediate(endIndex - branchToEndIndex)

	p.recordSource(startPC, v.Source())
	return nil
}

// compileValidatedDefine compiles a validated define form.
func (p *CompileTimeContinuation) compileValidatedDefine(ccnt CompileTimeCallContext, v *validate.ValidatedDefine) error {
	if v.IsFunction {
		// (define (name params...) body...) - compile as lambda then define
		return p.compileValidatedDefineFn(ccnt, v)
	}
	// (define name expr) - compile value then store
	return p.compileValidatedDefineVar(ccnt, v)
}

func (p *CompileTimeContinuation) compileValidatedDefineVar(ccnt CompileTimeCallContext, v *validate.ValidatedDefine) error {
	startPC := len(p.template.operations)

	// Get the symbol for the name
	sym, ok := v.Name.Unwrap().(*values.Symbol)
	if !ok {
		return values.ErrNotASymbol
	}
	sym = p.env.InternSymbol(sym)
	symbolScopes := v.Name.Scopes()

	// Create binding early for recursion support
	if p.env.LocalEnvironment() != nil {
		_, _ = p.env.MaybeCreateLocalBindingWithScopes(sym, environment.BindingTypeVariable, symbolScopes)
	} else {
		gi, created := p.env.CreateGlobalBinding(sym, environment.BindingTypeVariable)
		if created && symbolScopes != nil {
			binding := p.env.GetGlobalBinding(gi)
			if binding != nil {
				binding.SetScopes(symbolScopes)
			}
		}
	}

	// Compile the value expression
	err := p.compileValidated(ccnt.NotInTail(), v.Value)
	if err != nil {
		return err
	}

	// Store the value
	p.template.AppendOperations(NewOperationPush())
	if p.env.LocalEnvironment() != nil {
		li, _ := p.env.CreateLocalBinding(sym, environment.BindingTypeVariable)
		p.template.AppendOperations(NewOperationStoreLocalByLocalIndexImmediate(li))
	} else {
		gi, _ := p.env.CreateGlobalBinding(sym, environment.BindingTypeVariable)
		liti := p.template.MaybeAppendLiteral(gi)
		p.template.AppendOperations(NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(liti))
	}

	// define returns void
	p.template.AppendOperations(NewOperationLoadVoid())

	p.recordSource(startPC, v.Source())
	return nil
}

func (p *CompileTimeContinuation) compileValidatedDefineFn(ccnt CompileTimeCallContext, v *validate.ValidatedDefine) error {
	startPC := len(p.template.operations)

	// Get the symbol for the name
	sym, ok := v.Name.Unwrap().(*values.Symbol)
	if !ok {
		return values.ErrNotASymbol
	}
	sym = p.env.InternSymbol(sym)
	symbolScopes := v.Name.Scopes()

	// Declare the function name first for recursion support
	if p.env.LocalEnvironment() != nil {
		_, _ = p.env.MaybeCreateLocalBindingWithScopes(sym, environment.BindingTypeVariable, symbolScopes)
	} else {
		gi, created := p.env.CreateGlobalBinding(sym, environment.BindingTypeVariable)
		if created && symbolScopes != nil {
			binding := p.env.GetGlobalBinding(gi)
			if binding != nil {
				binding.SetScopes(symbolScopes)
			}
		}
	}

	// Create child environment and template for lambda body
	lenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, p.env)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.SetName(sym.Key) // Set function name for stack traces

	// Add parameters to the environment
	for _, paramSym := range v.Params.Required {
		param, ok := paramSym.Unwrap().(*values.Symbol)
		if !ok {
			return values.ErrNotASymbol
		}
		param = p.env.InternSymbol(param)
		paramScopes := paramSym.Scopes()
		_, ok = lenv.CreateLocalBinding(param, environment.BindingTypeVariable)
		if !ok {
			return values.WrapForeignErrorf(values.ErrDuplicateBinding, "duplicate parameter %q in lambda", param.Key)
		}
		if paramScopes != nil && len(paramScopes) > 0 {
			bindings := lenv.Bindings()
			if len(bindings) > 0 {
				binding := bindings[len(bindings)-1]
				if binding != nil {
					binding.SetScopes(paramScopes)
				}
			}
		}
		tpl.parameterCount++
	}

	// Handle rest parameter
	if v.Params.Rest != nil {
		rest, ok := v.Params.Rest.Unwrap().(*values.Symbol)
		if !ok {
			return values.ErrNotASymbol
		}
		rest = p.env.InternSymbol(rest)
		restScopes := v.Params.Rest.Scopes()
		_, ok = lenv.CreateLocalBinding(rest, environment.BindingTypeVariable)
		if !ok {
			return values.ErrDuplicateBinding
		}
		if restScopes != nil && len(restScopes) > 0 {
			bindings := lenv.Bindings()
			if len(bindings) > 0 {
				binding := bindings[len(bindings)-1]
				if binding != nil {
					binding.SetScopes(restScopes)
				}
			}
		}
		tpl.parameterCount++ // Rest parameter counts toward parameter count
		tpl.isVariadic = true
	}

	// Add template and environment to literals
	tpli := p.template.MaybeAppendLiteral(tpl)
	envi := p.template.MaybeAppendLiteral(childEnv)

	// Create compiler continuation for the body
	childCompiler := NewCompiletimeContinuation(tpl, childEnv)

	// Compile body expressions (last one in tail position)
	lambdaBodyContext := NewCompileTimeCallContext(true, ccnt.inExpression, childEnv)
	for i, bodyExpr := range v.Body {
		isLast := i == len(v.Body)-1
		bodyCtx := lambdaBodyContext.NotInTail()
		if isLast {
			bodyCtx = lambdaBodyContext
		}
		err := childCompiler.compileValidated(bodyCtx, bodyExpr)
		if err != nil {
			return err
		}
	}

	// Add return operation
	tpl.AppendOperations(NewOperationRestoreContinuation())

	// Emit MakeClosure operations in parent
	p.template.AppendOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(tpli),
		NewOperationPush(),
		NewOperationLoadLiteralByLiteralIndexImmediate(envi),
		NewOperationPush(),
		NewOperationMakeClosure(),
	)

	// Store in variable
	p.template.AppendOperations(NewOperationPush())
	if p.env.LocalEnvironment() != nil {
		li, _ := p.env.CreateLocalBinding(sym, environment.BindingTypeVariable)
		p.template.AppendOperations(NewOperationStoreLocalByLocalIndexImmediate(li))
	} else {
		gi, _ := p.env.CreateGlobalBinding(sym, environment.BindingTypeVariable)
		liti := p.template.MaybeAppendLiteral(gi)
		p.template.AppendOperations(NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(liti))
	}

	// define returns void
	p.template.AppendOperations(NewOperationLoadVoid())

	p.recordSource(startPC, v.Source())
	return nil
}

// compileValidatedLambda compiles a validated (lambda params body...) form.
func (p *CompileTimeContinuation) compileValidatedLambda(ccnt CompileTimeCallContext, v *validate.ValidatedLambda) error {
	startPC := len(p.template.operations)

	// Create child environment and template for lambda body
	lenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, p.env)
	tpl := NewNativeTemplate(0, 0, false)

	// Add parameters to the environment
	for _, paramSym := range v.Params.Required {
		param, ok := paramSym.Unwrap().(*values.Symbol)
		if !ok {
			return values.ErrNotASymbol
		}
		param = p.env.InternSymbol(param)
		paramScopes := paramSym.Scopes()
		_, ok = lenv.CreateLocalBinding(param, environment.BindingTypeVariable)
		if !ok {
			return values.WrapForeignErrorf(values.ErrDuplicateBinding, "duplicate parameter %q in lambda", param.Key)
		}
		if paramScopes != nil && len(paramScopes) > 0 {
			bindings := lenv.Bindings()
			if len(bindings) > 0 {
				binding := bindings[len(bindings)-1]
				if binding != nil {
					binding.SetScopes(paramScopes)
				}
			}
		}
		tpl.parameterCount++
	}

	// Handle rest parameter
	if v.Params.Rest != nil {
		rest, ok := v.Params.Rest.Unwrap().(*values.Symbol)
		if !ok {
			return values.ErrNotASymbol
		}
		rest = p.env.InternSymbol(rest)
		restScopes := v.Params.Rest.Scopes()
		_, ok = lenv.CreateLocalBinding(rest, environment.BindingTypeVariable)
		if !ok {
			return values.ErrDuplicateBinding
		}
		if restScopes != nil && len(restScopes) > 0 {
			bindings := lenv.Bindings()
			if len(bindings) > 0 {
				binding := bindings[len(bindings)-1]
				if binding != nil {
					binding.SetScopes(restScopes)
				}
			}
		}
		tpl.parameterCount++ // Rest parameter counts toward parameter count
		tpl.isVariadic = true
	}

	// Add template and environment to literals
	tpli := p.template.MaybeAppendLiteral(tpl)
	envi := p.template.MaybeAppendLiteral(childEnv)

	// Create compiler continuation for the body
	childCompiler := NewCompiletimeContinuation(tpl, childEnv)

	// Compile body expressions (last one in tail position)
	lambdaBodyContext := NewCompileTimeCallContext(true, ccnt.inExpression, childEnv)
	for i, bodyExpr := range v.Body {
		isLast := i == len(v.Body)-1
		bodyCtx := lambdaBodyContext.NotInTail()
		if isLast {
			bodyCtx = lambdaBodyContext
		}
		err := childCompiler.compileValidated(bodyCtx, bodyExpr)
		if err != nil {
			return err
		}
	}

	// Add return operation
	tpl.AppendOperations(NewOperationRestoreContinuation())

	// Emit MakeClosure operations for compileValidatedLambda
	p.template.AppendOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(tpli),
		NewOperationPush(),
		NewOperationLoadLiteralByLiteralIndexImmediate(envi),
		NewOperationPush(),
		NewOperationMakeClosure(),
	)

	p.recordSource(startPC, v.Source())
	return nil
}

// compileValidatedCaseLambda compiles a validated (case-lambda [clause] ...) form.
func (p *CompileTimeContinuation) compileValidatedCaseLambda(ccnt CompileTimeCallContext, v *validate.ValidatedCaseLambda) error {
	startPC := len(p.template.operations)

	// Compile each clause as a separate closure and push to stack
	for _, clause := range v.Clauses {
		// Create child environment and template for this clause
		lenv := environment.NewLocalEnvironment(0)
		childEnv := environment.NewEnvironmentFrameWithParent(lenv, p.env)
		tpl := NewNativeTemplate(0, 0, false)

		// Add parameters to the environment
		if clause.Params != nil {
			for _, paramSym := range clause.Params.Required {
				param, ok := paramSym.Unwrap().(*values.Symbol)
				if !ok {
					return values.ErrNotASymbol
				}
				param = p.env.InternSymbol(param)
				paramScopes := paramSym.Scopes()
				_, ok = lenv.CreateLocalBinding(param, environment.BindingTypeVariable)
				if !ok {
					return values.WrapForeignErrorf(values.ErrDuplicateBinding, "duplicate parameter %q in case-lambda clause", param.Key)
				}
				if paramScopes != nil && len(paramScopes) > 0 {
					bindings := lenv.Bindings()
					if len(bindings) > 0 {
						binding := bindings[len(bindings)-1]
						if binding != nil {
							binding.SetScopes(paramScopes)
						}
					}
				}
				tpl.parameterCount++
			}

			// Handle rest parameter
			if clause.Params.Rest != nil {
				rest, ok := clause.Params.Rest.Unwrap().(*values.Symbol)
				if !ok {
					return values.ErrNotASymbol
				}
				rest = p.env.InternSymbol(rest)
				restScopes := clause.Params.Rest.Scopes()
				_, ok = lenv.CreateLocalBinding(rest, environment.BindingTypeVariable)
				if !ok {
					return values.ErrDuplicateBinding
				}
				if restScopes != nil && len(restScopes) > 0 {
					bindings := lenv.Bindings()
					if len(bindings) > 0 {
						binding := bindings[len(bindings)-1]
						if binding != nil {
							binding.SetScopes(restScopes)
						}
					}
				}
				tpl.parameterCount++
				tpl.isVariadic = true
			}
		}

		// Add template and environment to literals
		tpli := p.template.MaybeAppendLiteral(tpl)
		envi := p.template.MaybeAppendLiteral(childEnv)

		// Create compiler continuation for the body
		childCompiler := NewCompiletimeContinuation(tpl, childEnv)

		// Compile body expressions (last one in tail position)
		lambdaBodyContext := NewCompileTimeCallContext(true, ccnt.inExpression, childEnv)
		for i, bodyExpr := range clause.Body {
			isLast := i == len(clause.Body)-1
			bodyCtx := lambdaBodyContext.NotInTail()
			if isLast {
				bodyCtx = lambdaBodyContext
			}
			err := childCompiler.compileValidated(bodyCtx, bodyExpr)
			if err != nil {
				return err
			}
		}

		// Add return operation
		tpl.AppendOperations(NewOperationRestoreContinuation())

		// Emit MakeClosure and push to stack
		p.template.AppendOperations(
			NewOperationLoadLiteralByLiteralIndexImmediate(tpli),
			NewOperationPush(),
			NewOperationLoadLiteralByLiteralIndexImmediate(envi),
			NewOperationPush(),
			NewOperationMakeClosure(),
			NewOperationPush(),
		)
	}

	// Create the case-lambda closure from all the clause closures
	p.template.AppendOperations(
		NewOperationMakeCaseLambdaClosure(len(v.Clauses)),
	)

	p.recordSource(startPC, v.Source())
	return nil
}

// compileValidatedSetBang compiles a validated (set! name expr) form.
func (p *CompileTimeContinuation) compileValidatedSetBang(ccnt CompileTimeCallContext, v *validate.ValidatedSetBang) error {
	startPC := len(p.template.operations)

	// Get the symbol
	sym, ok := v.Name.Unwrap().(*values.Symbol)
	if !ok {
		return values.ErrNotASymbol
	}
	sym = p.env.InternSymbol(sym)
	symbolScopes := v.Name.Scopes()

	// Compile the value expression
	err := p.compileValidated(ccnt.NotInTail(), v.Value)
	if err != nil {
		return err
	}
	p.AppendOperations(NewOperationPush())

	// Use scope-aware binding resolution
	binding := p.env.GetBindingWithScopes(sym, symbolScopes)
	if binding == nil {
		return values.WrapForeignErrorf(values.ErrNoSuchBinding, "no such binding %q with compatible scopes for set!", sym.Key)
	}

	// Check if it's a local binding
	li := p.env.GetLocalIndex(sym)
	if li != nil {
		p.AppendOperations(NewOperationStoreLocalByLocalIndexImmediate(li))
	} else {
		// Must be global
		gi := p.env.GetGlobalIndex(sym)
		if gi == nil {
			return values.WrapForeignErrorf(values.ErrNoSuchBinding, "internal error: binding found but no index for %q", sym.Key)
		}
		liti := p.template.MaybeAppendLiteral(gi)
		p.AppendOperations(
			NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(liti),
			NewOperationLoadVoid(),
		)
	}

	p.recordSource(startPC, v.Source())
	return nil
}

// compileValidatedQuote compiles a validated (quote datum) form.
func (p *CompileTimeContinuation) compileValidatedQuote(ccnt CompileTimeCallContext, v *validate.ValidatedQuote) error {
	startPC := len(p.template.operations)

	// Unwrap all syntax and intern symbols in the global environment.
	// This ensures symbol identity (eq?) works correctly across compilation boundaries per R7RS 6.5:
	// "Two symbols are identical (in the sense of eq?) if and only if their names are spelled the same way."
	unwrapped := v.Datum.UnwrapAll()
	interned := p.internSymbolsInValue(unwrapped)
	litIdx := p.template.MaybeAppendLiteral(interned)
	p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))

	p.recordSource(startPC, v.Source())
	return nil
}

// compileValidatedQuasiquote compiles a validated (quasiquote template) form.
// Quasiquote has complex runtime semantics, so we delegate to the existing compiler.
func (p *CompileTimeContinuation) compileValidatedQuasiquote(ccnt CompileTimeCallContext, v *validate.ValidatedQuasiquote) error {
	startPC := len(p.template.operations)

	// The existing quasiquote compiler expects the raw syntax template
	err := p.compileQuasiquoteDatum(ccnt, v.Template, 1)
	if err != nil {
		return err
	}

	p.recordSource(startPC, v.Source())
	return nil
}

// compileValidatedBegin compiles a validated (begin expr...) form.
func (p *CompileTimeContinuation) compileValidatedBegin(ccnt CompileTimeCallContext, v *validate.ValidatedBegin) error {
	startPC := len(p.template.operations)

	for i, expr := range v.Exprs {
		isLast := i == len(v.Exprs)-1
		exprCtx := ccnt.NotInTail()
		if isLast {
			exprCtx = ccnt // Last expression inherits tail position
		}
		err := p.compileValidated(exprCtx, expr)
		if err != nil {
			return err
		}
	}

	p.recordSource(startPC, v.Source())
	return nil
}

// compileValidatedCall compiles a validated function call (proc args...).
func (p *CompileTimeContinuation) compileValidatedCall(ccnt CompileTimeCallContext, v *validate.ValidatedCall) error {
	startPC := len(p.template.operations)

	var operationSaveContinuationIndex int
	if !ccnt.inTail {
		// Non-tail call: save continuation so we can return here after the call
		operationSaveContinuationIndex = p.template.operations.Length()
		p.AppendOperations(NewOperationSaveContinuationOffsetImmediate(0))
	}
	// Tail call: skip SaveContinuation - the callee will return directly to our caller

	// Compile the procedure expression
	err := p.compileValidated(ccnt.NotInTail(), v.Proc)
	if err != nil {
		return err
	}
	p.AppendOperations(NewOperationPush())

	// Compile arguments in order, pushing each to the stack
	for _, arg := range v.Args {
		err := p.compileValidated(ccnt.NotInTail(), arg)
		if err != nil {
			return err
		}
		p.AppendOperations(NewOperationPush())
	}

	// Pull the procedure and apply
	p.AppendOperations(
		NewOperationPull(),
		NewOperationApply(),
	)

	if !ccnt.inTail {
		// Patch the SaveContinuation offset for non-tail calls
		l := p.template.operations.Length()
		p.template.operations[operationSaveContinuationIndex] = NewOperationSaveContinuationOffsetImmediate(l - operationSaveContinuationIndex)
	}

	p.recordSource(startPC, v.Source())
	return nil
}

// compileValidatedLiteral handles self-evaluating values and passthrough forms.
func (p *CompileTimeContinuation) compileValidatedLiteral(ccnt CompileTimeCallContext, v *validate.ValidatedLiteral) error {
	// Check if this is actually a special form that passed through validation
	// (like define-syntax, define-library, etc.)
	if pair, ok := v.Value.(*syntax.SyntaxPair); ok {
		// This is a form that wasn't validated deeply - use the old path
		return p.CompilePrimitiveOrProcedureCall(ccnt, pair)
	}

	// Self-evaluating literal
	startPC := len(p.template.operations)
	err := p.CompileSelfEvaluating(ccnt, v.Value)
	if err != nil {
		return err
	}
	p.recordSource(startPC, v.Source())
	return nil
}
