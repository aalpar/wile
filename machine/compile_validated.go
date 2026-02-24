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
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/forms"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/internal/validate"
	"github.com/aalpar/wile/values"
)

// compileValidated dispatches compilation based on the validated expression type.
// Each ValidatedExpr type has a corresponding compile method that can assume
// the expression structure has already been validated.
//
// This is the main entry point for compiling validated IR. The validation phase
// has already verified syntax structure and produced type-safe ValidatedExpr nodes,
// so compilation can focus on bytecode generation without re-checking structure.
//
// Dispatch strategy:
//  1. Form-name lookup: Special forms (if, define, lambda, etc.) carry a formName
//     that maps to a registered compiler in the forms registry.
//  2. Type switch fallback: Expressions without form names (symbols, calls, literals)
//     are dispatched by their concrete ValidatedExpr type.

// compileValidatedSequence compiles a slice of validated expressions in order,
// with all but the last compiled in NotInTail position. Used by CompileValidatedBegin
// (pass 2) and compileBody (pass 2) to share the "last in tail position" logic.
func (p *CompileTimeContinuation) compileValidatedSequence(
	ctctx CompileTimeCallContext,
	body []validate.ValidatedExpr,
) error {
	for i, expr := range body {
		isLast := i == len(body)-1
		exprCtx := ctctx.NotInTail()
		if isLast {
			exprCtx = ctctx
		}
		err := p.compileValidated(exprCtx, expr)
		if err != nil {
			return err
		}
	}
	return nil
}

func (p *CompileTimeContinuation) compileValidated(ctctx CompileTimeCallContext, expr validate.ValidatedExpr) error {
	// Push the validated expression's source for finer-grained attribution.
	// Inner sub-expressions will push their own source, naturally creating
	// the correct nesting on the source stack.
	src := expr.Source()
	if src != nil {
		p.pushSource(src)
		defer p.popSource()
	}

	// Strategy 1: Form-name based dispatch via the forms registry.
	// Special forms like "if", "define", "lambda", "begin", "quote", etc. have
	// their formName set during validation. The forms registry maps these names
	// to compiler functions (e.g., "if" -> CompileValidatedIf).
	if expr.FormName() != "" {
		spec := forms.Lookup(expr.FormName())
		if spec != nil && spec.Compile != nil {
			// Found a registered compiler for this form - invoke it.
			// The compiler receives the validated expression with guaranteed structure.
			return spec.Compile(p, ctctx, expr)
		}
		// Form name present but no compiler registered - fall through to type switch.
		// This can happen for forms that pass through validation without deep checking.
	}

	// Strategy 2: Type-based dispatch for non-form expressions.
	// These are expressions that don't have a special form name:
	// - ValidatedCall: procedure application like (bindSymbolWithScopes x y)
	// - ValidatedSymbol: variable reference like x
	// - ValidatedLiteral: self-evaluating values like 42, "hello", #t
	switch v := expr.(type) {
	case *validate.ValidatedCall:
		// Procedure call: (proc arg1 arg2 ...)
		// Compiles proc and args, then emits Apply operation.
		return p.compileValidatedCall(ctctx, v)

	case *validate.ValidatedSymbol:
		// Variable reference: looks up binding and emits load operation.
		// May be local (stack-relative) or global (environment lookup).
		return p.CompileSymbol(ctctx, v.Symbol)

	case *validate.ValidatedLiteral:
		// Self-evaluating literal or passthrough form.
		// Numbers, strings, booleans, etc. compile to literal loads.
		// Some forms (define-syntax, define-library) pass through as literals
		// and are handled by the legacy compilation path.
		return p.compileValidatedLiteral(ctctx, v)

	default:
		// Exhaustiveness check: all ValidatedExpr types should be handled above.
		// This error indicates a new validated type was added without updating
		// this switch statement.
		return values.WrapForeignErrorf(values.ErrInvalidArgument, "unknown validated expression type: %T", expr)
	}
}

// CompileValidatedIf compiles a validated (if test conseq [alt]) form.
// The structure is guaranteed to be valid by the validator.
//
// Constant folding (Aho et al., Compilers §8.5): when the test is a
// compile-time literal, the entire if-form reduces to one branch. This
// is the simplest form of constant folding — evaluating known expressions
// at compile time rather than runtime.
// See BIBLIOGRAPHY.md "Constant Folding".
func (p *CompileTimeContinuation) CompileValidatedIf(ctctx CompileTimeCallContext, _ string, v *validate.ValidatedIf) error {
	// Constant folding: if the test is a compile-time-known literal, fold the
	// if form to just the consequent or alternative. Per R7RS, only #f is false;
	// all other values (including 0, "", '()) are truthy.
	folded, isFalsy := isLiteralFalse(v.Test)
	if folded {
		if isFalsy {
			// (if #f X Y) → Y, or void if no alternative
			if v.Alt != nil {
				return p.compileValidated(ctctx, v.Alt)
			}
			voidIdx := p.template.MaybeAppendLiteral(values.Void)
			p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(voidIdx))
			return nil
		}
		// (if <truthy-literal> X Y) → X
		return p.compileValidated(ctctx, v.Conseq)
	}

	// Compile the test condition (not in tail position).
	// The result lands in the value register; BranchOnFalseValue reads it
	// directly, avoiding a Push+Pop roundtrip through the eval stack.
	err := p.compileValidated(ctctx.NotInTail(), v.Test)
	if err != nil {
		return err
	}

	// Set up branch-on-false to skip consequent (reads value register directly)
	branchOnFalseIndex := p.template.CodeLen()
	p.AppendOperations(NewOperationBranchOnFalseValueOffsetImmediate(0)) // placeholder

	// Compile consequent (inherits tail position)
	err = p.compileValidated(ctctx, v.Conseq)
	if err != nil {
		return err
	}

	// Set up unconditional branch to skip alternative
	branchToEndIndex := p.template.CodeLen()
	p.AppendOperations(NewOperationBranchOffsetImmediate(0)) // placeholder

	// Target for branch-on-false
	altStart := p.template.CodeLen()

	// Compile alternative (or load void if none)
	if v.Alt != nil {
		err = p.compileValidated(ctctx, v.Alt)
		if err != nil {
			return err
		}
	} else {
		// No alternative - return void
		voidIdx := p.template.MaybeAppendLiteral(values.Void)
		p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(voidIdx))
	}

	// Fix up branch targets
	endIndex := p.template.CodeLen()
	p.patchBranchTarget(branchOnFalseIndex, altStart)
	p.patchBranchTarget(branchToEndIndex, endIndex)

	return nil
}

// isLiteralFalse checks if a validated expression is a compile-time-known
// literal. Returns (true, true) for #f, (true, false) for any other literal
// (which is truthy per R7RS), and (false, false) for non-literal expressions.
func isLiteralFalse(expr validate.ValidatedExpr) (isLiteral, isFalse bool) {
	lit, ok := expr.(*validate.ValidatedLiteral)
	if !ok {
		return false, false
	}
	if lit.Value == nil {
		return false, false
	}
	unwrapped := lit.Value.UnwrapAll()
	if unwrapped == nil {
		return false, false
	}
	b, isBool := unwrapped.(*values.Boolean)
	if !isBool {
		// Non-boolean literal — truthy per R7RS
		return true, false
	}
	return true, !b.Datum()
}

// CompileValidatedDefine compiles a validated define form.
func (p *CompileTimeContinuation) CompileValidatedDefine(ctctx CompileTimeCallContext, formName string, v *validate.ValidatedDefine) error {
	if v.IsFunction {
		// (define (name params...) body...) - compile as lambda then define
		return p.CompileValidatedDefineFn(ctctx, formName, v)
	}
	// (define name expr) - compile value then store
	return p.compileValidatedDefineVar(ctctx, v)
}

// declareDefineBinding creates the binding for a define form before compiling its value.
// This early declaration enables self-recursive definitions like (define (fact n) ... (fact (- n 1)) ...).
// Returns the interned symbol for use by the caller when storing the compiled value.
func (p *CompileTimeContinuation) declareDefineBinding(v *validate.ValidatedDefine) *values.Symbol {
	// Get the interned symbol for the name (validator guarantees it's a SyntaxSymbol)
	sym := p.env.InternSymbol(v.Name().Sym)
	symbolScopes := v.Name().Scopes()
	// Create binding early for recursion support
	if p.env.LocalEnvironment() != nil {
		_, _ = p.env.MaybeCreateLocalBindingWithScopes(sym, environment.BindingTypeVariable, symbolScopes, nil)
		return sym
	}
	gi, created := p.env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
	if created && symbolScopes == nil {
		return sym
	}
	binding := p.env.GetGlobalBinding(gi)
	if binding == nil {
		return sym
	}
	binding.SetScopes(symbolScopes)
	return sym
}

// compileValidatedDefineVar compiles the simple variable form of define.
//
// Usage: (define name expr)
//
// Examples:
//
//	(define x 42)                    ; bind x to 42
//	(define pi 3.14159)              ; bind pi to a float
//	(define greet (lambda (n) ...))  ; bind greet to a lambda (not function shorthand)
//
// Unlike the function shorthand (define (name ...) ...), this form does NOT
// create the binding before evaluating expr. This means self-reference in expr
// will fail: (define x (+ x 1)) is an error if x is not already defined.
//
// For the function shorthand form, see CompileValidatedDefineFn.
func (p *CompileTimeContinuation) compileValidatedDefineVar(ctctx CompileTimeCallContext, v *validate.ValidatedDefine) error {
	// Step 1: Declare the binding in the environment.
	// For variable defines, this still happens early, but since the value expression
	// cannot reference itself (unlike function defines), the order is less critical.
	// The binding is created but not yet populated with a value.
	sym := p.declareDefineBinding(v)

	// Step 2: Compile the value expression.
	// The expression is NOT in tail position because define is a definition, not
	// an expression that returns a meaningful value. The result goes into the
	// value register, ready to be stored.
	err := p.compileValidated(ctctx.NotInTail(), v.SubExp())
	if err != nil {
		return err
	}

	// Step 3: Store the compiled value into the binding and load void.
	// After this, the binding holds the value and the value register contains void
	// (since define returns an unspecified value per R7RS).
	return p.emitDefineStore(sym)
}

// emitDefineStore emits bytecode to store the compiled value into the defined binding.
// Assumes the value to store is in the value register. Emits push, store (local or global),
// and loads void since define returns an unspecified value per R7RS.
//
// This is the final phase of define compilation, shared by both compileValidatedDefineVar
// and CompileValidatedDefineFn. The caller has already:
//  1. Declared the binding via declareDefineBinding
//  2. Compiled the value expression (leaving result in value register)
func (p *CompileTimeContinuation) emitDefineStore(sym *values.Symbol) error {
	// Push the value from the value register to the eval stack.
	// Store operations consume from the stack, not the value register.
	p.AppendOperations(NewOperationPush())

	if p.env.LocalEnvironment() != nil {
		// Local context (inside a lambda body): store to local variable slot.
		// EnsureLocalBinding returns the slot index; the binding was already
		// declared by declareDefineBinding, so this just retrieves the index.
		li, _ := p.env.EnsureLocalBinding(sym, environment.BindingTypeVariable)
		p.AppendOperations(NewOperationStoreLocalByLocalIndexImmediate(li))
	} else {
		// Global context (top-level): store to global environment.
		// Global indices are stored in the literals pool since they're runtime values.
		// The operation loads the index from literals and stores the value there.
		gi, _ := p.env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
		liti := p.template.MaybeAppendLiteral(gi)
		p.AppendOperations(NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(liti))
	}

	// Load void into the value register as define's return value.
	// Per R7RS 5.3.1: "The result of a definition is unspecified."
	// We use void to represent this unspecified value.
	p.AppendOperations(NewOperationLoadVoid())
	return nil
}

// CompileValidatedDefineFn compiles the function shorthand form of define.
//
// Usage: (define (name param ...) body ...)
//
//	(define (name param ... . rest) body ...)
//
// This is syntactic sugar equivalent to:
//
//	(define name (lambda (param ...) body ...))
//
// Examples:
//
//	(define (square x) (* x x))           ; fixed arity
//	(define (sum . args) (apply + args))  ; variadic (all args)
//	(define (sum x . rest) (apply + x rest))  ; variadic (1+ args)
//
// The function name is bound before compiling the body to enable self-recursion:
//
//	(define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))
func (p *CompileTimeContinuation) CompileValidatedDefineFn(ctctx CompileTimeCallContext, _ string, v *validate.ValidatedDefine) error {
	// Step 1: Declare the binding early for self-recursion support.
	// This must happen before compiling the body so that references to the
	// function name within the body resolve correctly.
	sym := p.declareDefineBinding(v)

	// Step 2: Set up the closure's environment and bytecode template.
	// The local environment holds parameter bindings; compileClosure creates
	// the child environment frame after binding parameters.
	lenv := environment.NewLocalEnvironment(0)
	tpl := NewNativeTemplate(0, 0, false)

	// Record the function name for stack traces and debugging.
	// This makes error messages show "(fact)" instead of "(anonymous)".
	tpl.SetName(sym.Key)

	// Step 3: Compile the closure - binds parameters, compiles body, emits MakeClosure.
	// After this, the closure is in the value register ready to be stored.
	err := p.compileClosure(ctctx, tpl, lenv, v)
	if err != nil {
		return err
	}

	// Step 4: Store the closure in the binding and load void as the result.
	// define returns an unspecified value per R7RS; we use void.
	return p.emitDefineStore(sym)
}

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
			param := p.env.InternSymbol(paramSym.Sym)
			paramScopes := paramSym.Scopes()

			_, ok := lenv.EnsureLocalBinding(param, environment.BindingTypeVariable)
			if !ok {
				return 0, 0, values.WrapForeignErrorf(
					values.ErrDuplicateBinding,
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

	// Phase 5: Peephole optimization before escape analysis (optimization may
	// change which ops are present). Escape analysis determines whether Apply
	// can skip copying the closure's environment frame — safe when the body
	// contains no SaveContinuation and no MakeClosure.
	tpl.Optimize()
	tpl.computeNoCopyApply()

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

// CompileValidatedLambda compiles a validated (lambda params body...) form.
func (p *CompileTimeContinuation) CompileValidatedLambda(ctctx CompileTimeCallContext, _ string, v *validate.ValidatedLambda) error {
	// Create local environment and template for lambda body.
	// compileClosure creates the child environment frame after binding parameters.
	lenv := environment.NewLocalEnvironment(0)
	tpl := NewNativeTemplate(0, 0, false)

	err := p.compileClosure(ctctx, tpl, lenv, v)
	if err != nil {
		return err
	}
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
	lambdaBodyContext := NewCompileTimeCallContext(ctctx.ctx, true, ctctx.inExpression)

	// R7RS §5.3.2: Internal definitions use letrec* semantics
	// Pass 1: Pre-declare all define bindings so forward references work
	for _, bodyExpr := range clause.Body() {
		childCompiler.predeclareDefineBindingFromValidated(bodyExpr)
	}

	// Pass 2: Compile all expressions (with all bindings now visible)
	err := childCompiler.compileValidatedSequence(lambdaBodyContext, clause.Body())
	if err != nil {
		return err
	}

	childCompiler.AppendOperations(NewOperationRestoreContinuation())
	return nil
}

// predeclareDefineBindingFromValidated pre-creates a binding for a validated define form.
// This enables forward references within lambda bodies per R7RS §5.3.2.
func (p *CompileTimeContinuation) predeclareDefineBindingFromValidated(expr validate.ValidatedExpr) {
	def, ok := expr.(*validate.ValidatedDefine)
	if !ok {
		return // Not a define, skip
	}

	sym := p.env.InternSymbol(def.Name().Sym)
	symbolScopes := def.Name().Scopes()

	p.bindSymbolWithScopes(sym, symbolScopes)
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
func bindRestParameter(v validate.ValidatedBodyAndParams, p *CompileTimeContinuation, lenv *environment.LocalEnvironmentFrame, tpl *NativeTemplate) error {
	// Early exit if no rest parameter. Most lambdas are fixed-arity.
	if v.Params().Rest == nil {
		return nil
	}

	// Intern the rest parameter symbol for consistent identity.
	rest := p.env.InternSymbol(v.Params().Rest.Sym)
	restScopes := v.Params().Rest.Scopes()

	// Create a local binding slot for the rest parameter. This slot comes after
	// all required parameter slots. The VM knows to populate it with a list of
	// excess arguments because tpl.isVariadic is set below.
	_, ok := lenv.EnsureLocalBinding(rest, environment.BindingTypeVariable)
	if !ok {
		// Rest parameter name conflicts with a required parameter (e.g., (lambda (x . x) ...))
		return values.WrapForeignErrorf(values.ErrDuplicateBinding, "duplicate rest parameter %q in lambda", rest.Key)
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

// CompileValidatedCaseLambda compiles a validated (case-lambda [clause] ...) form.
//
// case-lambda (R7RS 4.2.9) creates a procedure that dispatches to different
// implementations based on the number of arguments. For example:
//
//	(case-lambda
//	  ((x) (* x x))           ; 1 arg: square
//	  ((x y) (* x y))         ; 2 args: multiply
//	  ((x y . rest) (apply + x y rest)))  ; 2+ args: sum all
//
// At runtime, the VM selects the first clause whose arity matches the call.
func (p *CompileTimeContinuation) CompileValidatedCaseLambda(ctctx CompileTimeCallContext, _ string, v *validate.ValidatedCaseLambda) error {
	// Phase 1: Compile each clause as a separate closure.
	// Unlike regular lambda which produces one closure, case-lambda produces
	// multiple closures (one per clause) that are combined into a dispatch structure.
	// Each clause closure is pushed to the eval stack for later combination.
	for _, clause := range v.Clauses() {
		lenv := environment.NewLocalEnvironment(0)
		tpl := NewNativeTemplate(0, 0, false)

		tpli, envi, err := p.compileClosureBody(ctctx, tpl, lenv, clause, "case-lambda clause")
		if err != nil {
			return err
		}

		p.AppendOperations(
			NewOperationLoadLiteralByLiteralIndexImmediate(tpli),
			NewOperationPush(),
			NewOperationLoadLiteralByLiteralIndexImmediate(envi),
			NewOperationPush(),
			NewOperationMakeClosure(),
			NewOperationPush(),
		)
	}

	// Phase 2: Combine all clause closures into a single case-lambda dispatch structure.
	// MakeCaseLambdaClosure pops N closures from the stack (in reverse order) and
	// creates a CaseLambdaClosure that, when called, dispatches to the appropriate
	// clause based on argument count. Clauses are tried in order; first match wins.
	p.AppendOperations(
		NewOperationMakeCaseLambdaClosure(len(v.Clauses())),
	)

	return nil
}

// CompileValidatedSetBang compiles a validated (set! name expr) form.
func (p *CompileTimeContinuation) CompileValidatedSetBang(ctctx CompileTimeCallContext, _ string, v *validate.ValidatedSetBang) error {
	// Get the interned symbol (validator guarantees it's a SyntaxSymbol)
	sym := p.env.InternSymbol(v.Name.Sym)
	symbolScopes := v.Name.Scopes()

	// Compile the value expression
	err := p.compileValidated(ctctx.NotInTail(), v.SubExp())
	if err != nil {
		return err
	}
	p.AppendOperations(NewOperationPush())

	// Use scope-aware binding resolution for validation
	binding := p.env.GetBindingWithScopes(sym, symbolScopes)
	if binding == nil {
		return values.WrapForeignErrorf(values.ErrNoSuchBinding, "no such binding %q with compatible scopes for set!", sym.Key)
	}

	// Check if it's a local binding
	// M1 fix: Use scope-aware lookup when symbol has scopes (matches CompileSymbol pattern)
	var li *environment.LocalIndex
	if len(symbolScopes) > 0 {
		// Symbol has scopes (from macro expansion), use scope-aware lookup
		li = p.env.GetLocalIndexWithScopes(sym, symbolScopes)
	} else {
		// Symbol has no scopes (from user code), use regular lookup
		li = p.env.GetLocalIndex(sym)
	}

	if li != nil {
		p.AppendOperations(
			NewOperationStoreLocalByLocalIndexImmediate(li),
			NewOperationLoadVoid(),
		)
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

	return nil
}

// CompileValidatedQuote compiles a validated (quote datum) form.
func (p *CompileTimeContinuation) CompileValidatedQuote(_ CompileTimeCallContext, _ string, v *validate.ValidatedQuote) error {
	// Unwrap all syntax and intern symbols in the global environment.
	// This ensures symbol identity (eq?) works correctly across compilation boundaries per R7RS 6.5:
	// "Two symbols are identical (in the sense of eq?) if and only if their names are spelled the same way."
	unwrapped := v.Datum.UnwrapAll()
	interned := p.internSymbolsInValue(unwrapped)
	litIdx := p.template.MaybeAppendLiteral(interned)
	p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
	return nil
}

// CompileValidatedQuasiquote compiles a validated (quasiquote template) form.
// Quasiquote has complex runtime semantics, so we delegate to the existing compiler.
func (p *CompileTimeContinuation) CompileValidatedQuasiquote(ctctx CompileTimeCallContext, _ string, v *validate.ValidatedQuasiquote) error {
	// The existing quasiquote compiler expects the raw syntax template
	err := p.compileQuasiquoteDatum(ctctx, v.Template, 1)
	if err != nil {
		return err
	}
	return nil
}

// CompileValidatedBegin compiles a validated (begin expr...) form.
//
// begin (R7RS 4.2.3) sequences expressions for side effects. All expressions
// are evaluated left-to-right; the value of the last expression becomes
// the value of the entire begin form.
//
// R7RS §5.3.2: Internal definitions use letrec* semantics - all defined names
// are visible throughout the body, enabling forward references between defines.
//
// Example: (begin (display "hello") (newline) 42) => 42 (after printing)
func (p *CompileTimeContinuation) CompileValidatedBegin(ctctx CompileTimeCallContext, _ string, v *validate.ValidatedBegin) error {
	// R7RS §5.3.2: Internal definitions use letrec* semantics
	// Pass 1: Pre-declare all define bindings so forward references work
	for _, expr := range v.Body() {
		p.predeclareDefineBindingFromValidated(expr)
	}

	// Pass 2: Compile each expression in sequence
	return p.compileValidatedSequence(ctctx, v.Body())
}

// compileValidatedCall compiles a validated function call (proc args...).
func (p *CompileTimeContinuation) compileValidatedCall(ctctx CompileTimeCallContext, v *validate.ValidatedCall) error {
	var operationSaveContinuationIndex int
	if !ctctx.inTail {
		// Non-tail call: save continuation so we can return here after the call
		operationSaveContinuationIndex = p.emitPatchableSaveContinuation()
	}
	// Tail call: skip SaveContinuation - the callee will return directly to our caller

	// Compile the procedure expression
	err := p.compileValidated(ctctx.NotInTail(), v.Proc())
	if err != nil {
		return err
	}
	p.AppendOperations(NewOperationPush())

	// Compile arguments in order, pushing each to the stack
	for _, arg := range v.Body() {
		err := p.compileValidated(ctctx.NotInTail(), arg)
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

	if !ctctx.inTail {
		p.patchSaveContinuationOffset(operationSaveContinuationIndex)
	}

	return nil
}

// compileValidatedLiteral handles self-evaluating values and passthrough forms.
func (p *CompileTimeContinuation) compileValidatedLiteral(ctctx CompileTimeCallContext, v *validate.ValidatedLiteral) error {
	// Check if this is actually a special form that passed through validation
	// (like define-syntax, define-library, etc.)
	pair, ok := v.Value.(*syntax.SyntaxPair)
	if ok && !pair.IsEmptyList() {
		// This is a form that wasn't validated deeply - use the old path
		return p.CompilePrimitiveOrProcedureCall(ctctx, pair)
	}

	// Self-evaluating literal
	err := p.CompileSelfEvaluating(ctctx, v.Value)
	if err != nil {
		return err
	}
	return nil
}

// CompileValidatedDynamicWind compiles a validated (dynamic-wind before thunk after) form.
//
// R7RS §6.10: dynamic-wind calls thunk without arguments, returning the result(s).
// Before is called whenever execution enters the dynamic extent of the call to thunk,
// and after is called whenever it exits.
//
// The key insight is that by compiling to bytecode, the cleanup code (calling after)
// is in the bytecode stream. When a continuation is captured inside the thunk and
// later restored, the cleanup code will run on normal completion.
//
// Bytecode structure:
//
//	<compile before> PUSH
//	<compile thunk>  PUSH
//	<compile after>  PUSH          ; Stack: [before, thunk, after]
//	PEEK_K 2                       ; value = before
//	SAVE_CONTINUATION →after_before
//	APPLY                          ; call before()
//	after_before:                  ; Stack: [before, thunk, after]
//	OP_PUSH_WIND                   ; create winding frame
//	PEEK_K 1                       ; value = thunk
//	SAVE_CONTINUATION →after_thunk
//	APPLY                          ; call thunk()
//	after_thunk:                   ; Stack: [before, thunk, after]
//	PUSH                           ; save thunk result, Stack: [before, thunk, after, result]
//	OP_POP_WIND                    ; pop winding frame
//	PEEK_K 1                       ; value = after
//	SAVE_CONTINUATION →after_after
//	APPLY                          ; call after()
//	after_after:                   ; Stack: [before, thunk, after, result]
//	PEEK_K 0                       ; value = result (thunk's return value)
//	DROP DROP DROP DROP            ; clean up stack
func (p *CompileTimeContinuation) CompileValidatedDynamicWind(ctctx CompileTimeCallContext, _ string, v *validate.ValidatedDynamicWind) error {
	// Phase 1: Compile and push before, thunk, after to stack
	// Note: We compile in expression context (not tail) since we need all three values
	exprCtx := NewCompileTimeCallContext(ctctx.ctx, false, true)

	err := p.compileValidated(exprCtx, v.Before)
	if err != nil {
		return err
	}
	p.AppendOperations(NewOperationPush())

	err = p.compileValidated(exprCtx, v.Thunk)
	if err != nil {
		return err
	}
	p.AppendOperations(NewOperationPush())

	err = p.compileValidated(exprCtx, v.After)
	if err != nil {
		return err
	}
	p.AppendOperations(NewOperationPush())
	// Stack: [before, thunk, after]

	// Phase 2: Call before thunk
	// Get before into value register (at depth 2)
	p.AppendOperations(NewOperationPeekK(2))
	// Save continuation to return here after call
	beforeCallReturnIndex := p.emitPatchableSaveContinuation()
	// Apply with 0 args (stack is fresh after SaveContinuation)
	p.AppendOperations(NewOperationApply())
	p.patchSaveContinuationOffset(beforeCallReturnIndex)
	// after_before: Stack is restored to [before, thunk, after]

	// Phase 3: Push winding frame
	p.AppendOperations(NewOperationPushWind())

	// Phase 4: Call thunk
	// Get thunk into value register (at depth 1)
	p.AppendOperations(NewOperationPeekK(1))
	thunkCallReturnIndex := p.emitPatchableSaveContinuation()
	p.AppendOperations(NewOperationApply())
	p.patchSaveContinuationOffset(thunkCallReturnIndex)
	// after_thunk: Stack is restored to [before, thunk, after]
	// Thunk's result is in value register

	// Save thunk result on stack
	p.AppendOperations(NewOperationPush())
	// Stack: [before, thunk, after, result]

	// Phase 5: Pop winding frame
	p.AppendOperations(NewOperationPopWind())

	// Phase 6: Call after thunk
	// Get after into value register (at depth 1 because result is at top)
	p.AppendOperations(NewOperationPeekK(1))
	afterCallReturnIndex := p.emitPatchableSaveContinuation()
	p.AppendOperations(NewOperationApply())
	p.patchSaveContinuationOffset(afterCallReturnIndex)
	// after_after: Stack is restored to [before, thunk, after, result]

	// Phase 7: Return thunk result
	// Get result into value register (at top of stack)
	p.AppendOperations(NewOperationPeekK(0))
	// Clean up stack
	p.AppendOperations(
		NewOperationDrop(), // result
		NewOperationDrop(), // after
		NewOperationDrop(), // thunk
		NewOperationDrop(), // before
	)

	return nil
}
