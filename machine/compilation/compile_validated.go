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
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/forms"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/internal/validate"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// compile_validated.go handles compilation of core Scheme forms that go through
// upfront validation (validate.Validate) before codegen. These are the fixed
// R7RS core forms: if, lambda, define, set!, quote, quasiquote, begin,
// case-lambda, dynamic-wind, apply, with-continuation-mark, let, let*,
// letrec, letrec* (binding forms compiled in compile_let.go).
//
// Extension forms (define-syntax, import, include, syntax-case, etc.) pass through
// validation as ValidatedLiteral and are compiled via registerSyntaxCompiler() in
// register.go, which registers compile functions in the forms registry.
//
// Decision criteria: if a form has fixed syntax that can be validated once before
// compilation, it goes through the validated path. If a form's syntax is
// extensible or defined by the extension system itself, it uses the registry path.

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

	// Unified dispatch by concrete type. Tier 1 forms (if, define, lambda, etc.)
	// dispatch directly — no string lookup, no type assertion adapter. Tier 2
	// forms (define-syntax, include, etc.) arrive as ValidatedLiteral and dispatch
	// through the compiler registry by FormName.
	switch v := expr.(type) {
	case *validate.ValidatedIf:
		return p.CompileValidatedIf(ctctx, v)
	case *validate.ValidatedDefine:
		return p.CompileValidatedDefine(ctctx, v)
	case *validate.ValidatedLambda:
		return p.CompileValidatedLambda(ctctx, v)
	case *validate.ValidatedCaseLambda:
		return p.CompileValidatedCaseLambda(ctctx, v)
	case *validate.ValidatedSetBang:
		return p.CompileValidatedSetBang(ctctx, v)
	case *validate.ValidatedQuote:
		return p.CompileValidatedQuote(ctctx, v)
	case *validate.ValidatedBegin:
		return p.CompileValidatedBegin(ctctx, v)
	case *validate.ValidatedQuasiquote:
		return p.CompileValidatedQuasiquote(ctctx, v)
	case *validate.ValidatedDynamicWind:
		return p.CompileValidatedDynamicWind(ctctx, v)
	case *validate.ValidatedApply:
		return p.CompileValidatedApply(ctctx, v)
	case *validate.ValidatedWithContinuationMark:
		return p.CompileValidatedWithContinuationMark(ctctx, v)
	case *validate.ValidatedLet:
		return p.CompileValidatedLet(ctctx, v)

	case *validate.ValidatedCall:
		return p.compileValidatedCall(ctctx, v)
	case *validate.ValidatedSymbol:
		return p.CompileSymbol(ctctx, v.Symbol)

	case *validate.ValidatedLiteral:
		// Tier 2 forms (define-syntax, include, syntax-case, etc.) are validated
		// as literals with a FormName. Dispatch through the compiler registry.
		if v.FormName() != "" {
			compiler := LookupCompiler(v.FormName())
			if compiler != nil {
				return compiler(p, ctctx, v)
			}
			// Form is known to the validator but has no compiler — it should have
			// been fully handled during expansion (e.g., let-syntax, letrec-syntax,
			// syntax-rules). If it reached compilation, the expander has a bug.
			if forms.Lookup(v.FormName()) != nil {
				return werr.WrapForeignErrorf(werr.ErrInvalidSyntax,
					"compile: form %q has no compiler (should be handled during expansion)", v.FormName())
			}
		}
		return p.compileValidatedLiteral(ctctx, v)

	default:
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "unknown validated expression type: %T", expr)
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
func (p *CompileTimeContinuation) CompileValidatedIf(ctctx CompileTimeCallContext, v *validate.ValidatedIf) error {
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
			p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(voidIdx))
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
	p.AppendOperations(machine.NewOperationBranchOnFalseValueOffsetImmediate(0)) // placeholder

	// Compile consequent (inherits tail position)
	err = p.compileValidated(ctctx, v.Conseq)
	if err != nil {
		return err
	}

	// Set up unconditional branch to skip alternative
	branchToEndIndex := p.template.CodeLen()
	p.AppendOperations(machine.NewOperationBranchOffsetImmediate(0)) // placeholder

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
		p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(voidIdx))
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
	return true, !b.Value
}

// CompileValidatedDefine compiles a validated define form.
func (p *CompileTimeContinuation) CompileValidatedDefine(ctctx CompileTimeCallContext, v *validate.ValidatedDefine) error {
	if v.IsFunction {
		// (define (name params...) body...) - compile as lambda then define
		return p.CompileValidatedDefineFn(ctctx, v)
	}
	// (define name expr) - compile value then store
	return p.compileValidatedDefineVar(ctctx, v)
}

// declareDefineBinding creates the binding for a define form before compiling its value.
// This early declaration enables self-recursive definitions like (define (fact n) ... (fact (- n 1)) ...).
// Returns the symbol for use by the caller when storing the compiled value.
func (p *CompileTimeContinuation) declareDefineBinding(v *validate.ValidatedDefine) (*values.Symbol, error) {
	// Get the symbol for the name (validator guarantees it's a SyntaxSymbol)
	sym := v.Name().Sym
	symbolScopes := v.Name().Scopes()
	symbolSource := v.Name().SourceContext()
	// Create binding early for recursion support
	if p.env.LocalEnvironment() != nil {
		_, _ = p.env.MaybeCreateLocalBinding(sym, environment.BindingTypeVariable, symbolScopes, symbolSource)
		return sym, nil
	}
	gi, created := p.env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)

	// Opt-in top-level immutability (WithImmutableTopLevel): when enabled, a
	// defined-once, never-set!-in-unit top-level define is rebind-stable, and a
	// later redefine of an already-stable binding is rejected. When disabled,
	// behavior is identical to before (the fast path below still fires).
	//
	// Q3 (layered-environment): enforce in the namespace's own user runtime global
	// (the per-Engine user top-level) AND in its sealed base (immutable primitives +
	// bootstrap procedures — the optimizer's Stable anchors; capture-safe procedures like
	// zero?/not must stay Stable here or the frame-reclaim classifier stops trusting them).
	// EXEMPT user-loaded libraries: a library body compiles against a flat NewChildRuntime
	// frame that shares its parent's namespace (so it is neither ns.Runtime() nor
	// ns.SealedBase()), keeping a library's cross-form (define x)/(set! x) mutable (R2).
	ns := p.env.Namespace()
	immTop := ns != nil && ns.ImmutableTopLevel() &&
		(ns.Runtime() == p.env || ns.SealedBase() == p.env)

	if created && len(symbolScopes) == 0 && symbolSource == nil && !immTop {
		return sym, nil
	}
	binding := p.env.GetGlobalBinding(gi)
	if binding == nil {
		return sym, nil
	}
	if immTop {
		// Redefine guard: a second top-level define of an already-stable binding
		// would rebind a procedure the frame optimizer may have proven stable in
		// an earlier unit. Reject it (defined-once across units). Keyed on the
		// Stable field specifically, NOT IsStable(): an imported binding (Stable
		// field false) must still be supersedable by a define (R7RS §5.3.1).
		m := binding.Meta()
		if !created && m != nil && m.Stable {
			return nil, werr.WrapForeignErrorf(
				werr.ErrImmutableBinding,
				"define: cannot redefine immutable top-level binding %q",
				sym.Key,
			)
		}
	}
	// Top-level define supersedes an imported binding (R7RS §5.3.1): the define
	// overwrites the value in the same slot, so drop the import *provenance* and
	// a subsequent set! on this binding is permitted.
	if binding.IsImported() {
		binding.EnsureMeta().Imported = false
	}
	m := binding.EnsureMeta()
	m.Scopes = symbolScopes
	if symbolSource != nil {
		m.Source = symbolSource
	}
	if immTop {
		// Discharge the rebind-stability conclusion from in-unit evidence: the
		// language now forbids the cross-unit set!/redefine that StableInUnit
		// alone could not rule out (the set! gate + the redefine guard above),
		// so StableInUnit becomes a sound basis for Stable. Off by default.
		m.Stable = v.StableInUnit
	}
	return sym, nil
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
	sym, err := p.declareDefineBinding(v)
	if err != nil {
		return err
	}

	// Step 2: Compile the value expression.
	// The expression is NOT in tail position because define is a definition, not
	// an expression that returns a meaningful value. The result goes into the
	// value register, ready to be stored.
	err = p.compileValidated(ctctx.NotInTail(), v.SubExp())
	if err != nil {
		return err
	}

	// Step 3: Store the compiled value into the binding and load void.
	// After this, the binding holds the value and the value register contains void
	// (since define returns an unspecified value per R7RS).
	return p.emitDefineStore(sym, v.Name().Scopes())
}

// emitDefineStore emits bytecode to store the compiled value into the defined binding.
// Assumes the value to store is in the value register. Emits push, store (local or global),
// and loads void since define returns an unspecified value per R7RS.
//
// This is the final phase of define compilation, shared by both compileValidatedDefineVar
// and CompileValidatedDefineFn. The caller has already:
//  1. Declared the binding via declareDefineBinding
//  2. Compiled the value expression (leaving result in value register)
func (p *CompileTimeContinuation) emitDefineStore(sym *values.Symbol, scopes []*syntax.Scope) error {
	// Push the value from the value register to the eval stack.
	// Store operations consume from the stack, not the value register.
	p.AppendOperations(machine.NewOperationPush())

	if p.env.LocalEnvironment() != nil {
		// Local context (inside a lambda body): store to local variable slot.
		// The binding was already declared by declareDefineBinding (scope-aware),
		// so retrieve its slot scope-aware too. A bare-name lookup would send two
		// hygienically-distinct same-named internal defines to slot 0, leaving the
		// second slot #!void (the internal-define analogue of the let-store bug).
		li := p.env.GetLocalIndex(sym, scopes)
		if li == nil {
			return werr.WrapForeignErrorf(machine.ErrBindingNotFound,
				"compile define: binding %q not found in local environment", sym.Key)
		}
		p.AppendOperations(machine.NewOperationStoreLocalByLocalIndexImmediate(li))
	} else {
		// Global context (top-level): store to global environment.
		// Global indices are stored in the literals pool since they're runtime values.
		// The operation loads the index from literals and stores the value there.
		gi, _ := p.env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
		liti := p.template.MaybeAppendLiteral(gi)
		p.AppendOperations(machine.NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(liti))
	}

	// Load void into the value register as define's return value.
	// Per R7RS 5.3.1: "The result of a definition is unspecified."
	// We use void to represent this unspecified value.
	p.AppendOperations(machine.NewOperationLoadVoid())
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
func (p *CompileTimeContinuation) CompileValidatedDefineFn(ctctx CompileTimeCallContext, v *validate.ValidatedDefine) error {
	// Step 1: Declare the binding early for self-recursion support.
	// This must happen before compiling the body so that references to the
	// function name within the body resolve correctly.
	sym, err := p.declareDefineBinding(v)
	if err != nil {
		return err
	}

	// Step 2: Set up the closure's environment and bytecode template.
	// The local environment holds parameter bindings; compileClosure creates
	// the child environment frame after binding parameters.
	lenv := environment.NewLocalEnvironment(0)
	tpl := machine.NewNativeTemplate(0, 0, false)

	// Record the function name for stack traces and debugging.
	// This makes error messages show "(fact)" instead of "(anonymous)".
	tpl.SetName(sym.Key)

	// Step 3: Compile the closure - binds parameters, compiles body, emits MakeClosure.
	// After this, the closure is in the value register ready to be stored.
	err = p.compileClosure(ctctx, tpl, lenv, v, p.frameReuseForDefine(v))
	if err != nil {
		return err
	}

	// Step 4: Store the closure in the binding and load void as the result.
	// define returns an unspecified value per R7RS; we use void.
	return p.emitDefineStore(sym, v.Name().Scopes())
}

// frameReuseForDefine returns the frame-reuse disposition for a function-form
// define. At most one mode is armed; self-tail (in-place rebind) takes precedence
// over release when a define qualifies for both — the only cost is forgoing the
// once-per-invocation release of a base-case general tail call, negligible vs. the
// per-iteration in-place reuse.
//
//   - frameReuseSelfTail requires BOTH a self-tail-reusable body
//     (validate.BodyIsSelfTailReusable: no capture/escape, non-variadic, no in-body
//     set! of the name, a depth-0 tail self call) AND a Stable binding — the op
//     hardcodes a jump to pc=0, sound only if the name can never be rebound. A
//     top-level define is Stable only under WithImmutableTopLevel; an internal
//     define resolves to a non-Stable local here and is excluded (also sound — a
//     sibling could set! it, which the in-body predicate cannot see).
//   - frameReuseRelease needs NO IsStable() check: OpReleaseEnvFrame does a normal
//     apply (re-resolving the callee), so the define's own rebindability is
//     irrelevant; only the body's capture/escape/callee-safety matters, which
//     validate.BodyIsFrameReleasable checks (callee stability enforced there).
func (p *CompileTimeContinuation) frameReuseForDefine(v *validate.ValidatedDefine) frameReuse {
	name := v.Name()
	binding := p.env.GetBinding(name.Sym, name.Scopes())
	if binding != nil && binding.IsStable() && validate.BodyIsSelfTailReusable(v, name.Sym.Key, p.env) {
		return selfTailReuse(name.Sym.Key, len(v.Params().Required))
	}
	if validate.BodyIsFrameReleasable(v, name.Sym.Key, p.env) {
		return releaseReuse()
	}
	return noFrameReuse()
}

// CompileValidatedLambda compiles a validated (lambda params body...) form.
func (p *CompileTimeContinuation) CompileValidatedLambda(ctctx CompileTimeCallContext, v *validate.ValidatedLambda) error {
	// Create local environment and template for lambda body.
	// compileClosure creates the child environment frame after binding parameters.
	lenv := environment.NewLocalEnvironment(0)
	tpl := machine.NewNativeTemplate(0, 0, false)

	// Anonymous lambdas have no self name to recurse on — no frame-reuse context.
	err := p.compileClosure(ctctx, tpl, lenv, v, noFrameReuse())
	if err != nil {
		return err
	}
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
func (p *CompileTimeContinuation) CompileValidatedCaseLambda(ctctx CompileTimeCallContext, v *validate.ValidatedCaseLambda) error {
	// Phase 1: Compile each clause as a separate closure.
	// Unlike regular lambda which produces one closure, case-lambda produces
	// multiple closures (one per clause) that are combined into a dispatch structure.
	// Each clause closure is pushed to the eval stack for later combination.
	for _, clause := range v.Clauses() {
		lenv := environment.NewLocalEnvironment(0)
		tpl := machine.NewNativeTemplate(0, 0, false)

		// case-lambda clauses are anonymous arity dispatch — no frame-reuse context.
		tpli, envi, err := p.compileClosureBody(ctctx, tpl, lenv, clause, "case-lambda clause", noFrameReuse())
		if err != nil {
			return err
		}

		p.AppendOperations(
			machine.NewOperationLoadLiteralByLiteralIndexImmediate(tpli),
			machine.NewOperationPush(),
			machine.NewOperationLoadLiteralByLiteralIndexImmediate(envi),
			machine.NewOperationPush(),
			machine.NewOperationMakeClosure(),
			machine.NewOperationPush(),
		)
	}

	// Phase 2: Combine all clause closures into a single case-lambda dispatch structure.
	// MakeCaseLambdaClosure pops N closures from the stack (in reverse order) and
	// creates a CaseLambdaClosure that, when called, dispatches to the appropriate
	// clause based on argument count. Clauses are tried in order; first match wins.
	p.AppendOperations(
		machine.NewOperationMakeCaseLambdaClosure(len(v.Clauses())),
	)

	return nil
}

// CompileValidatedSetBang compiles a validated (set! name expr) form.
func (p *CompileTimeContinuation) CompileValidatedSetBang(ctctx CompileTimeCallContext, v *validate.ValidatedSetBang) error {
	// Get the symbol (validator guarantees it's a SyntaxSymbol)
	sym := v.Name.Sym
	symbolScopes := v.Name.Scopes()

	// Compile the value expression
	err := p.compileValidated(ctctx.NotInTail(), v.SubExp())
	if err != nil {
		return err
	}
	p.AppendOperations(machine.NewOperationPush())

	// Use scope-aware binding resolution for validation
	binding := p.env.GetBinding(sym, symbolScopes)
	if binding == nil {
		return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "no such binding %q with compatible scopes for set!", sym.Key)
	}

	// R7RS §5.2: reject set! on imported bindings. By default this is the only
	// binding-level set! restriction — a program-defined top-level variable is
	// mutable (R7RS §4.1.6), "mutable unless imported."
	if binding.IsImported() {
		return werr.WrapForeignErrorf(
			werr.ErrImmutableBinding,
			"set!: cannot mutate imported binding %q",
			sym.Key,
		)
	}

	// Opt-in (WithImmutableTopLevel): also reject set! on a rebind-stable
	// top-level binding. set! requires its target already bound (above), and a
	// binding's Stable is finalized at its define — which necessarily precedes
	// any set! referencing it — so this gate always sees the final Stable, and
	// no runtime trap is needed. A documented deviation from R7RS §4.1.6, off by
	// default. The imported clause already covers imports; this adds the
	// proven-stable program-defined case.
	ns := p.env.Namespace()
	if ns != nil && ns.ImmutableTopLevel() && binding.IsStable() {
		return werr.WrapForeignErrorf(
			werr.ErrImmutableBinding,
			"set!: cannot mutate immutable top-level binding %q",
			sym.Key,
		)
	}

	// Check if it's a local binding
	// M1 fix: Use scope-aware lookup when symbol has scopes (matches CompileSymbol pattern)
	var li *environment.LocalIndex
	if len(symbolScopes) > 0 {
		// Symbol has scopes (from macro expansion), use scope-aware lookup
		li = p.env.GetLocalIndex(sym, symbolScopes)
	} else {
		// Fast path: see CompileSymbol invariant — empty scopes implies no locals in scope.
		li = p.env.GetLocalIndex(sym, nil)
	}

	if li != nil {
		p.AppendOperations(
			machine.NewOperationStoreLocalByLocalIndexImmediate(li),
			machine.NewOperationLoadVoid(),
		)
	} else {
		// Must be global
		gi := p.env.GetGlobalIndex(sym)
		if gi == nil {
			return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "internal error: binding found but no index for %q", sym.Key)
		}
		liti := p.template.MaybeAppendLiteral(gi)
		p.AppendOperations(
			machine.NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(liti),
			machine.NewOperationLoadVoid(),
		)
	}

	return nil
}

// CompileValidatedQuote compiles a validated (quote datum) form.
func (p *CompileTimeContinuation) CompileValidatedQuote(_ CompileTimeCallContext, v *validate.ValidatedQuote) error {
	// Validate quoted literal for circular datum labels.
	unwrapped := v.Datum.UnwrapAll()
	validated, err := p.validateQuotedLiteral(unwrapped)
	if err != nil {
		return err
	}
	// R7RS §4.1.2 makes quoted-literal pairs and vectors immutable. Mark them
	// in the engine-scoped side-set before interning so the list/vector
	// mutators can reject set-car! etc. on this datum.
	ns := p.env.Namespace()
	if ns != nil {
		markLiteralImmutable(validated, ns.ImmutableLiterals(), make(map[values.Value]struct{}))
	}
	litIdx := p.template.MaybeAppendLiteral(validated)
	p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
	return nil
}

// markLiteralImmutable recursively records every pair and vector reachable from
// v in the engine-scoped immutable-literal set. R7RS §4.1.2 makes quoted-literal
// constants immutable; structure sharing (per-template equal? dedup) means one
// mark on the canonical instance covers all sharing siblings. The visited map
// makes cyclic literals (#0=(1 . #0#)) terminate.
func markLiteralImmutable(v values.Value, set *environment.ImmutableLiterals, visited map[values.Value]struct{}) {
	if set == nil {
		return
	}
	switch obj := v.(type) {
	case *values.Pair:
		_, seen := visited[obj]
		if seen {
			return
		}
		visited[obj] = struct{}{}
		set.Mark(obj)
		markLiteralImmutable(obj.Car(), set, visited)
		markLiteralImmutable(obj.Cdr(), set, visited)
	case *values.Vector:
		_, seen := visited[obj]
		if seen {
			return
		}
		visited[obj] = struct{}{}
		set.Mark(obj)
		for _, elem := range *obj {
			markLiteralImmutable(elem, set, visited)
		}
	case *values.ByteVector:
		// Elements are *Byte leaves (no nested aggregates), so mark the
		// bytevector itself without recursing. R7RS §4.1.2.
		set.Mark(obj)
	}
}

// CompileValidatedQuasiquote compiles a validated (quasiquote template) form.
// Quasiquote has complex runtime semantics, so we delegate to the existing compiler.
func (p *CompileTimeContinuation) CompileValidatedQuasiquote(ctctx CompileTimeCallContext, v *validate.ValidatedQuasiquote) error {
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
func (p *CompileTimeContinuation) CompileValidatedBegin(ctctx CompileTimeCallContext, v *validate.ValidatedBegin) error {
	// R7RS §5.3.2: Internal definitions use letrec* semantics
	// Pass 1: Pre-declare all define bindings so forward references work
	for _, expr := range v.Body() {
		p.predeclareDefineBindingFromValidated(expr)
	}

	// Pass 2: Compile each expression in sequence
	return p.compileValidatedSequence(ctctx, v.Body())
}

// compileValidatedCall compiles a validated function call (proc args...).
//
// Direct-style compilation (Dybvig 1987, Ch. 3). The compiler emits stack
// operations for intermediate values instead of CPS or A-normal form.
//
//	(f x y) compiles to:
//	  [if !tail: SaveContinuation]  — push σ onto K
//	  <compile f>   Push            — S = [f]
//	  <compile x>   Push            — S = [f, x]
//	  <compile y>   Push            — S = [f, x, y]
//	  Pull                          — value = f, S = [x, y]
//	  Apply                         — call f with args from S
//
//	where tail = ctctx.inTail (tracked by value through recursive calls).
//
//	Invariant: tail calls emit no SaveContinuation. K does not grow.
//	  This is what makes proper tail recursion work (Clinger 1998).
//	Constrains: CESK transitions (non-tail pushes σ onto K, tail reuses K),
//	  peephole optimizer (may fuse Pull+Apply → PullApply).
//	Constrained by: CompileTimeCallContext.inTail (set by compileBody
//	  for the last expression in a sequence).
//
// See BIBLIOGRAPHY.md "Direct-Style Compilation".
// emitProcAndArgs compiles the procedure expression then each argument,
// pushing the procedure and every argument onto the eval stack in order.
// Shared by the validated-call and validated-apply emit paths; the apply path
// flattens its final list onto the stack after this returns.
func (p *CompileTimeContinuation) emitProcAndArgs(ctctx CompileTimeCallContext, proc validate.ValidatedExpr, args []validate.ValidatedExpr) error {
	err := p.compileValidated(ctctx.NotInTail(), proc)
	if err != nil {
		return err
	}
	p.AppendOperations(machine.NewOperationPush())

	for _, arg := range args {
		err = p.compileValidated(ctctx.NotInTail(), arg)
		if err != nil {
			return err
		}
		p.AppendOperations(machine.NewOperationPush())
	}
	return nil
}

// tryEmitSelfTailCall emits OpSelfTailCall for a depth-0 tail call to the
// enclosing self-tail-reusable closure, returning (true, nil) if it did so.
//
// The selfTail context is armed only on a closure body proven safe (and, for a
// top-level self, Stable) and is cleared on every let descent and dropped by
// NotInTail(), so a set selfTail at an inTail call site whose operator name and
// arity match the enclosing closure is exactly a DEPTH-0 tail self call. The
// emit evaluates the arguments (non-tail, pushing each onto the eval stack in
// slot order — old parameter values stay intact, making the op's drain-and-bind a
// parallel assignment), then the op rebinds the parameter slots and loops to pc=0.
func (p *CompileTimeContinuation) tryEmitSelfTailCall(ctctx CompileTimeCallContext, v *validate.ValidatedCall) (bool, error) {
	if !ctctx.inTail || ctctx.frameReuse.kind != frameReuseSelfTail {
		return false, nil
	}
	sym, ok := v.Proc().(*validate.ValidatedSymbol)
	if !ok {
		return false, nil
	}
	if sym.Symbol.Sym.Key != ctctx.frameReuse.name || len(v.Body()) != ctctx.frameReuse.arity {
		return false, nil
	}
	for _, arg := range v.Body() {
		err := p.compileValidated(ctctx.NotInTail(), arg)
		if err != nil {
			return false, err
		}
		p.AppendOperations(machine.NewOperationPush())
	}
	p.AppendOperations(machine.NewOperationSelfTailCall(ctctx.frameReuse.arity))
	return true, nil
}

func (p *CompileTimeContinuation) compileValidatedCall(ctctx CompileTimeCallContext, v *validate.ValidatedCall) error {
	emitted, err := p.tryEmitSelfTailCall(ctctx, v)
	if err != nil {
		return err
	}
	if emitted {
		return nil
	}

	inlined, err := p.tryInlineCall(ctctx, v)
	if err != nil {
		return err
	}
	if inlined {
		return nil
	}

	var operationSaveContinuationIndex int
	if !ctctx.inTail {
		// Non-tail call: save continuation so we can return here after the call
		operationSaveContinuationIndex = p.emitPatchableSaveContinuation()
	}
	// Tail call: skip SaveContinuation - the callee will return directly to our caller

	// Compile the procedure and arguments, pushing each onto the stack
	err = p.emitProcAndArgs(ctctx, v.Proc(), v.Body())
	if err != nil {
		return err
	}

	// Reclaimable general tail call: the parameter frame is dead now (proc + args
	// are on the eval stack), and the enclosing body was proven frame-releasable
	// (no capture/escape, only capture-safe callees), so release it to the pool
	// before applying — the callee's acquire reuses it. Emitted after the args
	// (which still read the frame) and before Pull+Apply. The frameReuseRelease
	// disposition is depth-0 (cleared on let descent), so p.env is the parameter
	// frame. A frameReuseSelfTail body takes the OpSelfTailCall path above instead.
	if ctctx.inTail && ctctx.frameReuse.kind == frameReuseRelease {
		p.AppendOperations(machine.NewOperationReleaseEnvFrame())
	}

	// Pull the procedure and apply
	p.AppendOperations(
		machine.NewOperationPull(),
		machine.NewOperationApply(),
	)

	if !ctctx.inTail {
		p.patchSaveContinuationOffset(operationSaveContinuationIndex)
	}

	return nil
}

// tryInlineCall checks whether a call can be inlined by substituting a
// let-bound lambda's body at the call site. Returns (true, nil) if the
// call was inlined, (false, nil) if inlining does not apply, or
// (false, err) on compilation failure.
func (p *CompileTimeContinuation) tryInlineCall(
	ctctx CompileTimeCallContext,
	v *validate.ValidatedCall,
) (bool, error) {
	if p.inlineCandidates == nil {
		return false, nil
	}

	sym, ok := v.Proc().(*validate.ValidatedSymbol)
	if !ok {
		return false, nil
	}

	bid, resolved := p.env.ResolveBindingID(sym.Symbol.Sym, sym.Symbol.Scopes())
	if !resolved {
		return false, nil
	}

	candidate, found := p.inlineCandidates[bid]
	if !found {
		return false, nil
	}

	// Only inline when the call site is in the same scope as the
	// registration site. Nested scopes (e.g., let* with duplicate names)
	// may shadow free variables that the lambda captured at definition.
	if p.env != candidate.env {
		return false, nil
	}

	// Guard against recursive inlining.
	if p.currentlyInlining != nil {
		_, inlining := p.currentlyInlining[bid]
		if inlining {
			return false, nil
		}
	}

	// Arity check: argument count must match parameter count exactly.
	// The binding is !Mutable (no set!) so the lambda's parameter count is
	// known at compile time. An arity mismatch is a guaranteed runtime error;
	// report it now instead of deferring to the VM.
	params := candidate.lambda.Params()
	if len(v.Body()) != len(params.Required) {
		return false, werr.WrapForeignErrorf(
			werr.ErrWrongNumberOfArguments,
			"inline call to %s: expected %d argument(s), got %d",
			sym.Symbol.Sym, len(params.Required), len(v.Body()),
		)
	}

	// Build synthetic let bindings: each parameter bound to the corresponding argument.
	// Mark Escapes=true to prevent registerInlineCandidates from treating these
	// synthetic bindings as inline candidates — their Mutable/Escapes flags have
	// not been computed by the validator.
	syntheticBindings := make([]validate.ValidatedLetBinding, len(params.Required))
	for i, param := range params.Required {
		syntheticBindings[i] = validate.ValidatedLetBinding{
			Name:    param,
			Init:    v.Body()[i],
			Escapes: true,
		}
	}

	syntheticLet := validate.NewValidatedLet(
		"let",
		v.Source(),
		validate.LetKindLet,
		syntheticBindings,
		candidate.lambda.Body(),
	)

	// Set recursion guard.
	if p.currentlyInlining == nil {
		p.currentlyInlining = make(map[environment.BindingID]struct{})
	}
	p.currentlyInlining[bid] = struct{}{}
	defer func() {
		delete(p.currentlyInlining, bid)
	}()

	return true, p.CompileValidatedLet(ctctx, syntheticLet)
}

// compileValidatedLiteral handles self-evaluating values (numbers, strings, booleans, etc.).
// Passthrough forms (define-syntax, syntax-case, etc.) are handled by registerSyntaxCompiler
// in register.go via the forms registry, so they never reach here.
func (p *CompileTimeContinuation) compileValidatedLiteral(ctctx CompileTimeCallContext, v *validate.ValidatedLiteral) error {
	return p.CompileSelfEvaluating(ctctx, v.Value)
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
func (p *CompileTimeContinuation) CompileValidatedDynamicWind(ctctx CompileTimeCallContext, v *validate.ValidatedDynamicWind) error {
	// Phase 1: Compile and push before, thunk, after to stack
	// Note: We compile in expression context (not tail) since we need all three values
	exprCtx := ctctx.NotInTail()

	err := p.compileValidated(exprCtx, v.Before)
	if err != nil {
		return err
	}
	p.AppendOperations(machine.NewOperationPush())

	err = p.compileValidated(exprCtx, v.Thunk)
	if err != nil {
		return err
	}
	p.AppendOperations(machine.NewOperationPush())

	err = p.compileValidated(exprCtx, v.After)
	if err != nil {
		return err
	}
	p.AppendOperations(machine.NewOperationPush())
	// Stack: [before, thunk, after]

	// Phase 2: Call before thunk
	// Get before into value register (at depth 2)
	p.AppendOperations(machine.NewOperationPeekK(2))
	// Save continuation to return here after call
	beforeCallReturnIndex := p.emitPatchableSaveContinuation()
	// Apply with 0 args (stack is fresh after SaveContinuation)
	p.AppendOperations(machine.NewOperationApply())
	p.patchSaveContinuationOffset(beforeCallReturnIndex)
	// after_before: Stack is restored to [before, thunk, after]

	// Phase 3: Push winding frame
	p.AppendOperations(machine.NewOperationPushWind())

	// Phase 4: Call thunk
	// Get thunk into value register (at depth 1)
	p.AppendOperations(machine.NewOperationPeekK(1))
	thunkCallReturnIndex := p.emitPatchableSaveContinuation()
	p.AppendOperations(machine.NewOperationApply())
	p.patchSaveContinuationOffset(thunkCallReturnIndex)
	// after_thunk: Stack is restored to [before, thunk, after]
	// Thunk's result is in value register

	// Save thunk result on stack
	p.AppendOperations(machine.NewOperationPush())
	// Stack: [before, thunk, after, result]

	// Phase 5: Pop winding frame
	p.AppendOperations(machine.NewOperationPopWind())

	// Phase 6: Call after thunk
	// Get after into value register (at depth 1 because result is at top)
	p.AppendOperations(machine.NewOperationPeekK(1))
	afterCallReturnIndex := p.emitPatchableSaveContinuation()
	p.AppendOperations(machine.NewOperationApply())
	p.patchSaveContinuationOffset(afterCallReturnIndex)
	// after_after: Stack is restored to [before, thunk, after, result]

	// Phase 7: Return thunk result
	// Get result into value register (at top of stack)
	p.AppendOperations(machine.NewOperationPeekK(0))
	// Clean up stack
	p.AppendOperations(
		machine.NewOperationDrop(), // result
		machine.NewOperationDrop(), // after
		machine.NewOperationDrop(), // thunk
		machine.NewOperationDrop(), // before
	)

	return nil
}

// CompileValidatedApply compiles a validated (apply proc arg1 ... args) form.
//
// R7RS §6.10: apply calls proc with the arguments arg1 ... concatenated
// with the elements of args (the final argument, which must be a list).
//
// Bytecode (non-tail):
//
//	SaveContinuation →after
//	<compile proc>          PUSH
//	<compile arg1>          PUSH
//	...
//	<compile argN>          PUSH
//	<compile finalList>              ; value = finalList
//	OpUnpackListToStack              ; stack: [proc, arg1, ..., argN, x1, x2, ...]
//	Pull                             ; value = proc
//	Apply                            ; calls proc(arg1, ..., argN, x1, x2, ...)
//	after:
//
// Tail position: same without SaveContinuation/patch.
func (p *CompileTimeContinuation) CompileValidatedApply(ctctx CompileTimeCallContext, v *validate.ValidatedApply) error {
	var saveContinuationIndex int
	if !ctctx.inTail {
		saveContinuationIndex = p.emitPatchableSaveContinuation()
	}

	// Compile proc and prefix args, pushing each onto the stack
	err := p.emitProcAndArgs(ctctx, v.Proc, v.PrefixArgs)
	if err != nil {
		return err
	}

	// Compile final list (stays in value register)
	err = p.compileValidated(ctctx.NotInTail(), v.FinalList)
	if err != nil {
		return err
	}

	// Flatten the list onto the eval stack
	p.AppendOperations(machine.NewOperationUnpackListToStack())

	// Pull proc from bottom of stack, then apply
	p.AppendOperations(
		machine.NewOperationPull(),
		machine.NewOperationApply(),
	)

	if !ctctx.inTail {
		p.patchSaveContinuationOffset(saveContinuationIndex)
	}

	return nil
}

// CompileValidatedWithContinuationMark compiles (with-continuation-mark key val body).
//
// Tail position:
//
//	<compile key> PUSH
//	<compile val>
//	SetContMark               ; pops key, sets marks[key] = val
//	<compile body in tail>
//
// Non-tail position:
//
//	<compile key> PUSH
//	<compile val>
//	SaveContMark              ; pops key, saves (key, old) on stack, sets mark
//	<compile body in non-tail>
//	RestoreContMark           ; pops (old, key), restores mark
func (p *CompileTimeContinuation) CompileValidatedWithContinuationMark(
	ctctx CompileTimeCallContext,
	v *validate.ValidatedWithContinuationMark,
) error {
	exprCtx := ctctx.NotInTail()

	// Compile key expression
	err := p.compileValidated(exprCtx, v.Key)
	if err != nil {
		return err
	}
	p.AppendOperations(machine.NewOperationPush())

	// Compile val expression
	err = p.compileValidated(exprCtx, v.Val)
	if err != nil {
		return err
	}

	if ctctx.inTail {
		// Tail position: set mark, compile body in tail, no restore
		p.AppendOperations(machine.NewOperationSetContMark())
		return p.compileValidated(ctctx, v.Body)
	}

	// Non-tail position: save+set, body, restore
	p.AppendOperations(machine.NewOperationSaveContMark())
	err = p.compileValidated(exprCtx, v.Body)
	if err != nil {
		return err
	}
	p.AppendOperations(machine.NewOperationRestoreContMark())
	return nil
}
