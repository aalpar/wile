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
	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// compile_validated.go handles compilation of core Scheme forms that go through
// upfront validation (validate.Validate) before codegen. These are the fixed
// R7RS core forms: if, lambda, define, set!, quote, quasiquote, begin,
// case-lambda, dynamic-wind, apply, with-continuation-mark, let, let*,
// letrec, letrec* (binding forms compiled in compile_let.go).
//
// Extension forms (define-syntax, import, include, syntax-case, etc.) pass through
// validation as ValidatedLiteral and are compiled via the syntaxCompiler adapter
// in register.go, whose init() registers each entry with forms.RegisterCompiler.
//
// Decision criteria: if a form has fixed syntax that can be validated once before
// compilation, it goes through the validated path. If a form's syntax is
// extensible or defined by the extension system itself, it uses the registry path.

// compilerFor returns the per-engine CompilerFunc for a form name, or nil. This
// is the any → CompilerFunc dispatch assertion site; a mis-typed Compile becomes
// nil here and is surfaced by VerifyCompilers's checked assertion (test-only).
func (p *CompileTimeContinuation) compilerFor(name string) CompilerFunc {
	spec := forms.RegistryFor(p.env).Lookup(name)
	if spec == nil {
		return nil
	}
	compile, _ := spec.Compile.(CompilerFunc)
	return compile
}

// noCompilerDiagnostic reproduces the two legacy diagnostics: a registered form
// with no compiler should have been handled during expansion (let-syntax,
// syntax-rules, …); an unregistered name is an unknown validated expression type.
func (p *CompileTimeContinuation) noCompilerDiagnostic(expr validate.ValidatedExpr) error {
	name := expr.FormName()
	if forms.RegistryFor(p.env).Lookup(name) != nil {
		return werr.WrapForeignErrorf(werr.ErrInvalidSyntax,
			"compile: form %q has no compiler (should be handled during expansion)", name)
	}
	return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
		"unknown validated expression type: %T", expr)
}

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

// compileValidated dispatches compilation of a single validated expression.
// Grammar-head cases (ValidatedCall, ValidatedSymbol, ValidatedLiteral) are
// matched by concrete type; everything else — all named forms — routes through
// the per-engine forms registry by FormName.
func (p *CompileTimeContinuation) compileValidated(ctctx CompileTimeCallContext, expr validate.ValidatedExpr) error {
	// Push the validated expression's source for finer-grained attribution.
	src := expr.Source()
	if src != nil {
		p.pushSource(src)
		defer p.popSource()
	}

	// Grammar head: the hottest, never-dialect-reconfigured nodes dispatch by
	// concrete type so the common path pays no map lookup. Everything named as a
	// form routes through the per-engine registry by FormName.
	switch v := expr.(type) {
	case *validate.ValidatedCall:
		return p.compileValidatedCall(ctctx, v)
	case *validate.ValidatedSymbol:
		return p.CompileSymbol(ctctx, v.Symbol)
	case *validate.ValidatedLiteral:
		// A bare self-eval literal has no compiler and self-evaluates. A Tier-2
		// passthrough form (cond-expand, include, import) is the SAME Go type but
		// carries a FormName a compiler claims; it must be compiled, never folded.
		// isSelfEvaluatingLiteral is the single predicate shared with the if-fold
		// (isLiteralFalse), so the two cannot disagree about what a literal is.
		if p.isSelfEvaluatingLiteral(v) {
			return p.compileValidatedLiteral(ctctx, v)
		}
		compile := p.compilerFor(v.FormName())
		if compile != nil {
			return compile(p, ctctx, v)
		}
		// A registered form with no compiler (let-syntax, letrec-syntax,
		// syntax-rules) should have been consumed during expansion; reaching
		// codegen is an expander bug. Diagnose it rather than silently
		// self-evaluating the raw form, which would yield a wrong value.
		return p.noCompilerDiagnostic(v)
	default:
		// Tier-1 forms (if, lambda, define, set!, quote, begin, apply, let, …),
		// name-keyed. let/let*/letrec/letrec* all resolve to CompileValidatedLet.
		compile := p.compilerFor(expr.FormName())
		if compile == nil {
			return p.noCompilerDiagnostic(expr)
		}
		return compile(p, ctctx, expr)
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
func CompileValidatedIf(p *CompileTimeContinuation, ctctx CompileTimeCallContext, expr forms.ValidatedExpr) error {
	v := expr.(*validate.ValidatedIf)
	// Constant folding: if the test is a compile-time-known literal, fold the
	// if form to just the consequent or alternative. Per R7RS, only #f is false;
	// all other values (including 0, "", '()) are truthy.
	folded, isFalsy := p.isLiteralFalse(v.Test)
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

// isSelfEvaluatingLiteral reports whether a ValidatedLiteral is a bare
// self-evaluating datum rather than a Tier-2 passthrough form.
//
// *validate.ValidatedLiteral carries BOTH: real literals (5, #f, "s") and
// passthrough forms (cond-expand, include, import), whose FormName is
// overwritten with the keyword during validation. The only thing that separates
// them is whether a compiler claims the FormName — which is exactly the question
// compileValidated asks at dispatch. Asking the identical question here is what
// keeps the fold and the dispatcher from ever disagreeing again.
func (p *CompileTimeContinuation) isSelfEvaluatingLiteral(lit *validate.ValidatedLiteral) bool {
	if p.compilerFor(lit.FormName()) != nil {
		return false
	}
	return forms.RegistryFor(p.env).Lookup(lit.FormName()) == nil
}

// isLiteralFalse reports whether expr is a compile-time-known self-evaluating
// literal, and whether it is #f. Returns (true, true) for #f, (true, false) for
// any other self-evaluating literal (truthy per R7RS), and (false, false) for
// anything the compiler must actually emit — including Tier-2 passthrough forms,
// which are *ValidatedLiteral but are NOT constants.
func (p *CompileTimeContinuation) isLiteralFalse(expr validate.ValidatedExpr) (isLiteral, isFalse bool) {
	lit, ok := expr.(*validate.ValidatedLiteral)
	if !ok {
		return false, false
	}
	if !p.isSelfEvaluatingLiteral(lit) {
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
		// Non-boolean self-evaluating literal — truthy per R7RS.
		return true, false
	}
	return true, !b.Value
}

// CompileValidatedLambda compiles a validated (lambda params body...) form.
func CompileValidatedLambda(p *CompileTimeContinuation, ctctx CompileTimeCallContext, expr forms.ValidatedExpr) error {
	v := expr.(*validate.ValidatedLambda)
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
func CompileValidatedCaseLambda(p *CompileTimeContinuation, ctctx CompileTimeCallContext, expr forms.ValidatedExpr) error {
	v := expr.(*validate.ValidatedCaseLambda)
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
func CompileValidatedSetBang(p *CompileTimeContinuation, ctctx CompileTimeCallContext, expr forms.ValidatedExpr) error {
	v := expr.(*validate.ValidatedSetBang)
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
	binding := p.env.GetBinding(sym, syntax.ScopesOf(symbolScopes))
	if binding == nil {
		// The set! half of the same defect: stamp the identifier being
		// assigned, so the reported location is the offending name rather
		// than the enclosing top-level form.
		return wrapSourcedError(v.Name.SourceContext(),
			werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "no such binding %q with compatible scopes for set!", sym.Key))
	}

	// R7RS §5.2: reject set! on imported bindings. Under WithMutableTopLevel()
	// this is the only binding-level set! restriction, "mutable unless imported"
	// (R7RS §4.1.6). Under the engine default (WithImmutableTopLevel) the
	// IsStable() check below also fires, on a defined-once top-level variable.
	if binding.IsImported() {
		return werr.WrapForeignErrorf(
			werr.ErrImmutableBinding,
			"set!: cannot mutate imported binding %q",
			sym.Key,
		)
	}

	// Reject set! on a rebind-stable binding. A Stable binding is a frame-reclaim
	// optimizer anchor: the optimizer may release a caller's frame at a tail call
	// trusting the anchor never becomes a continuation-capturing procedure, so an
	// in-place mutation would be unsound. This keys on IsStable() DIRECTLY, not on
	// the namespace's ImmutableTopLevel() flag, so a Stable anchor copied into a
	// MUTABLE interaction/eval child namespace (e.g. scheme-report-environment, which
	// snapshots the immutable base) stays set!-protected — the child permits
	// define-shadow but never in-place mutation of an anchor. IsStable() is only ever
	// set under immutability (WithStableBasePrimitives / the immTop define stamp), so
	// in a fully-mutable engine nothing is Stable and this never fires (R7RS §4.1.6
	// mutability preserved). set! requires its target already bound (above) and Stable
	// is finalized at the define, so this always sees the final value — no runtime
	// trap needed. The imported clause above already covers imports.
	if binding.IsStable() {
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
		li = p.env.GetLocalIndex(sym, syntax.ScopesOf(symbolScopes))
	} else {
		// Fast path: see CompileSymbol invariant — empty scopes implies no locals in scope.
		li = p.env.GetLocalIndex(sym, syntax.EmptyScopes())
	}

	if li != nil {
		p.AppendOperations(
			machine.NewOperationStoreLocalByLocalIndexImmediate(li),
			machine.NewOperationLoadVoid(),
		)
	} else {
		// Must be global. Scope-aware, matching the local lookup above: once a
		// macro-introduced binder is stored under its intro scope, a wildcard
		// lookup here would resolve to the name's first slot and mutate the
		// user's variable instead.
		gi := p.env.GetGlobalIndexWithScopes(sym, syntax.ScopesOf(symbolScopes))
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
func CompileValidatedQuote(p *CompileTimeContinuation, _ CompileTimeCallContext, expr forms.ValidatedExpr) error {
	v := expr.(*validate.ValidatedQuote)
	// Validate quoted literal for circular datum labels.
	unwrapped := v.Datum.UnwrapAll()
	validated, err := p.validateQuotedLiteral(unwrapped)
	if err != nil {
		return err
	}
	// R7RS §4.1.2 makes quoted-literal pairs and vectors immutable. Mark them
	// in the engine-scoped side-set before interning so the list/vector
	// mutators can reject set-car! etc. on this datum.
	litIdx := p.appendConstantLiteral(validated)
	p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
	return nil
}

// CompileValidatedQuasiquote compiles a validated (quasiquote template) form.
// Quasiquote has complex runtime semantics, so we delegate to the existing compiler.
func CompileValidatedQuasiquote(p *CompileTimeContinuation, ctctx CompileTimeCallContext, expr forms.ValidatedExpr) error {
	v := expr.(*validate.ValidatedQuasiquote)
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
func CompileValidatedBegin(p *CompileTimeContinuation, ctctx CompileTimeCallContext, expr forms.ValidatedExpr) error {
	v := expr.(*validate.ValidatedBegin)
	// R7RS §5.3.2: Internal definitions use letrec* semantics
	// Pass 1: Pre-declare all define bindings so forward references work
	for _, expr := range v.Body() {
		p.predeclareDefineFromValidatedRecursive(expr)
	}

	// Pass 1.5 (Lever A): at the user-program top-level unit only, classify the unit
	// for interprocedural frame reclamation, once. A forward/mutual edge needs the
	// callee's rebind-stability before the callee's body compiles, so
	// ClassifyFrameReclaim reads it from the validation-time StableInUnit carried on
	// each ValidatedDefine (thread-local to this compile) rather than from the shared
	// *Binding. No pre-stamp of the binding is required — which also removes the
	// transient cross-thread Stable window a concurrent compile could observe.
	//
	// The gate is the user mutable-runtime frame (ns.Runtime()==p.env); it
	// structurally excludes macro transformers (expand-phase env), libraries
	// (child-runtime frame), and lambda-body child compilers (p.env is a local
	// frame). The nil guard fires it once per root compiler (a nested top-level begin
	// re-enters on the same p but finds the map already set).
	//
	// The unit's arity table is collected here too, under the same gate and the
	// same once-guard, because it wants exactly the same three conditions: it is
	// only sound where top-level immutability holds, only meaningful for the user
	// unit, and only correct for the whole unit rather than a sub-body. It feeds
	// checkCallArity for callees whose binding exists but whose closure value does
	// not yet. Sharing the guard is safe in the empty case: collectTopLevelDefines
	// recurses into nested begins, so an outer walk that finds no stable define
	// leaves nothing for a re-entering inner walk to find either.
	ns := p.env.Namespace()
	if ns != nil && ns.ImmutableTopLevel() && ns.Runtime() == p.env && p.frameReclaimVerdict == nil {
		p.frameReclaimVerdict = validate.ClassifyFrameReclaim(v.Body(), p.env)
		p.unitArity = validate.UnitArityOf(v.Body())
	}

	// Pass 2: Compile each expression in sequence. The begin's own body is the
	// letrec* group for defines it predeclared above, so it overrides any group
	// inherited from an enclosing body.
	return p.compileValidatedSequence(ctctx.WithEnclosingDefines(v.Body()), v.Body())
}

// compileValidatedLiteral handles self-evaluating values (numbers, strings, booleans, etc.).
// Passthrough forms (define-syntax, syntax-case, etc.) are handled by the
// syntaxCompiler adapter in register.go via the forms registry, so they never
// reach here.
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
//	BOX_VALUES                     ; collapse thunk's 0/1/N result values to one carrier
//	PUSH                           ; save boxed result, Stack: [before, thunk, after, boxed]
//	OP_POP_WIND                    ; pop winding frame
//	PEEK_K 1                       ; value = after
//	SAVE_CONTINUATION →after_after
//	APPLY                          ; call after()
//	after_after:                   ; Stack: [before, thunk, after, boxed]
//	PEEK_K 0                       ; value = boxed result
//	UNBOX_VALUES                   ; expand carrier back to thunk's 0/1/N values
//	DROP DROP DROP DROP            ; clean up stack
//
// BOX_VALUES/UNBOX_VALUES keep the saved result at exactly one eval-stack slot:
// OpPush pushes every value in the register (PushAll for multiple, nothing for
// zero), so without boxing a thunk returning (values …) would mis-align the
// PeekK/Drop offsets (R7RS §6.10 requires dynamic-wind to return the thunk's
// values, including zero or several).
func CompileValidatedDynamicWind(p *CompileTimeContinuation, ctctx CompileTimeCallContext, expr forms.ValidatedExpr) error {
	v := expr.(*validate.ValidatedDynamicWind)
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

	// Box the thunk's result so multiple (or zero) values occupy exactly ONE
	// eval-stack slot. OpPush pushes every value in the register (PushAll for
	// multi, nothing for zero), which would break the fixed [.. , result] layout
	// the PeekK/Drop offsets below assume. Boxing collapses 0/1/N values into a
	// single carrier; OperationUnboxValues restores them after the after-thunk.
	p.AppendOperations(machine.NewOperationBoxValues())
	// Save boxed thunk result on stack
	p.AppendOperations(machine.NewOperationPush())
	// Stack: [before, thunk, after, boxed-result]

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
	// Get boxed result into value register (at top of stack)
	p.AppendOperations(machine.NewOperationPeekK(0))
	// Unbox: expand the carrier back into the register's 0/1/N values.
	p.AppendOperations(machine.NewOperationUnboxValues())
	// Clean up stack
	p.AppendOperations(
		machine.NewOperationDrop(), // boxed result
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
func CompileValidatedApply(p *CompileTimeContinuation, ctctx CompileTimeCallContext, expr forms.ValidatedExpr) error {
	v := expr.(*validate.ValidatedApply)
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
func CompileValidatedWithContinuationMark(p *CompileTimeContinuation, ctctx CompileTimeCallContext, expr forms.ValidatedExpr) error {
	v := expr.(*validate.ValidatedWithContinuationMark)
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
