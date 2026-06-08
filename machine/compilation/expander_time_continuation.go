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

// expander_time_continuation.go implements macro expansion for syntax-rules.
//
// The expander runs after parsing and before compilation. It walks the syntax
// tree looking for macro invocations and expands them to their definitions.
//
// Expansion Process:
//   1. For each expression, check if it's a macro invocation
//   2. If yes, invoke the transformer closure (OperationSyntaxRulesTransform)
//   3. The transformer returns the expanded syntax
//   4. Recursively expand the result (macros can expand to other macro calls)
//   5. Return the fully expanded syntax tree to the compiler
//
// Macro Detection:
//   When ExpandSyntaxExpression sees a symbol, it checks the environment
//   for a binding with BindingTypeSyntax. If found, the binding's value
//   is a machine.MachineClosure (the compiled transformer), which is invoked.
//
// The expander is separate from the compiler because:
//   - Macros must be expanded before compiling (they change the syntax)
//   - Expansion may need to run compiled code (the transformer)
//   - Hygiene scopes are added during expansion, not compilation
//
// Reference: R7RS Section 4.3 (Macros)

import (
	"context"

	"github.com/aalpar/wile/machine"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// DefaultMaxExpandDepth bounds structural recursion depth during macro
// expansion. Without a bound, deeply nested syntax — reachable not from text
// (the parser caps that, see internal/parser DefaultMaxParseDepth) but from
// programmatically-constructed syntax such as macro output, datum->syntax, and
// quasiquote — triggers a fatal, unrecoverable Go stack overflow that kills the
// host process. 0 means unlimited. Mirrors the VM's DefaultMaxCallDepth and the
// parser's DefaultMaxParseDepth.
//
// Chosen empirically: the expander overflows the Go stack between ~400k (heavy
// macro-re-expansion paths) and ~800k (light procedure-call nesting) levels;
// 50000 leaves an order-of-magnitude margin below the crash while sitting far
// above any practical program. A flat recursive macro (and/or/cond with N
// clauses) accumulates expansion depth linearly in N, but such forms are
// O(N^2) to expand and unusable well before 50000 clauses, so the bound does
// not regress any program anyone actually runs. Callers with genuinely deeper
// machine-generated syntax opt out via SetMaxDepth(0) / WithMaxExpandDepth(0).
const DefaultMaxExpandDepth int = 50000

// expandDepthGuard bounds total structural recursion across one expansion run.
//
// A single ExpandExpression tree-walk spawns child ExpanderTimeContinuation
// objects for lambda/let/let-syntax bodies (see newChildExpander). Those child
// frames stay live on the Go stack while the parent recurses, so the bound must
// reflect cumulative Go-stack depth, not per-object depth. The guard is shared
// by pointer across a parent and all its children so depth accumulates
// regardless of how many continuation objects a single run creates. (This is
// why a per-object int — sufficient for the parser, which has one object per
// parse — is not sufficient here.)
type expandDepthGuard struct {
	depth int
	max   int // 0 = unlimited
}

// ExpanderTimeContinuation is a continuation used during the expansion phase.
//
// It walks the syntax tree, detecting and expanding macro invocations.
// The env field provides access to macro definitions (BindingTypeSyntax bindings).
type ExpanderTimeContinuation struct {
	ctx context.Context
	env *environment.EnvironmentFrame
	// libraryScope is set when expanding inside a library body.
	// Threaded to CompileSyntaxRules for cross-library macro hygiene.
	libraryScope *syntax.Scope
	// evaluator abstracts VM execution for transformer invocation
	// so the expander can be tested without the concrete VM.
	evaluator machine.MacroEvaluator
	// depthGuard bounds structural recursion. Shared by pointer with child
	// expanders (newChildExpander) so the whole run shares one counter.
	depthGuard *expandDepthGuard
}

// NewExpanderTimeContinuation creates a new ExpanderTimeContinuation. The
// returned expander begins a fresh expansion run with its own depth guard
// defaulted to DefaultMaxExpandDepth; override via SetMaxDepth.
func NewExpanderTimeContinuation(ctx context.Context, env *environment.EnvironmentFrame, evaluator machine.MacroEvaluator) *ExpanderTimeContinuation {
	q := &ExpanderTimeContinuation{
		ctx:        ctx,
		env:        env,
		evaluator:  evaluator,
		depthGuard: &expandDepthGuard{max: DefaultMaxExpandDepth},
	}
	return q
}

// newChildExpander creates a child expander for a nested body (lambda, let,
// let-syntax, etc.) that shares this expander's depth guard. The child is part
// of the same continuous Go recursion as its parent, so it must accumulate into
// the same depth counter rather than reset to zero.
//
// It is otherwise identical to the expander the call sites previously built with
// NewExpanderTimeContinuation(p.ctx, env, p.evaluator): ctx and evaluator carry
// over, env is the nested-body environment, and libraryScope is deliberately
// left nil (these body sites never propagated it — where library scope must
// flow into a nested expander, the code sets it explicitly, e.g.
// compile_time_continuation_include.go). Sharing only the depth guard keeps the
// hygiene behavior unchanged.
func (p *ExpanderTimeContinuation) newChildExpander(env *environment.EnvironmentFrame) *ExpanderTimeContinuation {
	q := &ExpanderTimeContinuation{
		ctx:        p.ctx,
		env:        env,
		evaluator:  p.evaluator,
		depthGuard: p.depthGuard,
	}
	return q
}

// SetMaxDepth sets the maximum structural recursion depth allowed during
// expansion for this run. A value of 0 (or negative, clamped to 0) disables
// the limit. Mirrors the parser's SetMaxDepth and MachineContext.SetMaxCallDepth.
func (p *ExpanderTimeContinuation) SetMaxDepth(n int) {
	if n < 0 {
		n = 0
	}
	p.depthGuard.max = n
}

// Context returns the context associated with this expander continuation.
func (p *ExpanderTimeContinuation) Context() context.Context {
	return p.ctx
}

// hasLocalVariableBinding delegates to EnvironmentFrame.HasLocalVariableBinding.
// R7RS §4.2.2: let bindings shadow outer bindings including macros.
func (p *ExpanderTimeContinuation) hasLocalVariableBinding(sym *values.Symbol, scopes []*syntax.Scope) bool {
	return p.env.HasLocalVariableBinding(sym, scopes)
}

// ExpandExpression expands a syntax expression.
//
// This is the single recursion chokepoint of the expander: every descent into a
// sub-expression — nested cars, argument lists, primitive-form bodies (via child
// expanders), and macro re-expansion — funnels through here. The depth guard
// therefore bounds total Go-stack recursion for the whole expansion run,
// returning a catchable ErrExpandDepthExceeded instead of letting deeply nested
// (programmatically-constructed) syntax crash the host with a fatal Go stack
// overflow. See expandDepthGuard and DefaultMaxExpandDepth.
func (p *ExpanderTimeContinuation) ExpandExpression(expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	// Context cancellation takes precedence over the depth bound: an explicitly
	// cancelled run reports ctx.Err(), not ErrExpandDepthExceeded, even when both
	// conditions are live at this entry. Checked before touching the depth
	// counter so the increment/decrement stays symmetric on the cancel path.
	select {
	case <-p.ctx.Done():
		return nil, p.ctx.Err()
	default:
	}
	g := p.depthGuard
	g.depth++
	defer func() {
		g.depth--
	}()
	if g.max > 0 && g.depth > g.max {
		return nil, wrapSourcedError(expr.SourceContext(),
			werr.WrapForeignErrorf(werr.ErrExpandDepthExceeded,
				"expand: nesting depth exceeds maximum of %d", g.max))
	}
	var result syntax.SyntaxValue
	var err error
	if syntax.IsSyntaxEmptyList(expr) {
		return expr, nil
	}
	switch stx := expr.(type) {
	case *syntax.SyntaxPair:
		car := stx.SyntaxCar()
		cdr := stx.SyntaxCdr()
		result, err = p.ExpandSyntaxOrProcedureCall(car, cdr)
		if err != nil {
			return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(err, "expand: failed to expand list expression"))
		}
		return result, nil
	case *syntax.SyntaxSymbol:
		return p.ExpandSymbol(stx)
	case *syntax.SyntaxObject:
		// Self-evaluating value (integer, boolean, string, etc.)
		return stx, nil
	}
	return p.ExpandSelfEvaluating(expr)
}

// ExpandSymbol handles a symbol expression.
func (p *ExpanderTimeContinuation) ExpandSymbol(expr *syntax.SyntaxSymbol) (syntax.SyntaxValue, error) {
	return expr, nil
}

// ExpandSyntaxOrProcedureCall handles a list expression. The car may be a
// symbol (possibly a macro), a nested pair (computed procedure), or a
// self-evaluating value (like in quoted data or malformed expressions).
func (p *ExpanderTimeContinuation) ExpandSyntaxOrProcedureCall(car syntax.SyntaxValue, cdr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	switch v := car.(type) {
	case *syntax.SyntaxPair:
		// Car is a pair - expand it (computed procedure), then expand arguments
		newCar, err := p.ExpandExpression(v)
		if err != nil {
			return nil, wrapSourcedError(car.SourceContext(), werr.WrapForeignErrorf(err, "failed to expand car expression"))
		}
		rest1, err := p.ExpandSyntaxArgumentList(cdr)
		if err != nil {
			return nil, wrapSourcedError(car.SourceContext(), werr.WrapForeignErrorf(err, "failed to expand argument list"))
		}
		return syntax.NewSyntaxCons(newCar, rest1, newCar.SourceContext()), nil
	case *syntax.SyntaxSymbol:
		// Car is a symbol - check if it's a macro, expand arguments either way
		return p.ExpandSyntaxExpression(v, cdr)
	case *syntax.SyntaxObject:
		// Car is a self-evaluating value - just expand arguments
		rest1, err := p.ExpandSyntaxArgumentList(cdr)
		if err != nil {
			return nil, wrapSourcedError(car.SourceContext(), werr.WrapForeignErrorf(err, "failed to expand argument list"))
		}
		return syntax.NewSyntaxCons(car, rest1, car.SourceContext()), nil
	default:
		// Unknown car type - return expression unchanged
		return syntax.NewSyntaxCons(car, cdr, car.SourceContext()), nil
	}
}

// ExpandSelfEvaluating handles self-evaluating expressions.
func (p *ExpanderTimeContinuation) ExpandSelfEvaluating(expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return expr, nil
}

// ExpandPrimitiveForm handles expansion within primitive forms like if, begin,
// lambda, define, etc. Some primitives need their subexpressions expanded
// (like if, begin) while others should be left unchanged (like quote, define-syntax).
//
// This function looks up the primitive expander in the expand environment registry.
// If found, it invokes the expander; otherwise returns the form unchanged.
func (p *ExpanderTimeContinuation) ExpandPrimitiveForm(primName string, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	// Look up the primitive expander in the registry
	symVal := values.NewSymbol(primName)
	scopes := sym.Scopes()

	pe := LookupPrimitiveExpander(p.env, symVal, scopes)
	if pe != nil {
		return pe.Expand(p, sym, expr)
	}
	// Unknown primitive - return unchanged (safe default)
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// ExpandSyntaxExpression checks if sym is a macro and expands it, or returns
// the expression as a procedure call if not.
//
// This is where macro invocation happens:
//  1. Look up the symbol in the expand environment
//  2. If bound with BindingTypeSyntax, it's a macro - invoke the transformer
//  3. If it's a primitive (like quote, if, define-syntax), don't expand args
//  4. Otherwise, treat as procedure call and expand arguments
//
// The transformer closure (machine.MachineClosure from CompileSyntaxRules) is invoked
// by creating a machine.MachineContext and running it. The transformer:
//   - Receives the full macro invocation form on the eval stack
//   - Pattern matches against its clauses (OperationSyntaxRulesTransform)
//   - Expands the matching template with captured bindings
//   - Adds an intro scope to the expansion for hygiene
//   - Returns the expanded syntax in the value register
//
// The expanded result may itself contain macro invocations, so the caller
// should recursively expand it.
func (p *ExpanderTimeContinuation) ExpandSyntaxExpression(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	sym0, ok := sym.Unwrap().(*values.Symbol) // Ensure sym is a symbol
	if !ok {
		return nil, wrapSourcedError(sym.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotASymbol, "expected a symbol for syntax, got %T", sym.Unwrap()))
	}

	// R7RS §4.2.2: Local variable bindings shadow macros AND primitive forms
	// Check if there's a local variable binding before checking for macros or primitives
	hasLocalBinding := p.hasLocalVariableBinding(sym0, sym.Scopes())

	if !hasLocalBinding {
		// No local variable shadowing - check for macros
		// First check local bindings in p.env (supports let-syntax local macros)
		// Then fall back to the global expand environment
		var bnd *environment.Binding

		// Check local bindings first (for let-syntax/letrec-syntax)
		bnd = p.env.GetBinding(sym0, nil)

		// If not found locally, check the global expand environment
		if bnd == nil || bnd.BindingType() != environment.BindingTypeSyntax {
			expandEnv := p.env.Expand()
			bnd = expandEnv.GetBinding(sym0, nil)
		}

		// Check if it's a macro binding
		if bnd != nil && bnd.BindingType() == environment.BindingTypeSyntax {
			// This is a macro - invoke the transformer
			return p.expandMacroInvocation(sym, expr, bnd)
		}

		// Library scope macro fallback: if the symbol carries a library scope,
		// check the library env's expand phase for an unexported macro binding.
		// This enables macros that reference unexported helper macros.
		symbolScopes := sym.Scopes()
		if p.env.Namespace() != nil && len(symbolScopes) > 0 {
			for _, scope := range symbolScopes {
				libEnv := p.env.Namespace().LookupLibraryEnv(scope)
				if libEnv == nil {
					continue
				}
				libExpandEnv := libEnv.Expand()
				libBnd := libExpandEnv.GetBinding(sym0, nil)
				if libBnd != nil && libBnd.BindingType() == environment.BindingTypeSyntax {
					return p.expandMacroInvocation(sym, expr, libBnd)
				}
			}
		}

		// Not a macro - check if it's a primitive (quote, if, define-syntax, etc.)
		symVal := sym0
		pe := LookupPrimitiveExpander(p.env, symVal, sym.Scopes())
		if pe != nil {
			return pe.Expand(p, sym, expr)
		}
	}

	// Regular procedure call - expand arguments (they might contain macro calls)
	exprPair, ok := expr.(*syntax.SyntaxPair)
	if ok && !syntax.IsSyntaxEmptyList(exprPair) {
		expandedArgs, err := p.ExpandSyntaxArgumentList(exprPair)
		if err != nil {
			return nil, wrapSourcedError(sym.SourceContext(), werr.WrapForeignErrorf(err, "failed to expand arguments"))
		}
		return syntax.NewSyntaxCons(sym, expandedArgs, sym.SourceContext()), nil
	}
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// invokeTransformerClosure is defined in machine/macro_evaluator.go
// because it accesses machine.MachineContext internals (pool, unexported fields).

// expandMacroInvocation invokes a macro transformer and returns the expanded result.
// This is called when ExpandSyntaxExpression determines that a symbol is bound to a macro.
func (p *ExpanderTimeContinuation) expandMacroInvocation(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue, bnd *environment.Binding) (syntax.SyntaxValue, error) {
	// Check for ER macro transformer first
	erTransformer, isER := bnd.Value().(*ERMacroTransformer)
	if isER {
		return p.expandERMacroInvocation(sym, expr, erTransformer)
	}

	cls, ok := bnd.Value().(machine.Closure)
	if !ok {
		return nil, wrapSourcedError(sym.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotAClosure, "not a closure: %T", bnd.Value()))
	}

	// For syntax-rules transformers, we pass the entire input form as an argument.
	// The transformer expects the full form including the macro name.
	inputForm := syntax.NewSyntaxCons(sym, expr, sym.SourceContext())

	// Set up the expander context so the transformer can access the use-site environment.
	// This is critical for R7RS §4.3.2 auxiliary syntax hygiene: the pattern matcher
	// needs to check if input identifiers have lexical bindings at the use site.
	// For example, in (let ((=> #f)) (cond (#t => 'ok))), the pattern matcher needs
	// to see that => is bound by the lambda (from let expansion) to correctly
	// determine that it shouldn't match the literal => in cond's pattern.
	expanderCtx := NewExpanderContext(p.env, p)

	mc, err := p.evaluator.InvokeTransformer(p.ctx, cls, expanderCtx, inputForm)
	if err != nil {
		return nil, wrapSourcedError(sym.SourceContext(), werr.WrapForeignErrorf(err, "expand: syntax-rules transformer invocation failed"))
	}
	defer machine.ReleaseSubContext(mc)

	// Check if the transformer produced a result
	result := mc.GetValue()
	if values.IsVoid(result) {
		return nil, wrapSourcedError(sym.SourceContext(), werr.WrapForeignErrorf(werr.ErrUnexpectedNil, "syntax transformer produced no result"))
	}

	// For syntax-rules transformers, the result should be the expanded form.
	// The expanded result may itself contain macro invocations (especially for
	// recursive macros like `and`, `or`, `let*`, etc.), so we must recursively
	// expand it.
	stx, ok := result.(syntax.SyntaxValue)
	if ok {
		// Recursively expand the result to handle nested macro calls
		return p.ExpandExpression(stx)
	}
	return nil, wrapSourcedError(sym.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotASyntaxValue, "syntax transformer returned non-syntax value: %T", result))
}

// expandERMacroInvocation handles expansion of explicit-renaming macro invocations.
// It unwraps the input form to raw s-expressions, creates rename/compare closures,
// calls the 3-arg transformer, and re-wraps the result for recursive expansion.
func (p *ExpanderTimeContinuation) expandERMacroInvocation(
	sym *syntax.SyntaxSymbol,
	expr syntax.SyntaxValue,
	erTransformer *ERMacroTransformer,
) (syntax.SyntaxValue, error) {
	wrapped, err := p.invokeERTransformer(sym, expr, erTransformer)
	if err != nil {
		return nil, wrapSourcedError(sym.SourceContext(), werr.WrapForeignErrorf(err, "expand: er-macro-transformer expansion failed"))
	}
	// Recursively expand the result
	return p.ExpandExpression(wrapped)
}

// invokeERTransformer runs the ER transformer and returns the re-wrapped result
// without recursive expansion. Used by both expandERMacroInvocation (which recurses)
// and ExpandOnce (which does not).
func (p *ExpanderTimeContinuation) invokeERTransformer(
	sym *syntax.SyntaxSymbol,
	expr syntax.SyntaxValue,
	erTransformer *ERMacroTransformer,
) (syntax.SyntaxValue, error) {
	// Build complete input form: (macro-name . args)
	inputForm := syntax.NewSyntaxCons(sym, expr, sym.SourceContext())

	// Unwrap to raw s-expression for the transformer
	rawForm := inputForm.UnwrapAll()

	// Create a fresh intro scope for this invocation. Unbound renamed symbols
	// (like temporary names) get this scope to prevent variable capture.
	introScope := syntax.NewScope()

	// Create rename closure (captures definition-site expand env + intro scope)
	renameCls := NewERRenameClosure(erTransformer.DefEnv(), introScope)

	// Create compare closure (captures use-site env)
	compareCls := NewERCompareClosure(p.env)

	// Set up expander context for auxiliary syntax hygiene
	expanderCtx := NewExpanderContext(p.env, p)

	// Invoke the 3-arg transformer: (transformer form rename compare)
	mc, err := p.evaluator.InvokeTransformer(p.ctx, erTransformer.Closure(), expanderCtx, rawForm, renameCls, compareCls)
	if err != nil {
		return nil, wrapSourcedError(sym.SourceContext(), werr.WrapForeignErrorf(err, "er-macro-transformer: transformer failed"))
	}
	defer machine.ReleaseSubContext(mc)

	result := mc.GetValue()
	if values.IsVoid(result) {
		return nil, wrapSourcedError(sym.SourceContext(), werr.WrapForeignErrorf(
			werr.ErrUnexpectedNil,
			"er-macro-transformer: transformer produced no result",
		))
	}

	// Re-wrap the result to syntax.
	// Already-SyntaxValue nodes (from rename) pass through unchanged.
	// Raw symbols get use-site source context (no special scopes = use-site resolution).
	wrapped := schemeutil.DatumToSyntaxValue(p.ctx, sym.SourceContext(), result)
	return wrapped, nil
}

// ExpandOnce performs a single step of macro expansion.
// Returns (expanded-syntax, did-expand, error).
// If the input is a macro call, it expands it once and returns (result, true, nil).
// If the input is not a macro call, it returns (input, false, nil).
// Unlike ExpandExpression, this does NOT recursively expand the result.
func (p *ExpanderTimeContinuation) ExpandOnce(expr syntax.SyntaxValue) (syntax.SyntaxValue, bool, error) {
	// Only pairs can be macro calls
	stxPair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return expr, false, nil
	}

	// Handle empty list
	if syntax.IsSyntaxEmptyList(stxPair) {
		return expr, false, nil
	}

	// Check if the car is a symbol
	car := stxPair.SyntaxCar()
	sym, ok := car.(*syntax.SyntaxSymbol)
	if !ok {
		return expr, false, nil
	}

	sym0, ok := sym.Unwrap().(*values.Symbol)
	if !ok {
		return expr, false, nil
	}

	// R7RS §4.2.2: Local variable bindings shadow macros
	// Check if there's a local variable binding before checking for macros
	if p.hasLocalVariableBinding(sym0, sym.Scopes()) {
		// Local variable shadows macro - no expansion
		return expr, false, nil
	}

	// Check local bindings first (for let-syntax/letrec-syntax),
	// then fall back to the global expand environment.
	// This mirrors ExpandSyntaxExpression's lookup order.
	bnd := p.env.GetBinding(sym0, nil)
	if bnd == nil || bnd.BindingType() != environment.BindingTypeSyntax {
		expandEnv := p.env.Expand()
		bnd = expandEnv.GetBinding(sym0, nil)
	}

	// Check if it's a macro binding
	if bnd == nil || bnd.BindingType() != environment.BindingTypeSyntax {
		// Not a macro - no expansion
		return expr, false, nil
	}

	// Build the cdr (argument list) for both ER and syntax-rules paths.
	var cdr syntax.SyntaxValue
	cdrPair, ok := stxPair.SyntaxCdr().(*syntax.SyntaxPair)
	if ok {
		cdr = cdrPair
	} else {
		cdr = syntax.SyntaxEmptyList
	}

	// Check for ER macro transformer first
	erTransformer, isER := bnd.Value().(*ERMacroTransformer)
	if isER {
		result, err := p.invokeERTransformer(sym, cdr, erTransformer)
		if err != nil {
			return nil, false, wrapSourcedError(sym.SourceContext(), werr.WrapForeignErrorf(err, "expand-once: er-macro-transformer failed"))
		}
		return result, true, nil
	}

	// Get the transformer closure (syntax-rules / lambda)
	cls, ok := bnd.Value().(machine.Closure)
	if !ok {
		return nil, false, wrapSourcedError(sym.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotAClosure, "not a closure: %T", bnd.Value()))
	}

	inputForm := syntax.NewSyntaxCons(sym, cdr, sym.SourceContext())

	mc, err := p.evaluator.InvokeTransformer(p.ctx, cls, nil, inputForm)
	if err != nil {
		return nil, false, wrapSourcedError(sym.SourceContext(), werr.WrapForeignErrorf(err, "expand-once: transformer invocation failed"))
	}
	defer machine.ReleaseSubContext(mc)

	// Check if the transformer produced a result
	result := mc.GetValue()
	if values.IsVoid(result) {
		return nil, false, wrapSourcedError(sym.SourceContext(), werr.WrapForeignErrorf(werr.ErrUnexpectedNil, "syntax transformer produced no result"))
	}

	// Return the result WITHOUT recursive expansion
	stx, ok := result.(syntax.SyntaxValue)
	if ok {
		return stx, true, nil
	}

	return nil, false, wrapSourcedError(sym.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotASyntaxValue, "syntax transformer returned non-syntax value: %T", result))
}

// ExpandSyntaxArgumentList expands each argument in the argument list.
// It returns a new syntax list with the expanded arguments.
func (p *ExpanderTimeContinuation) ExpandSyntaxArgumentList(args syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	// instantiate result list
	q := syntax.SyntaxEmptyList
	// go through each argument and expand it
	// and append to result list
	// if any error, return error
	// if not a proper list, return error
	// finally return the new list
	tail, err := syntax.SyntaxForEach(p.ctx, args, func(_ context.Context, _ int, _ bool, v syntax.SyntaxValue) error {
		v0, err := p.ExpandExpression(v)
		if err != nil {
			return wrapSourcedError(v.SourceContext(), werr.WrapForeignErrorf(err, "failed to expand argument list"))
		}
		// append to result list
		cdr := syntax.SyntaxList(v0.SourceContext(), v0)
		q = q.SyntaxAppend(cdr).(syntax.SyntaxTuple)
		return nil
	})
	if err != nil {
		return nil, wrapSourcedError(args.SourceContext(), werr.WrapForeignErrorf(err, "failed to expand argument list"))
	}
	// tail contains the last element of the list, which should be an empty list. anything else is an error.
	if !syntax.IsSyntaxEmptyList(tail) {
		return nil, wrapSourcedError(args.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotASyntaxList, "expected a list of arguments, got %T", tail))
	}
	return q, nil
}
