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

	"github.com/aalpar/wile/pkg/machine"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/schemeutil"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// DefaultMaxExpandDepth bounds structural recursion depth during macro
// expansion. Without a bound, deeply nested syntax — reachable not from text
// (the parser caps that, see parser.DefaultMaxParseDepth) but from
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
// reflect cumulative Go-stack depth, not per-object depth. Pointer shares the
// guard across a parent and all its children, so depth accumulates regardless
// of how many continuation objects a single run creates. (This is why a
// per-object int — sufficient for the parser, which has one object per parse
// — is not sufficient here.)
type expandDepthGuard struct {
	depth int
	max   int // 0 = unlimited
}

// enter records one level of recursion and reports whether the bound is now
// exceeded. Pair each enter with a deferred leave so depth stays symmetric on
// every return path (including the exceeded one). Used by the quasiquote
// expander, whose mutual recursion is otherwise unbounded — a parallel hole to
// ExpandExpression's guard above, reachable from programmatically-constructed
// (macro/datum->syntax) quasiquote forms the parser's depth cap cannot reach.
func (g *expandDepthGuard) enter() (exceeded bool) {
	g.depth++
	return g.max > 0 && g.depth > g.max
}

// leave undoes one enter.
func (g *expandDepthGuard) leave() {
	g.depth--
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
	// useSiteScopes registers the use-site scopes this run mints, so pruning can
	// recognize its own. Shared by pointer with child expanders. See
	// expander_use_site.go.
	useSiteScopes *useSiteScopeLog
}

// NewExpanderTimeContinuation creates a new ExpanderTimeContinuation. The
// returned expander begins a fresh expansion run with its own depth guard.
// The bound is read from the env's namespace (WithMaxExpandDepth, forwarded onto
// the shared EngineServices at engine build) so every expander site — the
// top-level pass and the ~8 compile-time re-expansion sites reached during
// library and body compilation — honors it uniformly. A namespace not built by
// an Engine (e.g., a direct unit-test compiler) reports unset and defaults to
// DefaultMaxExpandDepth. SetMaxDepth still overrides per-run afterward.
func NewExpanderTimeContinuation(ctx context.Context, env *environment.EnvironmentFrame, evaluator machine.MacroEvaluator) *ExpanderTimeContinuation {
	maxDepth := DefaultMaxExpandDepth
	n, ok := env.Namespace().MaxExpandDepth()
	if ok {
		maxDepth = n
	}
	q := &ExpanderTimeContinuation{
		ctx:           ctx,
		env:           env,
		evaluator:     evaluator,
		depthGuard:    &expandDepthGuard{max: maxDepth},
		useSiteScopes: &useSiteScopeLog{scopes: values.NewMapSet[*syntax.Scope](0)},
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
// flow into a nested expander, the code sets it explicitly, e.g.,
// compile_time_continuation_include.go). Sharing only the depth guard keeps the
// hygiene behavior unchanged.
func (p *ExpanderTimeContinuation) newChildExpander(env *environment.EnvironmentFrame) *ExpanderTimeContinuation {
	q := &ExpanderTimeContinuation{
		ctx:           p.ctx,
		env:           env,
		evaluator:     p.evaluator,
		depthGuard:    p.depthGuard,
		useSiteScopes: p.useSiteScopes,
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

// hasLocalVariableBinding delegates to EnvironmentFrame.HasLocalVariableBinding,
// resolving the reference hygienically against its own scope set.
// R7RS §4.2.2: let bindings shadow outer bindings including macros.
func (p *ExpanderTimeContinuation) hasLocalVariableBinding(sym *values.Symbol, scopes []*syntax.Scope) bool {
	return p.env.HasLocalVariableBinding(sym, syntax.ScopesOf(scopes))
}

// ExpandTopLevelExpression expands a whole TOP-LEVEL form.
//
// Use this at every top-level entry point (the Engine's ExpandAndCompile, the
// test-harness pipelines); use ExpandExpression for sub-forms.
//
// Hygiene for a macro-introduced top-level binder is a property of the GLOBAL
// FRAME, not of this expansion step: the binder carries the expansion's intro
// scope and the frame keys its slots by scope set, so two expansions land in
// two slots and a bare (empty-scope) reference cannot reach either (R7RS
// §4.3.2). This used to be restored syntactically here, by renaming every
// macro-introduced binder and its references to a fresh unique name; see
// pkg/environment/global_environment_frame.go for the storage that replaced it,
// and pkg/wile/toplevel_binder_scope_test.go for the pinned behavior.
func (p *ExpanderTimeContinuation) ExpandTopLevelExpression(expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.ExpandExpression(expr)
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
	// canceled run reports ctx.Err(), not ErrExpandDepthExceeded, even when both
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
		result, err = p.ExpandSyntaxOrProcedureCall(car, cdr, stx.SourceContext())
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
func (p *ExpanderTimeContinuation) ExpandSyntaxOrProcedureCall(car syntax.SyntaxValue, cdr syntax.SyntaxValue, parentCtx *syntax.SourceContext) (syntax.SyntaxValue, error) {
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
		// Unknown car type (e.g. the empty-list operator in `(())`) - the
		// empty-list singleton carries no source context, so fall back to the
		// enclosing form's context rather than emitting a location-less node
		// that codegen would then mislocate to the parent form.
		ctx := car.SourceContext()
		if ctx == nil {
			ctx = parentCtx
		}
		return syntax.NewSyntaxCons(car, cdr, ctx), nil
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
// This function looks up the primitive expander in the expanded environment registry.
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

// lookupMacroBinding resolves the reference sym to a macro (syntax) binding, or nil.
//
// Five probes, in order: (arm 1) the local env (let-syntax / letrec-syntax), scope-precise;
// (D2) the definition-site pin sym carries as a free template identifier — consulted between
// arms 1 and 2 so a co-introduced keyword still shadows it but a use-site define-syntax
// cannot capture it; (arm 2) the expand phase one step up from the expanding frame (the
// symmetric counterpart of define-syntax storage — expanding phase-N code reads macros at
// phase N+1, so a macro defined inside a transformer body resolves at its climbed phase; at
// phaseLevel 0, NextPhase() == Expand()); (arm 2b) the owner's SEALED phase-1 tier, from
// phase 2 and above, which is how the startup set's macros stay reachable from a
// transformer nested in a transformer; and (arm 3) the library env named by any scope the
// symbol carries, which is how a library macro reaches an UNEXPORTED helper macro of its own
// library.
//
// It takes the full SyntaxSymbol (not the bare *values.Symbol) so it can read the pin; the
// callers keep their own sym.Unwrap() for the hasLocalVariableBinding / LookupPrimitiveExpander
// checks. It exists because ExpandOnce once had only arms 1 and 2. (Expand-once ...) therefore
// reported a macro reachable solely real macro call that the expander itself expands perfectly
// well.
func (p *ExpanderTimeContinuation) lookupMacroBinding(sym *syntax.SyntaxSymbol, symbolScopes []*syntax.Scope) *environment.Binding {
	sym0, ok := sym.Unwrap().(*values.Symbol)
	if !ok {
		return nil
	}

	// ARM 1: current-phase local+global, scope-precise. Resolve under the reference's
	// own scope set, not nil. GetBinding documents nil as MATCH ANY, which takes the
	// first live slot of the name and so lets a bare user reference reach a keyword
	// introduced inside a macro expansion. This is only sound once binders are
	// scope-keyed at creation: a wildcard read and a wildcard write were cancelling, so
	// fixing either alone relocates the asymmetry rather than closing it. A co-introduced
	// let-syntax keyword (shares the intro scope) is caught HERE and MUST win over the pin
	// (the R1 invariant); a use-site let-syntax binder of a different scope is refused here.
	//
	// A ∅-scoped match is DEMOTED below the pin, per coIntroducedByExpansion — this arm
	// is the macro-path twin of CompileSymbol's co-introduced-global arm and needs the
	// same guard. Phase 0 never exercises the demotion (every BindingTypeSyntax creation
	// site writes an Expand-phase global or a scoped local, so no ∅-scoped syntax global
	// is reachable from a phase-0 p.env), but a PROCEDURAL transformer body compiles
	// against env.NextPhase() (compile_transformer.go's compileAndEvalLambdaTransformer,
	// shared by lambda and er-macro-transformer), so there p.env IS the phase-1 frame where
	// phase-0 define-syntax deposits exactly such globals, and a same-named user keyword
	// captured a library macro's pinned template identifier.
	//
	// Demoted, not dropped: with no pin the ∅-scoped match is still the right answer, and
	// it is the only arm that finds it (arm 2 looks a phase ABOVE p.env). So the fallback
	// sits between the pin and arm 2, which makes pin-beats-ambient the whole behavior
	// change.
	var ambient *environment.Binding
	bnd := p.env.GetBinding(sym0, syntax.ScopesOf(symbolScopes))
	if bnd != nil && bnd.BindingType() == environment.BindingTypeSyntax {
		if coIntroducedByExpansion(bnd) {
			return bnd
		}
		ambient = bnd
	}

	// D2: definition-site pin. A free template identifier carrying a GlobalIndex resolves
	// to its DEFINITION-site macro binding here — AFTER arm 1 (so a co-introduced
	// let-syntax keyword, which shares the intro scope, still shadows it: the R1 ordering,
	// mirrored from compile_time_continuation.go's post-GetLocalIndex tryResolvedBinding)
	// and BEFORE the use-site NextPhase/library arms (so a top-level or use-site
	// define-syntax cannot capture it). A directly typed reference carries no pin and falls
	// straight through — the present/absent-pin split IS the def-site/use-site split. The
	// pin is a specific (frame, slot) via GetOwnGlobalBinding (no parent walk), strictly
	// more specific than the scope walk, so it cannot reintroduce a wildcard match; and the
	// BindingTypeSyntax filter excludes compile-time handlers (primitive expanders / syntax
	// compilers are BindingTypePrimitive), so a pin resolving to one falls through.
	gi, ok := sym.ResolvedBinding.(*environment.GlobalIndex)
	if ok && gi != nil && gi.Env != nil {
		// gi.Env != nil guards the immediate deref below: a *GlobalIndex can be a
		// DEFERRED index (Env == nil, e.g. NewDeferredGlobalIndex). Every pin attached to
		// SyntaxSymbol.ResolvedBinding today is resolved (Env set), but that is a non-local
		// invariant the type does not enforce, so guard the field we dereference.
		pinned := gi.Env.GetOwnGlobalBinding(gi)
		if pinned != nil && pinned.BindingType() == environment.BindingTypeSyntax {
			return pinned
		}
	}

	// ARM 1, demoted: no pin resolved, so the ambient ∅-scoped match from the current
	// phase stands. This is how a direct reference inside a procedural transformer body
	// reaches a phase-0 (define-syntax …): that keyword lives in p.env's own phase there.
	if ambient != nil {
		return ambient
	}

	// ARM 2: NextPhase (define-syntax storage). A top-level user (define-syntax …) lands
	// here; without the pin above, this is where the #1 capture happened.
	expandEnv := p.env.NextPhase()
	bnd = expandEnv.GetBinding(sym0, syntax.ScopesOf(symbolScopes))
	if bnd != nil && bnd.BindingType() == environment.BindingTypeSyntax {
		return bnd
	}

	// ARM 2b: the owner's SEALED phase-1 tier, from phase 2 and above. A
	// bootstrap define-syntax writes at (phase 1, sealed), an exact-phase
	// coordinate, so a transformer nested in a transformer (phase-2 code) cannot
	// reach `and` — or, once they are macros, syntax-case and syntax-rules —
	// through arms 1 and 2. The Go syntax forms those macros replace were
	// reachable from every phase: syntax compilers sit at the ambient coordinate
	// and LookupPrimitiveExpander reads env.Expand(), the owner's first rung,
	// from any level. This arm gives the startup set's macros the same reach.
	//
	// The sealed tier, not the whole phase-1 frame: the expander read it mirrors
	// goes through LookupPhaseBinding, which accepts only BindingTypePrimitive,
	// so it never exposed a user macro either, and a user (define-syntax …)
	// writes at (1, mutable), which stays phase-sealed — visibility follows
	// mutability (memory 2026-07-10-hermetic-phases-mutability-visibility).
	//
	// The phase-2 rung is no longer the only way in. The arm also runs from phase
	// 0 and 1 when ARM 2 resolved to something that is NOT a macro, because that
	// is the second face of the phase-shifted-import defect LookupPhaseBinding
	// documents: an (import (for-syntax (scheme base))) installs base's exported
	// `syntax-rules` — a phase-0 *SyntaxCompiler, BindingTypePrimitive — at
	// (phase 1, MUTABLE), where it out-ranks the Scheme-layer macro sitting
	// at (1, sealed). ARM 2 then declines it on type, the head is left unexpanded,
	// and the COMPILER dispatches the Go form silently, which is the "one Go
	// syntax form compiled under WithSchemeSyntaxForms" column of that defect.
	//
	// Gating on a BindingTypePrimitive answer, rather than on the phase, keeps the
	// narrowing to the mask case: a macro answer has already returned from ARM 2,
	// an ARM 2 miss (nil) leaves the phase-2 rule exactly as it was, and a user's
	// own BindingTypeVariable binding of the name still shadows. Only a
	// registry-minted row — a primitive expander, a syntax compiler, a value-less
	// keyword, or an import that copied one — gets looked past. Same discriminator,
	// and same reason, as LookupPhaseBinding.
	//
	// It is the compile-time HANDLER that masks here, not the import's own slot:
	// measured, ARM 2's winner for `syntax-rules` under the Scheme layer is the
	// sealed Go *SyntaxCompiler, carrying no Imported meta. The import is what
	// routes the lookup here at all — with `define-syntax` masked at phase 1 the
	// form takes a different path — so this arm and LookupPhaseBinding's fallback
	// are two faces of one defect, and neither alone closes it.
	masked := bnd != nil && bnd.BindingType() == environment.BindingTypePrimitive
	if p.env.PhaseLevel() > environment.PhaseExpand || masked {
		ge := p.env.GlobalEnvironment()
		if ge != nil {
			bnd = ge.SealedBindingAt(sym0, syntax.ScopesOf(symbolScopes), environment.PhaseExpand)
			if bnd != nil && bnd.BindingType() == environment.BindingTypeSyntax {
				return bnd
			}
		}
	}

	// ARM 3: library-scope (unexported helper macro of the symbol's own library).
	if p.env.Namespace() == nil {
		return nil
	}
	for _, scope := range symbolScopes {
		libEnv := p.env.Namespace().LookupLibraryEnv(scope)
		if libEnv == nil {
			continue
		}
		// AllScopes (match-any by name) is deliberate here, unlike arms 1/2. The
		// SCOPE already did the routing: LookupLibraryEnv(scope) selected the
		// symbol's home library env. The helper was defined there under the
		// library's own DEFINITION scopes, which are not a subset of the use-site
		// symbolScopes (a different expansion universe). A scoped query
		// (ScopesOf(symbolScopes)) would apply Flatt's subset filter and reject
		// the helper, breaking the cross-library resolution this arm exists for.
		// So the final step is a nominal by-name lookup within the routed env.
		libBnd := libEnv.Expand().GetBinding(sym0, syntax.AllScopes())
		if libBnd != nil && libBnd.BindingType() == environment.BindingTypeSyntax {
			return libBnd
		}
	}
	return nil
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
		bnd := p.lookupMacroBinding(sym, sym.Scopes())
		if bnd != nil {
			return p.expandMacroInvocation(sym, expr, bnd)
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
	//
	// The input is stamped with a fresh use-site scope, which is NOT removed from
	// the result. Everything the transformer passes through therefore wears a
	// scope nothing it introduces has, which is what lets ordinary subset
	// resolution tell a use-site identifier from a macro-introduced one. See
	// the newUseSiteScope and pruneUseSiteScopes below for the binder positions
	// where the scope has to come off again.
	inputForm := p.withUseSiteScope(syntax.NewSyntaxCons(sym, expr, sym.SourceContext()))

	// Flatt's introduction step (design §2.2): a fresh scope goes onto the WHOLE
	// input, the transformer runs, and the scope is flipped on the output.
	// Identifiers that came in through the input carry it and lose it on the
	// flip; identifiers the transformer introduced gain it. One mechanism gives
	// every closure transformer hygiene and per-invocation freshness of
	// datum->syntax binders, including the Scheme-specified forms P1 adds, which
	// manipulate no scopes themselves. From here on the Go syntax-rules and
	// syntax-case transforms pass a nil intro scope of their own (Q9), so a
	// closure transformer sees exactly one. ER stays outside the flip until P2:
	// expandMacroInvocation returned into expandERMacroInvocation above, and
	// invokeERTransformer's UnwrapAll hands back pass-through identifiers
	// re-wrapped with the keyword's use-site context (no intro scope), which a
	// flip would stamp.
	//
	// Order with use-site scopes: use-site added, intro added, invoke, intro
	// flipped, use-site pruned.
	introScope := syntax.NewScopeWithLabel("intro")
	inputForm = syntax.AddScopeToSyntax(inputForm, introScope)

	// Set up the expander context so the transformer can access the use-site environment.
	// This is critical for R7RS §4.3.2 auxiliary syntax hygiene: the pattern matcher
	// needs to check if input identifiers have lexical bindings at the use site.
	// For example, in (let ((=> #f)) (cond (#t => 'ok))), the pattern matcher needs
	// to see that the lambda binds => (from let expansion) to correctly
	// determine that it shouldn't match the literal => in cond's pattern.
	expanderCtx := NewExpanderContext(p.env, p)
	expanderCtx.SetIntroductionScope(introScope)

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
		// Flip the introduction scope off everything that came in through the
		// input and onto everything the transformer introduced (see the mint
		// above). Then strip this run's use-site scopes from any binder the
		// output defines, before re-expansion carries the form further. Every
		// macro output passes this one point, so a definition that only surfaces
		// after several expansions is pruned at its own level. Identity while
		// nothing mints.
		stx = syntax.FlipScope(stx, introScope)
		stx = p.pruneUseSiteScopes(stx)
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

	// Unwrap to raw s-expression for the transformer.
	//
	// This is why use-site scopes stop here: UnwrapAll strips every scope, and
	// DatumToSyntaxValue re-wraps the result with plain use-site context below, so
	// an ER transformer neither receives nor returns scoped syntax. Its hygiene is
	// the rename closure instead. Stamping inputForm above would be erased on this
	// line — the asymmetry with expandMacroInvocation is the boundary, not an
	// oversight to repair.
	rawForm := inputForm.UnwrapAll()

	// Create a fresh intro scope for this invocation. Unbound renamed symbols
	// (like temporary names) get this scope to prevent variable capture.
	introScope := syntax.NewScope()

	// Create rename closure. It reads the DEFINITION-SITE store — that is what
	// makes a rename denote at the definition site and not here — but at the
	// phase the OUTPUT lands in, which is this expansion's own phase, not the
	// phase the transformer BODY runs at.
	//
	// DefEnv() is the frame the transformer's right-hand side compiled in, one
	// rung UP the tower (compileTransformerValue's env.NextPhase()). Reading the
	// rename there asks the transformer's IMPLEMENTATION environment what a name
	// means, and the output is not written in that language. Design §3.5 pins the
	// contract as (datum->syntax #'k sym), where `k` is the er-macro-transformer
	// keyword as written at the user's define-syntax — a phase-0 identifier in a
	// phase-0 program — so the renamed identifier is an identifier of the output's
	// phase. Racket, whose explicit phasing this tree copies, answers the same way:
	// a template identifier resolves at the phase of use while the transformer
	// body's reference to that name resolves one phase up, and a for-syntax-only
	// import leaves the template identifier unbound. (Chez says otherwise and is
	// not the oracle: it implements R6RS implicit phasing and collapses the tower.)
	//
	// Measured, the phase-up read was doing two wrong things. It let an
	// (import (for-syntax …)) mint a phase-1 slot that out-ranked the phase-0
	// binding the use site reaches, so `compare` reported two spellings of one
	// keyword as different bindings and an ER-built cond stopped recognizing
	// `else`; and it let a rename SMUGGLE a phase-1-only import into phase-0
	// output, where a plain reference to that name is a compile error.
	//
	// Where the old read was RIGHT it was right by way of the base's bulk row,
	// which hands a phase-1 query the very same *Binding object the phase-0 slot
	// holds (materializeBulkLocked returns the source's own slotRef). So for every
	// name that resolved correctly before, this reads the identical pointer; the
	// only queries whose answer moves are the ones where phase 1 owns a slot of
	// its own, and those are exactly the ones that must not be visible here.
	//
	// p.env.PhaseLevel() rather than a constant: a macro defined at phase 0 and
	// used inside a transformer body emits phase-1 code, and the rename follows
	// the use.
	renameCls := NewERRenameClosure(erTransformer.DefEnv().AtPhase(p.env.PhaseLevel()), introScope)

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
	wrapped, err := schemeutil.DatumToSyntaxValue(p.ctx, sym.SourceContext(), result)
	if err != nil {
		return nil, wrapSourcedError(sym.SourceContext(), err)
	}
	return wrapped, nil
}

// ExpandOnce performs a single step of macro expansion.
// Returns (expanded-syntax, did-expand, error).  If the input is a macro call, it expands
// it once and returns (result, true, nil).  If the input is not a macro call, it returns
// (input, false, nil).  Unlike ExpandExpression, this does NOT recursively expand the result.
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

	// The SAME four-probe lookup the expander itself uses — local, definition-site
	// pin, next phase, then the library env named by the symbol's scopes. This used
	// to be a hand-copied two-step version, missing the library arm, so (expand-once …) reported a macro
	// reachable only through a library scope as not-a-macro, even though expansion
	// itself handled it fine.
	bnd := p.lookupMacroBinding(sym, sym.Scopes())
	if bnd == nil {
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

	// Stamped, flipped and pruned exactly as expandMacroInvocation does, and
	// handed the same context. This site is a hand-copy of that one — its own
	// comment above records a defect from the last time the two diverged — and
	// expand-once exists to show what one expansion step produces, so an
	// unstamped result here would be a lie about the real path rather than a
	// saving. After P0.4 and P0.5 a transformer reads the context from
	// syntax-local-value and free-identifier=?, and a nil here made single-step
	// expansion diverge from the loop.
	inputForm := p.withUseSiteScope(syntax.NewSyntaxCons(sym, cdr, sym.SourceContext()))
	introScope := syntax.NewScopeWithLabel("intro")
	inputForm = syntax.AddScopeToSyntax(inputForm, introScope)

	expanderCtx := NewExpanderContext(p.env, p)
	expanderCtx.SetIntroductionScope(introScope)

	mc, err := p.evaluator.InvokeTransformer(p.ctx, cls, expanderCtx, inputForm)
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
		return p.pruneUseSiteScopes(syntax.FlipScope(stx, introScope)), true, nil
	}

	return nil, false, wrapSourcedError(sym.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotASyntaxValue, "syntax transformer returned non-syntax value: %T", result))
}

// ExpandSyntaxArgumentList expands each argument in the argument list.
// It returns a new syntax list with the expanded arguments.
func (p *ExpanderTimeContinuation) ExpandSyntaxArgumentList(args syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	// instantiate result list
	q := syntax.SyntaxEmptyList
	// go through each argument and expand it and append to result list if any error, return error if not a proper
	// list, return error finally return the new list
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
