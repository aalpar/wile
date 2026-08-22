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
	"context"
	"slices"

	"github.com/aalpar/wile/pkg/machine"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// inlineCandidate holds a let-bound lambda eligible for call-site inlining.
type inlineCandidate struct {
	lambda *validate.ValidatedLambda
	// env is the compile-time environment at registration. Inlining is only
	// safe when the call site is in the same scope — nested scopes may shadow
	// free variables that the lambda captured at its definition site.
	env *environment.EnvironmentFrame
}

// CompileTimeContinuation is a continuation used during the compilation phase
type CompileTimeContinuation struct {
	env         *environment.EnvironmentFrame
	template    *machine.NativeTemplate
	sourceStack []*syntax.SourceContext
	// libraryCallback is called when a library is compiled (for LoadLibrary)
	libraryCallback func(*CompiledLibrary)
	// libraryScope is the unique scope for the library being compiled.
	// Non-nil only when compiling inside a define-library body.
	// Threaded to CompileSyntaxRules so free identifiers in macro templates
	// can carry the library scope for cross-library hygiene.
	libraryScope *syntax.Scope
	// patternVars and patternVarSyntax carry the enclosing syntax-case clause's
	// pattern context into CompileSyntax so that (syntax template) forms with
	// ellipsis can be expanded hygienically (the runtime ellipsis path mirrors
	// the syntax-rules transformer). Set on the body compiler by
	// compileSyntaxCaseClause; nil for (syntax ...) outside syntax-case (which
	// has no captures and errors at runtime anyway). patternVars distinguishes
	// pattern variables (substituted) from free identifiers (resolved at the
	// definition site); patternVarSyntax carries each pattern variable's scopes
	// for nested-macro scope comparison.
	patternVars      values.StringSet
	patternVarSyntax map[string]*syntax.SyntaxSymbol
	// fileResolver controls how include/load resolves files.
	// Defaults to the resolver stored on Namespace (usually
	// OSFileResolver); set to EmbedFileResolver for bootstrap.
	fileResolver FileResolver
	// evaluator abstracts VM execution for compile-time evaluation
	// and transformer invocation so the compiler can be tested
	// without the concrete VM.
	evaluator machine.MacroEvaluator

	// inlineCandidates maps BindingID → lambda for let-bound closures eligible
	// for call-site inlining. Populated by CompileValidatedLet, consumed by
	// compileValidatedCall. Keyed by BindingID for stable cross-scope identity.
	inlineCandidates map[environment.BindingID]inlineCandidate
	// currentlyInlining tracks bindings being inlined to prevent infinite
	// recursion for self-referential letrec bindings.
	currentlyInlining values.MapSet[environment.BindingID]
	// inlineThreshold is the maximum body length (in top-level expressions)
	// for inlining eligibility. 0 disables inlining.
	inlineThreshold int
	// frameReclaimVerdict is the interprocedural ClassifyFrameReclaim result for the
	// top-level unit, computed once in CompileValidatedBegin and consulted by
	// frameReuseForDefine THROUGH unitFrameReclaimVerdict, never read directly — see
	// that method for the boundary the direct read violated. nil on child/closure
	// compilers, library/transformer compiles, and single-expression (non-begin)
	// programs — there frameReuseForDefine falls back to the per-body predicate
	// (status quo).
	//
	// Keyed by validate.ScopedBindingKey — the define name's (Sym.Key, scope
	// fingerprint) — matching ClassifyFrameReclaim's own return domain. Two
	// hygiene-distinct top-level binders of one name carry different scope sets, so
	// each has a distinct identity and its own verdict instead of sharing one;
	// frameReuseForDefine looks up the identity of the very define it is compiling, so
	// a macro-introduced define and a colliding user define no longer share a
	// reclamation verdict. This also makes the let-body read-gate in
	// unitFrameReclaimVerdict structurally redundant — an internal define's name
	// carries the enclosing let/lambda scope, so its identity differs from a same-named
	// top-level define and misses this top-level map — though that gate is kept as an
	// explicit tightening.
	frameReclaimVerdict map[validate.ScopedBindingKey]bool
	// unitArity maps each StableInUnit top-level define in this unit to its
	// formals' arity. Set beside frameReclaimVerdict in CompileValidatedBegin,
	// under the same gate, and read by checkCallArity for a callee whose binding
	// exists but whose closure value does not (a same-unit define builds its
	// closure at run time, so Binding.Value() holds nothing to interrogate).
	//
	// Keyed by symbol Key, unlike frameReclaimVerdict's scoped identity, and the
	// difference is forced: frameReuseForDefine looks up the define it is
	// compiling and so holds the binder's own scopes, while a call site holds
	// only a reference, whose scope set is a strict superset of its binder's. A
	// fingerprint cannot answer a subset query. What makes the name key sound
	// here is instead validate.UnitArityOf's StableInUnit gate — see its comment.
	unitArity map[string]validate.UnitArityInfo
	// quasiMaxDepth bounds structural recursion in the quasiquote/quasisyntax
	// expander. 0 means use DefaultMaxExpandDepth; tests set a low value to
	// exercise the bound cheaply. See effectiveQuasiMaxDepth.
	quasiMaxDepth int
	// boxedSlots is the flat-closure boxing verdict: every local slot that holds
	// a *values.Box rather than its value directly, named absolutely by owning
	// frame rather than by (slot, depth).
	//
	// Decided at the BINDER (markBoxedBinders), because "captured ∧ assigned" is
	// a property of the variable and a lambda cannot see a set! in its enclosing
	// body. SHARED — not copied — with the child continuation a lambda body
	// compiles under, so a read arbitrarily deep inside agrees with the emit
	// that installed the cell. Absolute keys are what make the sharing correct
	// without re-basing at every frame descent.
	//
	// Three sites must give the same answer or the program hands a #<box> to
	// Scheme: emitBoxSlots at the binder, emitLocalLoad at every read,
	// emitLocalStore at every write. All three route through localIsBoxed.
	boxedSlots values.MapSet[boxedSlotKey]
	// freeLayout is the free-variable layout of the lambda whose body THIS
	// continuation compiles — the vector its closures were built with, and the
	// only way its body can reach a variable of an enclosing lambda now that the
	// static link is a phase view with no lexical parent.
	//
	// nil for the top-level program compiler and for any body that captures
	// nothing. Set by compileBody on the child continuation, never shared: a
	// layout belongs to one lambda.
	freeLayout []freeVar
	// boxOnDeclare is the reference index of the body region currently being
	// compiled, consulted by declareDefineBinding for a define whose slot no
	// region scan enumerated. See maybeBoxOnDeclare for why it exists and why
	// its predicate is weaker than the region scans'.
	boxOnDeclare refIndex
	// shape is the frame a merged `let`'s slots are allocated in: the enclosing
	// lambda's parameter frame, which is also what tpl.SetShape recorded, so a
	// slot appended to it widens the frame every apply builds.
	//
	// nil disables merging entirely, and that is how the top-level, library and
	// transformer compilers keep the OpPushEnv path: they have no enclosing frame
	// to merge into. Set by compileBody on the child continuation.
	shape *environment.EnvironmentFrame
	// mergedShape maps each compile-time `let` frame that was merged to the frame
	// it was merged INTO. A frame present here does not exist at run time, so
	// runtimeIndex skips it when converting a compile-time depth into a runtime
	// one. The value is not always p.shape: a body that retains its lexical env
	// can reach an outer lambda's merged slot by depth.
	//
	// SHARED, not copied, with every continuation compiling against the same env
	// chain — for the same reason boxedSlots is. A reference from inside a
	// syntax-case clause body reaching out into an enclosing merged `let` is
	// compiled by a different continuation, and one with an empty map here would
	// emit an untranslated index.
	mergedShape map[*environment.EnvironmentFrame]*environment.EnvironmentFrame
	// mergedSlot maps a merged frame's own slot to the slot it occupies in shape.
	//
	// Allocated lazily, one slot at a time, deliberately: a frame's binding count
	// is not fixed when its `let` starts (predeclareBodyDefines runs first, and
	// withBoxOnDeclare's net can create a slot at any point during the body), so
	// a contiguous per-`let` block would be corrupted by any later append to that
	// frame. Shared alongside mergedFrames.
	mergedSlot map[boxedSlotKey]int
}

// unitFrameReclaimVerdict reports the interprocedural verdict for the define with
// identity id, and only while this compiler is still at the top level of the unit
// the verdict was computed for.
//
// The gate (ns.Runtime() == p.env) is now belt-and-suspenders: binding-identity
// keying already makes the leak it guarded impossible. CompileValidatedLet compiles
// a let body on this SAME compiler with p.env swapped to the body's frame
// (compile_let.go — the only such swap in the package; every other body compile
// builds a child continuation, where this map is nil), so a define below top level
// reaches frameReuseForDefine with the map still live. But an internal define's name
// carries the enclosing let/lambda scope, so its identity differs from a same-named
// top-level define and misses this top-level map, reading false whatever that
// define's verdict is. The gate is retained as an explicit tightening: it can
// withhold a verdict, never grant one, per the subsystem's kill-don't-guard rule.
func (p *CompileTimeContinuation) unitFrameReclaimVerdict(id validate.ScopedBindingKey) bool {
	if p.frameReclaimVerdict == nil {
		return false
	}
	ns := p.env.Namespace()
	if ns == nil || ns.Runtime() != p.env {
		return false
	}
	return p.frameReclaimVerdict[id]
}

// effectiveQuasiMaxDepth returns the recursion bound for quasiquote/quasisyntax
// expansion: the per-continuation override when set, else DefaultMaxExpandDepth.
func (p *CompileTimeContinuation) effectiveQuasiMaxDepth() int {
	if p.quasiMaxDepth > 0 {
		return p.quasiMaxDepth
	}
	return DefaultMaxExpandDepth
}

// newQuasiDepthGuard builds a fresh depth guard for one top-level quasiquote or
// quasisyntax expansion (or needs-runtime analysis) on this continuation.
func (p *CompileTimeContinuation) newQuasiDepthGuard() *expandDepthGuard {
	return &expandDepthGuard{max: p.effectiveQuasiMaxDepth()}
}

// NewCompileTimeContinuation creates a new CompileTimeContinuation.
// The file resolver defaults to the one stored on the Namespace.
// If none is set, falls back to a fresh OSFileResolver.
func NewCompileTimeContinuation(tpl *machine.NativeTemplate, env *environment.EnvironmentFrame, evaluator machine.MacroEvaluator) *CompileTimeContinuation {
	resolver := env.FileResolver()
	if resolver == nil {
		resolver = NewOSFileResolver(env)
	}
	q := &CompileTimeContinuation{
		env:             env,
		template:        tpl,
		fileResolver:    resolver,
		evaluator:       evaluator,
		inlineThreshold: DefaultInlineThreshold,
	}
	return q
}

// SetFileResolver overrides the file resolver used by include/load.
// Nil resets to the environment's resolver (or OSFileResolver as fallback).
func (p *CompileTimeContinuation) SetFileResolver(r FileResolver) {
	if r == nil {
		r = p.env.FileResolver()
		if r == nil {
			r = NewOSFileResolver(p.env)
		}
	}
	p.fileResolver = r
}

// DefaultInlineThreshold is the default maximum body length for procedure
// inlining. A lambda body with more expressions than this is not inlined.
const DefaultInlineThreshold = 5

// SetInlineThreshold sets the maximum body length for procedure inlining.
// 0 disables inlining entirely.
func (p *CompileTimeContinuation) SetInlineThreshold(n int) {
	p.inlineThreshold = n
}

// formArgs extracts the argument list from a compiled form's expression.
// expr is the CDR of the form (keyword stripped by syntaxCompiler in register.go).
// usage describes what the form expects (e.g. "bindings and body") for error
// messages. Returns the arguments as a non-empty SyntaxPair, or an error if
// expr is not a SyntaxPair or is the empty list.
func formArgs(expr syntax.SyntaxValue, formName, usage string) (*syntax.SyntaxPair, error) {
	argsPair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(argsPair) {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s: expected %s", formName, usage)
	}
	return argsPair, nil
}

// formSingleArg extracts exactly one argument from a compiled form's expression.
// Returns an error if the form does not have exactly one argument.
func formSingleArg(expr syntax.SyntaxValue, formName string) (syntax.SyntaxValue, error) {
	parts, err := syntax.FormParts(expr, formName, 1, 1)
	if err != nil {
		return nil, err
	}
	return parts[0], nil
}

// SetLibraryCallback sets a callback function that will be called when a library
// is compiled via CompileDefineLibrary. This is used by LoadLibrary to capture
// the compiled library.
func (p *CompileTimeContinuation) SetLibraryCallback(cb func(*CompiledLibrary)) {
	p.libraryCallback = cb
}

// coIntroducedByExpansion reports whether a binding found by a scope-precise walk
// under a reference's own scope set was introduced by the SAME expansion (or
// library) as that reference, and therefore outranks the reference's
// definition-time pin.
//
// THIS PREDICATE IS WHAT SEPARATES THE FIX FROM A NAIVE REORDER, at both sites that
// place a scope-precise arm ahead of a pin: CompileSymbol's co-introduced-global arm
// and ExpanderTimeContinuation.lookupMacroBinding's arm 1. The resolution
// (probeRankedLocked / scopedBestOf) has already filtered candidates on bindingScopes ⊆ symbolScopes, so a
// NON-EMPTY match shares at least one scope object with the reference; scopes are
// minted per expansion, so sharing one means shared provenance, which is the R7RS §4.3
// renaming the arm implements. An ambient ∅-scoped binding shares nothing yet passes
// the subset filter vacuously (∅ ⊆ anything), so it does NOT qualify — that is the
// binding class the pin exists to protect from capture (guard/guard-aux,
// %prompt-reinstall; PR #814).
//
// It does not decide what happens to a non-qualifying match: CompileSymbol falls
// through to later arms that can still reach it, lookupMacroBinding retains it as its
// post-pin fallback. Only the ranking against the pin is shared.
func coIntroducedByExpansion(bnd *environment.Binding) bool {
	return bnd != nil && len(bnd.Scopes()) > 0
}

// CompileSymbol compiles a syntax symbol expression.
//
// Resolution order (Flatt 2016 "sets of scopes"): a scope-set local match wins
// over everything, including a ResolvedBinding pin carried from macro expansion.
// This lets a binding co-introduced by a template — which carries the same intro
// scope as the reference — shadow a same-named global. The ResolvedBinding and
// library-scope fallbacks fire only when no lexical binding shadows.
// See plans/2026-06-15-macro-hygiene-global-shadow-fix.local.md (Change 2).
func (p *CompileTimeContinuation) CompileSymbol(ctctx CompileTimeCallContext, expr *syntax.SyntaxSymbol) error {
	sym := expr.Sym

	// Get the scopes from the syntax symbol for hygiene checking.
	// Both this path and the expander's hasLocalVariableBinding (expander_time_continuation.go)
	// check bindingScopes ⊆ useScopes via syntax.ScopesMatch. This path uses the environment's
	// maximality algorithm (GetLocalIndex) to find the most specific binding for
	// codegen dispatch; the expander only needs a yes/no shadow check for a single binding.
	symbolScopes := expr.Scopes()

	// Fast path: if the symbol has no scopes, skip scope-aware resolution.
	//
	// Safety invariant: empty scopes implies no local bindings in scope.
	// Every binding form that creates locals (lambda, and by extension let/let*/letrec)
	// goes through expandLambdaForm, which adds a lambdaScope to all body identifiers
	// BEFORE inner expansion. A symbol with empty scopes can therefore only appear at
	// top level, where GetLocalIndex returns nil and falls through to globals.
	if len(symbolScopes) == 0 {
		// Try local binding first
		li := p.env.GetLocalIndex(sym, syntax.EmptyScopes())
		if li != nil {
			p.emitLocalLoad(li)
			return nil
		}

		// A pre-resolved global binding (cross-library hygiene / ER rename) is
		// consulted after the local check but before the use-site global, so a
		// free template identifier keeps its definition-site identity.
		if p.tryResolvedBinding(expr) {
			return nil
		}

		// Try global binding. This arm is the empty-scope-set case (symbolScopes
		// is empty here), so it must use the scope-carrying form, NOT the wildcard
		// GetGlobalIndex: a reference written outside any macro expansion must not
		// reach a binder introduced inside one.
		gi := p.env.GetGlobalIndexWithScopes(sym, syntax.ScopesOf(symbolScopes))
		if gi == nil {
			// Stamp the IDENTIFIER's own location, not the enclosing form's.
			// Without a stamp here the error reaches CompileExpression's
			// wrapCompilationError carrying no SourcedError at all, so the
			// only location available is the top-level form's — an unbound
			// reference on line 4 of a loaded file reported line 1, and line
			// 4 appeared nowhere in the text OR in the errors.As chain.
			// engine.go's extractor documents the intent this restores:
			// "the innermost source location ... at the actual error site
			// (e.g., the undefined variable), not the enclosing form".
			return wrapSourcedError(expr.SourceContext(),
				werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "no such local or global binding %q", sym.Key))
		}

		bd := p.env.GetGlobalBinding(gi)
		if bd != nil {
			return p.emitCachedBindingLoad(expr.SourceContext(), sym, bd)
		}
		// Binding not yet defined at compile time — fall back to runtime resolution
		i := p.template.MaybeAppendLiteral(gi)
		p.AppendOperations(
			machine.NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(i),
		)
		return nil
	}

	// Sym has scopes (from macro expansion); use scope-aware binding resolution.
	// Check for a local binding with matching scopes FIRST: a binding
	// co-introduced by the same template carries the same intro scope as this
	// reference, so it must shadow the pinned/global fallbacks below (R1 fix —
	// previously the ResolvedBinding pin short-circuited ahead of this check).
	li := p.env.GetLocalIndex(sym, syntax.ScopesOf(symbolScopes))
	if li != nil {
		// Found a local binding with matching scopes
		p.emitLocalLoad(li)
		return nil
	}

	// No LOCAL binding shadows — but a template that introduces a top-level
	// `define` creates a GLOBAL binding, which GetLocalIndex above cannot see. Such
	// a binder is co-introduced by this very expansion and must shadow the pin for
	// the same reason a co-introduced `let` binder does: R7RS §4.3 renames an
	// identifier a transformer inserts a binding for throughout its scope, so a
	// same-named global that merely happened to exist when the transformer compiled
	// must not reach it. This arm is the R1 invariant's global half; without it a
	// (define-syntax m … (begin (define (rec …) …) (define export rec))) bound
	// `export` to the definition site's `rec`.
	//
	// coIntroducedByExpansion IS WHAT KEEPS THIS FROM SWALLOWING THE PIN: a ∅-scoped
	// global qualifies for nothing and falls through to the pin. Ranking among
	// qualifying candidates is scopedBestOf's, not this arm's: the {intro}-scoped
	// co-introduced binder beats the ∅-scoped user global on cardinality without any
	// new comparison.
	//
	// Global-only by construction. GetBinding would search locals first, and
	// emitting a cached-binding load for a local would be wrong. A nil GetLocalIndex
	// does imply GetBinding's local phase is empty today (both walk resolveLocal
	// with the same scopedBestOf and ScopesCompatible filter), but that is a
	// non-local invariant, so use the accessor that cannot depend on it — the same
	// one the empty-scope path above uses.
	// Read the binding off the (Env, Slot) the index already names, via
	// GetOwnGlobalBinding's pinned-slot path, rather than GetGlobalBinding's
	// re-walk: one lock, no re-hash, and no chance of a second walk landing on a
	// different slot than the one whose scopes the guard below inspects. The
	// Env != nil guard covers a DEFERRED index (NewDeferredGlobalIndex), which
	// resolveGlobal never produces but the type does not forbid.
	coGI := p.env.GetGlobalIndexWithScopes(sym, syntax.ScopesOf(symbolScopes))
	if coGI != nil && coGI.Env != nil {
		coBD := coGI.Env.GetOwnGlobalBinding(coGI)
		if coIntroducedByExpansion(coBD) {
			return p.emitCachedBindingLoad(expr.SourceContext(), sym, coBD)
		}
	}

	// No co-introduced binder either. A free template identifier carrying a
	// definition-time GlobalIndex (cross-library hygiene / ER rename) resolves to
	// that binding now, before the library-scope and scoped-global fallbacks.
	if p.tryResolvedBinding(expr) {
		return nil
	}

	// Library scope lookup takes priority over general scope matching.
	// When a macro's free identifier carries a library scope, we redirect
	// to the library's env via the TLE scope registry. This must come
	// before GetBinding because the outer expansion of
	// define-library may create placeholder bindings with empty scopes
	// in the caller env, which would falsely match any reference scopes.
	libGI := p.env.GetGlobalIndexFromLibraryScopes(sym, symbolScopes)
	if libGI != nil {
		// The syntactic-keyword refusal is applied HERE rather than by routing
		// through emitCachedBindingLoad: the value must be read at execution
		// time (see below), and a cached-binding load reads it now. Classifying
		// the binding is compile-time work, so it can be done here without
		// disturbing that. Env is nil for a DEFERRED index, which resolveGlobal
		// never produces but the type does not forbid; a nil binding is not a
		// refusal (refuseCompileTimeMeaning says why).
		if libGI.Env != nil {
			libBD := libGI.Env.GetOwnGlobalBinding(libGI)
			err := refuseCompileTimeMeaning(expr.SourceContext(), sym, libBD)
			if err != nil {
				return err
			}
		}
		// Use runtime resolution via GlobalIndex so the binding value
		// is read at execution time (after the library template runs).
		i := p.template.MaybeAppendLiteral(libGI)
		p.AppendOperations(
			machine.NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(i),
		)
		return nil
	}

	// Check global binding with scope matching
	globalBinding := p.env.GetBinding(sym, syntax.ScopesOf(symbolScopes))
	if globalBinding != nil {
		// It must be a global binding (since local lookup failed).
		// globalBinding was found by GetBinding — use it directly
		// as a cached binding to skip runtime map/lock overhead.
		return p.emitCachedBindingLoad(expr.SourceContext(), sym, globalBinding)
	}

	// No binding found that matches the scopes. Same stamp, same reason, as
	// the empty-scope arm above: this is the scoped half of one defect.
	return wrapSourcedError(expr.SourceContext(),
		werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "no such binding %q with compatible scopes", sym.Key))
}

// refuseCompileTimeMeaning is the syntactic-keyword refusal every arm of
// CompileSymbol that reaches a resolved global must apply. A syntax compiler or
// primitive expander (BindingTypePrimitive) and a macro transformer
// (BindingTypeSyntax) are compile-time meanings, and emitting a load of one
// leaks it into the value world (R7RS §4.1.1/§4.3.1 — a syntactic keyword is
// not a variable). BindingType is the authority; the frame the walk landed in
// is not. Globals only ever carry Variable, Primitive or Syntax (Unknown is
// local-slot scaffolding), so the negative form refuses exactly the two handler
// tags.
//
// A nil binding is NOT a refusal. It means the name has no compile-time binding
// yet, so there is nothing to classify — the arm that reads one falls through
// to runtime resolution, which is the shape the library-scope arm needs.
// emitCachedBindingLoad's own callers have already established non-nil, so this
// tolerance does not widen anything there.
//
// No runtime-side twin is needed: handlers are seated exclusively at owner
// construction, which precedes every user compilation against that owner, so a
// name that is unbound at compile time cannot become handler-bound later and a
// deferred index can never resolve to one.
//
// It RAISES on the arms that reach a resolved global, because there the
// ordinary path IS what resolved to the handler. tryResolvedBinding calls it
// for the CLASSIFICATION only and discards the error, falling through to
// ordinary resolution — that arm is a fallback with a path left to try, and a
// dialect-removed form must end at ErrNoSuchBinding rather than here. Calling
// it there rather than re-testing the tag inline is what keeps one authority
// for "this is a compile-time meaning".
//
// src stamps the IDENTIFIER's own location, for the reason the unbound-
// reference arms above give: without it the error reaches
// wrapCompilationError carrying no SourcedError, and the enclosing form's line
// is reported instead — measured, a library body's (list if) at 7:12 reported
// 6:5, and the top-level twin reported 1:0.
//
// TestCompileSymbolRefusesCompileTimeMeaningOnEveryGlobalArm is the ratchet
// that keeps a new arm from skipping it.
func refuseCompileTimeMeaning(src *syntax.SourceContext, sym *values.Symbol, bd *environment.Binding) error {
	if bd == nil {
		return nil
	}
	if bd.BindingType() != environment.BindingTypeVariable {
		return wrapSourcedError(src, werr.WrapForeignErrorf(werr.ErrSyntacticKeywordAsVariable,
			"compile: syntactic keyword %q used as a variable", sym.Key))
	}
	return nil
}

// emitCachedBindingLoad emits a load of an already-resolved global binding,
// caching the *Binding on the template so the VM skips the runtime map lookup
// and its lock, and applying refuseCompileTimeMeaning first.
//
// CompileSymbol reaches a resolved global on SIX arms, not three, and every one
// of them applies refuseCompileTimeMeaning. Three funnel through here — the
// empty-scope global, the co-introduced binder, and the scoped global. The
// library-scope arm CANNOT: it must keep runtime resolution because the library
// template has not run when the reference compiles, and a cached-binding load
// would read the value too early, so it calls the refusal directly. That is why
// the refusal is a free function rather than a step inside this one. The
// remaining two are tryResolvedBinding's own resolved-pin paths, which call it
// for the classification and fall through rather than raising.
//
// (The "there are three" wording this comment carried was false for the whole
// life of the library arm, and a library body's (list if) evaluated to
// (#<primitive-expander:if>) behind it.)
//
// Callers must have established that bd is a GLOBAL binding. A local resolves by
// slot (LoadLocalByLocalIndex) and caching its *Binding would be wrong twice over:
// the wrong opcode, and a pointer into a []Binding that EnsureLocalBinding can
// reallocate.
func (p *CompileTimeContinuation) emitCachedBindingLoad(src *syntax.SourceContext, sym *values.Symbol, bd *environment.Binding) error {
	err := refuseCompileTimeMeaning(src, sym, bd)
	if err != nil {
		return err
	}
	idx := p.template.AppendCachedBinding(bd)
	p.AppendOperations(
		machine.NewOperationLoadCachedBinding(idx),
	)
	return nil
}

// tryResolvedBinding emits a load for the symbol's pre-resolved global binding,
// if it carries one, returning true when it consumed the symbol. Free
// identifiers in macro templates carry their definition-time
// *environment.GlobalIndex (cross-library hygiene; ER renames) so they resolve
// to the definition-site binding regardless of the use-site library context.
//
// This is a FALLBACK: callers consult it only AFTER scope-set local resolution
// (GetLocalIndex), so a binding co-introduced by the same template shadows the
// recorded global. A ResolvedBinding that is not a *GlobalIndex (or is nil)
// returns false so the caller falls through to normal resolution.
// See plans/2026-06-15-macro-hygiene-global-shadow-fix.local.md (Change 2).
func (p *CompileTimeContinuation) tryResolvedBinding(expr *syntax.SyntaxSymbol) bool {
	if expr.ResolvedBinding == nil {
		return false
	}
	gi, ok := expr.ResolvedBinding.(*environment.GlobalIndex)
	if !ok || gi == nil {
		return false
	}
	bd := gi.Env.GetOwnGlobalBinding(gi)
	if bd != nil {
		// A pin resolving to a compile-time meaning is a phase confusion: refuse
		// the load so the caller falls through to ordinary resolution — for a
		// dialect-removed form that chain ends at ErrNoSuchBinding (a phase-1
		// primitive expander is not a phase-0 candidate), the documented
		// removed-form contract (dialect.go). The old compileTimeHandler type
		// assertion missed a pinned macro TRANSFORMER (a plain machine closure),
		// which leaked by this path.
		//
		// The CLASSIFICATION is refuseCompileTimeMeaning's, called for its
		// verdict and not for its error: this arm falls through where the
		// resolved-global arms raise, because it still has an ordinary path to
		// try. Re-testing the tag inline here is what let a change to the
		// classification land on one of the two and not the other, and it kept
		// this arm out of the ratchet's reach set.
		if refuseCompileTimeMeaning(expr.SourceContext(), expr.Sym, bd) != nil {
			return false
		}
		idx := p.template.AppendCachedBinding(bd)
		p.AppendOperations(
			machine.NewOperationLoadCachedBinding(idx),
		)
		return true
	}
	// Binding not yet defined at compile time — fall back to runtime resolution.
	i := p.template.MaybeAppendLiteral(gi)
	p.AppendOperations(
		machine.NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(i),
	)
	return true
}

// CompileMeta compiles a meta expression.
func (p *CompileTimeContinuation) CompileMeta(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	rest, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAPair, "%T is not a pair", expr)
	}
	// Get the expand environment and compile expressions in it
	metaEnv := p.env.Expand()
	metaCont := NewCompileTimeContinuation(p.template, metaEnv, p.evaluator)
	metaCont.SetInlineThreshold(p.inlineThreshold)
	err := metaCont.compileExpressionList(ctctx, rest)
	if err != nil {
		return werr.WrapForeignErrorf(err, "failed to compile meta")
	}
	return nil
}

func (p *CompileTimeContinuation) compileExpressionList(ctctx CompileTimeCallContext, expr *syntax.SyntaxPair) error {
	if !expr.IsList() {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "expected a list of expressions, got %T", expr)
	}
	tail, err := syntax.SyntaxForEach(ctctx.ctx, expr, func(_ context.Context, _ int, hasNext bool, v syntax.SyntaxValue) error {
		ctctx0 := ctctx
		if hasNext {
			ctctx0 = ctctx.NotInTail()
		}
		err := p.CompileExpression(ctctx0, v)
		if err != nil {
			return werr.WrapForeignErrorf(err, "failed to compile expression list")
		}
		return nil
	})
	if err != nil {
		return werr.WrapForeignErrorf(err, "failed to compile expression list")
	}
	if !syntax.IsSyntaxEmptyList(tail) {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "expected expression list, got %T", tail)
	}
	return nil
}

// CompileExpression compiles a general expression.
// Pushes the expression's source context onto the source stack so that all
// operations emitted during compilation (including infrastructure ops like
// Branch and Push) are tagged with the source location.
func (p *CompileTimeContinuation) CompileExpression(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	p.pushSource(expr.SourceContext())
	defer p.popSource()

	// Validate the expression first
	// Pass the environment so validation can check for local variable shadowing
	// of special forms (R7RS §4.2.2)
	result := validate.ValidateExpression(ctctx.ctx, p.env, expr)
	if !result.Ok() {
		return p.wrapCompilationError(
			werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s", result.Error()))
	}
	// Compile the validated form
	err := p.compileValidated(ctctx, result.Expr)
	if err != nil {
		return p.wrapCompilationError(err)
	}
	return nil
}

// validateQuotedLiteral walks a quoted literal value to detect circular pair
// structures from datum labels (e.g., '#0=(a . #0#)), returning an error when it
// finds one. The value returned is the literal to compile: the original when the
// walk changed nothing, a rebuilt copy otherwise (both callers use the returned
// value, not the argument).
//
// The cdr spine is walked iteratively; only cars recurse. See
// validateQuotedLiteralWithVisited for why.
func (p *CompileTimeContinuation) validateQuotedLiteral(v values.Value) (values.Value, error) {
	return p.validateQuotedLiteralWithVisited(v, nil)
}

func (p *CompileTimeContinuation) validateQuotedLiteralWithVisited(
	v values.Value, visited values.MapSet[values.Value],
) (values.Value, error) {
	switch val := v.(type) {
	case *values.Symbol:
		return val, nil
	case *values.Pair:
		if val == nil {
			return nil, nil
		}
		if visited == nil {
			visited = values.NewMapSet[values.Value](0)
		}
		// Walk the cdr spine iteratively. Recursing per cdr made list *length*
		// into Go stack depth, so a multi-million-element quoted literal
		// overflowed the host stack at compile time — the same length-vs-depth
		// defect fixed in syntax.UnwrapAllShared. Cars still recurse: their
		// depth is nesting, which the parser already bounds.
		//
		// Spine marks stay in visited for the whole walk and are removed on the
		// way out, preserving the original path-scoped semantics: a reference
		// back into the live spine is a cycle, while structure shared between
		// completed sibling subtrees is not.
		var (
			spine   []*values.Pair
			cars    []values.Value
			tail    values.Value
			changed bool
		)
		unwind := func() {
			for _, c := range spine {
				visited.Remove(c)
			}
		}
		cur := val
		for {
			dup := visited.ContainsOne(cur)
			if dup {
				unwind()
				return nil, werr.WrapForeignErrorf(
					werr.ErrInvalidSyntax,
					"compile: circular datum label in quoted literal",
				)
			}
			visited.Add(cur)
			spine = append(spine, cur)

			car, err := p.validateQuotedLiteralWithVisited(cur.Car(), visited)
			if err != nil {
				unwind()
				return nil, err
			}
			cars = append(cars, car)
			if car != cur.Car() {
				changed = true
			}

			next, isPair := cur.Cdr().(*values.Pair)
			if !isPair || next == nil {
				tail, err = p.validateQuotedLiteralWithVisited(cur.Cdr(), visited)
				if err != nil {
					unwind()
					return nil, err
				}
				if tail != cur.Cdr() {
					changed = true
				}
				break
			}
			cur = next
		}
		unwind()
		if !changed {
			return val, nil
		}
		out := tail
		for i := range slices.Backward(spine) {
			out = values.NewCons(cars[i], out)
		}
		return out, nil
	case *values.Vector:
		if val == nil || val.Length() == 0 {
			return val, nil
		}
		if visited == nil {
			visited = values.NewMapSet[values.Value](0)
		}
		dup := visited.ContainsOne(val)
		if dup {
			return nil, werr.WrapForeignErrorf(
				werr.ErrInvalidSyntax,
				"compile: circular datum label in quoted literal",
			)
		}
		visited.Add(val)
		changed := false
		newElements := make([]values.Value, val.Length())
		for i, elem := range val.Elems() {
			validated, err := p.validateQuotedLiteralWithVisited(elem, visited)
			if err != nil {
				return nil, err
			}
			newElements[i] = validated
			if validated != elem {
				changed = true
			}
		}
		visited.Remove(val)
		if !changed {
			return val, nil
		}
		return values.NewVector(newElements...), nil
	default:
		return v, nil
	}
}

// CompileSelfEvaluating compiles a self-evaluating expression (literal).
func (p *CompileTimeContinuation) CompileSelfEvaluating(_ CompileTimeCallContext, expr syntax.SyntaxValue) error {
	if expr == nil {
		// Load void for nil expressions
		p.AppendOperations(
			machine.NewOperationLoadVoid(),
		)
		return nil
	}
	// Validate quoted literal for circular datum labels.
	// Use UnwrapAll() to fully unwrap syntax values (including vector elements)
	// so that equal? comparisons work correctly on literal vectors.
	val, err := p.validateQuotedLiteral(expr.UnwrapAll())
	if err != nil {
		return err
	}
	// A self-evaluating aggregate (#(1 2 3), #u8(1 2 3)) is a compile-time
	// constant exactly as a quoted one is, and R7RS §4.1.2 makes it immutable.
	// It reaches the same pool and dedups against quoted twins, so it must be
	// marked at its own append or the survivor's immutability depends on which
	// form the compiler saw first.
	li := p.appendConstantLiteral(val)
	p.AppendOperations(
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(li),
	)
	return nil
}

// AppendOperations appends operations tagged with the current source from the source stack.
// Routes through the integer-dispatch code[] path: Wave 1-3 operations become
// direct instructions, everything else goes via machine.OpComplex to the sideTable.
func (p *CompileTimeContinuation) AppendOperations(ops ...machine.Operation) {
	p.template.AppendOperationsWithSource(p.currentSource(), ops...)
}

// appendPushAt emits an OpPush attributed to src rather than to the form
// currently being compiled.
//
// OpPush raises when the expression it is delivering produced any count but one
// value, and that raise is reported at the instruction's own source location.
// For an argument or initializer push, the useful location is the SUBEXPRESSION
// that misbehaved, not the call or binding form containing it: compileValidated
// has already popped the argument's source by the time the push is emitted, so
// without this the diagnostic for `(+ (values 1 2) 3)` points at the `+` call.
// A nil src falls back to the enclosing form.
func (p *CompileTimeContinuation) appendPushAt(src *syntax.SourceContext) {
	if src == nil {
		p.AppendOperations(machine.NewOperationPush())
		return
	}
	p.template.AppendOperationsWithSource(src, machine.NewOperationPush())
}

// emitPatchableSaveContinuation emits a SaveContinuation with a placeholder
// offset of 0. Returns the code[] index for later patching via
// patchSaveContinuationOffset.
func (p *CompileTimeContinuation) emitPatchableSaveContinuation() int {
	idx := p.template.CodeLen()
	p.AppendOperations(machine.NewOperationSaveContinuationOffsetImmediate(0))
	return idx
}

// patchSaveContinuationOffset patches a previously emitted SaveContinuation
// placeholder with the correct relative offset from the placeholder to the
// current position.
func (p *CompileTimeContinuation) patchSaveContinuationOffset(idx int) {
	offset := p.template.CodeLen() - idx
	p.template.PatchInstructionArg(idx, int32(offset))
}

// patchBranchTarget patches a previously emitted branch instruction (Branch,
// BranchOnFalseValue, or SaveContinuation) with the offset to targetIdx.
func (p *CompileTimeContinuation) patchBranchTarget(idx, targetIdx int) {
	offset := targetIdx - idx
	p.template.PatchInstructionArg(idx, int32(offset))
}

func (p *CompileTimeContinuation) pushSource(src *syntax.SourceContext) {
	p.sourceStack = append(p.sourceStack, src)
}

func (p *CompileTimeContinuation) popSource() {
	if len(p.sourceStack) > 0 {
		p.sourceStack = p.sourceStack[:len(p.sourceStack)-1]
	}
}

func (p *CompileTimeContinuation) currentSource() *syntax.SourceContext {
	if len(p.sourceStack) == 0 {
		return nil
	}
	return p.sourceStack[len(p.sourceStack)-1]
}

// wrapCompilationError attaches the current source location to a compilation error.
// If no source context is available, returns the error unchanged.
func (p *CompileTimeContinuation) wrapCompilationError(err error) error {
	if err == nil {
		return nil
	}
	src := p.currentSource()
	if src == nil {
		return err
	}
	return &SourcedError{Source: src, Cause: err}
}
