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

// operation_syntax_rules_transform.go implements the runtime behavior of syntax-rules.
//
// This VM operation executes when a macro is invoked during expansion. It:
//   1. Retrieves compiled clauses from the value register
//   2. Reads the input form from local parameter 0 (the transformer's argument)
//   3. Tries each pattern against the input (first match wins, per R7RS)
//   4. Expands the matching template with captured bindings
//   5. Adds an "intro scope" to the expansion for hygiene
//
// Hygiene Implementation (Flatt's "sets of scopes" model):
//   - Each macro invocation creates a fresh "intro scope"
//   - This scope is added to the identifiers the TEMPLATE introduces.
//     Identifiers substituted from pattern variables keep the use site's
//     scopes, and a free template identifier that resolved to a local binding
//     at definition time is emitted with those definition-site scopes instead.
//   - When resolving variables, the scope sets must be compatible:
//     bindingScopes ⊆ useScopes (see syntax/scope_utils.go:ScopesMatch)
//   - This prevents a macro's internal "tmp" from capturing a user's "tmp"
//
// Example: The swap! macro introduces a "tmp" variable:
//   (define-syntax swap!
//     (syntax-rules ()
//       ((swap! x y) (let ((tmp x)) (set! x y) (set! y tmp)))))
//
// The capture needs the user's binding to reach the macro's ARGUMENTS, so take
// (define b 99) and (let ((tmp 5)) (swap! tmp b) tmp). Without hygiene the
// template's own (let ((tmp x)) ...) captures the user's "tmp": the (set! x y)
// meant for the user's variable writes the macro's instead, nothing is swapped,
// and the form returns 5. With hygiene the macro's "tmp" gets an intro scope
// that distinguishes it from the user's, the swap happens, and the form
// returns 99.
//
// Reference: "Binding as Sets of Scopes" (Flatt, 2016)

import (
	"errors"
	"fmt"
	"slices"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/match"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// envBindingChecker implements match.BindingChecker for R7RS auxiliary syntax hygiene.
// It checks if an identifier has a lexical binding in the current environment.
type envBindingChecker struct {
	env *environment.EnvironmentFrame
}

// Verify envBindingChecker implements match.BindingChecker
var _ match.BindingChecker = (*envBindingChecker)(nil)

// HasBinding checks if the given symbol with scopes has a lexical binding.
// This is used by the pattern matcher to determine if an input identifier
// should match a pattern literal. Per R7RS §4.3.2, literals match only if
// both have the same lexical binding, or both have no lexical binding.
func (p *envBindingChecker) HasBinding(sym string, scopes []*syntax.Scope) bool {
	if p.env == nil {
		return false
	}
	s := values.NewSymbol(sym)
	binding := p.env.GetBinding(s, syntax.ScopesOf(scopes))
	return binding != nil
}

// GetBinding returns the binding for the given symbol with scopes.
// This is used for R7RS §4.3.2 auxiliary syntax hygiene: we compare the
// actual bindings (not just whether they exist) to determine if a literal
// matches. Two identifiers match only if they have the same binding.
func (p *envBindingChecker) GetBinding(sym string, scopes []*syntax.Scope) *environment.Binding {
	if p.env == nil {
		return nil
	}
	s := values.NewSymbol(sym)
	return p.env.GetBinding(s, syntax.ScopesOf(scopes))
}

// GetLiteralBinding resolves the USE-SITE side of the R7RS §4.3.2 literal
// comparison: the frame's own lexical chain at its own phase, then the
// special-form registry phase, and nowhere else.
func (p *envBindingChecker) GetLiteralBinding(sym string, scopes []*syntax.Scope) (*environment.Binding, bool) {
	return lookupLiteralBinding(p.env, sym, scopes, useSiteFallbackPhases)
}

// useSiteFallbackPhases is the use site's fallback set. It is exactly the
// special-form registry phase, because the use site's phase is a known, exact
// fact: the identifier is being read by code at env.PhaseLevel(), and any other
// phase's binding of the name is a different program's.
//
// Walking every present phase instead breaks phase hermeticity on the literal
// path, in both directions. From a transformer body (phase 1) an ascending walk
// hits phase 0 first, so a user's top-level (define else #f) pre-empts the
// auxiliary binding and refuses cond, case and guard inside EVERY transformer — a
// hard compile error on programs that compiled before. From phase 0, the same
// walk lets a (begin-for-syntax (define else #f)) reach into phase-0 code.
var useSiteFallbackPhases = []environment.Phase{environment.PhaseCompile}

// definitionFallbackPhases is the definition site's fallback set: every phase
// BELOW env's own, descending, and then the special-form registry phase.
//
// A pattern literal is compared at the phase where the macro is USED, which is at
// or below the phase of the form that writes the literal, and which is not known
// when the pin is taken. syntax-rules writes its literals in the macro's own
// definition env, so its use phase is env's own phase and the descent finds
// nothing; a syntax-case inside a (lambda (stx) ...) transformer writes them one
// phase up from where the macro is used, and only the descent reaches the binding
// the use site will resolve. Racket agrees: (define else 5) beside a syntax-case
// macro with an `else` literal answers (ELSE 1), the two identifiers being one
// phase-0 binding, even though the literal is written in phase-1 code.
//
// The registry phase comes LAST so an ordinary binding at a lower phase wins over
// the ambient auxiliary-syntax one. The descent stops at PhaseRuntime: negative
// phases rank for-template bindings, a different axis rather than a lower rung of
// this one, and PresentPhases excludes them for the same reason.
func definitionFallbackPhases(env *environment.EnvironmentFrame) []environment.Phase {
	own := env.PhaseLevel()
	if own <= environment.PhaseRuntime {
		return useSiteFallbackPhases
	}
	q := make([]environment.Phase, 0, int(own-environment.PhaseRuntime)+1)
	for phase := own - 1; phase >= environment.PhaseRuntime; phase-- {
		q = append(q, phase)
	}
	return append(q, environment.PhaseCompile)
}

// lookupLiteralBinding resolves sym for the R7RS §4.3.2 literal comparison: in
// env's own lexical chain at env's own phase, then in each fallback phase in
// order, then the AMBIENT binding of the name — returning the first hit.
//
// The own-frame probe comes first and is load-bearing on its own: a phase VIEW
// has no lexical parent chain, so a phases-only probe cannot see a let-bound
// shadow — it answers the phase-2 auxiliary binding for the input identifier in
// (let ((else #f)) (cond (else 'TOOK-ELSE))), and that control regresses to
// TOOK-ELSE.
//
// Ambient last is load-bearing. An ambient binding — the (ANY, sealed)
// coordinate, which the ranked probe at ANY phase reaches as T3 — is held back
// and returned only when no exact-phase slot of the name exists at env's own
// phase or at any fallback phase. Without the hold-back a phase-1 probe for a
// name that is both ambient and bound by the user at phase 0 would answer the
// ambient binding before the descent had looked at phase 0; a syntax-case
// macro's literal is written at phase 1 and used at phase 0, and the user's
// phase-0 binding must win that pin (TestPatternLiteralRespectsAUseSiteShadow,
// "syntax-case, global shadow"). When the name is not ambient, ambient is nil
// and the comparison is the plain "first hit" it was.
//
// ok is false only for an ambiguous resolution. EnvironmentFrame.GetBinding
// panics with werr.ErrAmbiguousBinding on an equal-cardinality incomparable scope
// tie and there is no other production recover site, so the panic is converted
// into the answer here rather than escaping through the matcher. Anything else
// recovered is re-panicked unchanged.
func lookupLiteralBinding(
	env *environment.EnvironmentFrame,
	sym string,
	scopes []*syntax.Scope,
	fallbacks []environment.Phase,
) (q *environment.Binding, ok bool) {
	if env == nil {
		return nil, true
	}
	defer func() {
		r := recover()
		if r == nil {
			return
		}
		rerr, isErr := r.(error)
		if isErr && errors.Is(rerr, werr.ErrAmbiguousBinding) {
			q = nil
			ok = false
			return
		}
		panic(r)
	}()

	s := values.NewSymbol(sym)
	sq := syntax.ScopesOf(scopes)
	// A detached transient frame carries no store; there is then nothing ambient.
	var ambient *environment.Binding
	store := env.GlobalEnvironment()
	if store != nil {
		ambient = store.AmbientBinding(s, sq)
	}
	q = env.GetBinding(s, sq)
	if q != nil && q != ambient {
		return q, true
	}
	// PresentPhases is consulted only as the guard on AtPhase: a phase the owner
	// has neither instantiated nor bound anything at has nothing to probe, and
	// AtPhase would mint the view as a side effect of a search.
	present := env.PresentPhases()
	for _, phase := range fallbacks {
		if phase == env.PhaseLevel() || !slices.Contains(present, phase) {
			continue
		}
		v := env.AtPhase(phase)
		if v == nil {
			continue
		}
		q = v.GetBinding(s, sq)
		if q != nil && q != ambient {
			return q, true
		}
	}
	return ambient, true
}

// OperationSyntaxRulesTransform is a VM operation that performs macro expansion.
//
// Execution context:
//   - Value register: contains clausesWrapper with compiled pattern/template pairs
//   - Local parameter 0: contains the input form (the macro invocation)
//
// The operation is part of the transformer closure created by CompileSyntaxRules.
type OperationSyntaxRulesTransform struct {
	machine.OperationBase
}

func NewOperationSyntaxRulesTransform() *OperationSyntaxRulesTransform {
	return &OperationSyntaxRulesTransform{
		OperationBase: machine.NewOperationBaseWithGoName("operation:syntax-rules-transform", "SyntaxRulesTransform"),
	}
}

func (p *OperationSyntaxRulesTransform) Apply(mc *machine.MachineContext) (*machine.MachineContext, error) {
	// Get the clauses from the value register
	clausesVal := mc.GetValue()
	if clausesVal == nil {
		return nil, mc.WrapError(werr.ErrInternal, "syntax-rules: no clauses in value register")
	}

	// Extract from wrapper
	wrapper, ok := clausesVal.(*ClausesWrapper)
	if !ok {
		// The value register might have the input if operations aren't running correctly
		// Check if this is actually being called as the second operation
		return nil, mc.WrapError(werr.ErrInternal, fmt.Sprintf("syntax-rules: expected clauses wrapper in value register, got %T (PC=%d)", clausesVal, mc.PC()))
	}
	clauses := wrapper.Clauses

	// Get the input form from local parameter 0 (transformer is called with input as argument)
	inputVal := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	if inputVal == nil {
		return nil, mc.WrapError(werr.ErrInvalidSyntax, "syntax-rules: invalid input form")
	}

	// Convert input to syntax value if needed
	var input syntax.SyntaxValue
	stx, ok := inputVal.(syntax.SyntaxValue)
	if ok {
		input = stx
	} else {
		// Wrap raw value in syntax
		input = syntax.NewSyntaxObject(inputVal, nil)
	}

	// Get the use-site source context for better error reporting
	var useSiteCtx *syntax.SourceContext
	if input != nil {
		useSiteCtx = input.SourceContext()
	}

	// Extract macro name from the input form's car for origin tracking
	macroName := ""
	inputPair, ok := input.(*syntax.SyntaxPair)
	if ok {
		car := inputPair.Car()
		if car != nil {
			sym, ok := car.(*syntax.SyntaxSymbol)
			if ok {
				macroName = sym.Key()
			}
		}
	}

	// Create origin info for tracking macro expansion chains
	// The origin chain is built up as macros expand other macros
	var origin *syntax.OriginInfo
	if macroName != "" {
		// Check if input already has origin info (from a previous macro expansion)
		var parentOrigin *syntax.OriginInfo
		if useSiteCtx != nil && useSiteCtx.Origin != nil {
			parentOrigin = useSiteCtx.Origin
		}
		origin = &syntax.OriginInfo{
			Identifier: macroName,
			Location:   useSiteCtx,
			Parent:     parentOrigin,
		}
	}

	// Create binding checker for R7RS auxiliary syntax hygiene.
	// This allows the pattern matcher to check if an identifier like => or else
	// has been locally bound, in which case it shouldn't match the pattern literal.
	//
	// We use the expander context's environment (the use-site environment) rather
	// than mc.EnvironmentFrame() (the macro definition-time environment). This is critical for
	// checking if identifiers like => are bound by enclosing forms (like lambda
	// from let expansion) at the macro use site.
	bindingEnv := mc.EnvironmentFrame()
	if mc.ExpanderContext() != nil && mc.ExpanderContext().Env() != nil {
		bindingEnv = mc.ExpanderContext().Env()
	}
	bindingChecker := &envBindingChecker{env: bindingEnv}

	// Try each clause in order
	for i, clause := range clauses {
		// clause.Matcher is compiled once at macro-definition time and shared on the
		// macro binding across all expansions, including concurrent ones from SRFI-18
		// threads. Its embedded Matcher's capture/syntax stacks and bindingChecker are
		// mutated per expansion (Expand reads the stack the match populated), so run
		// each expansion on its own clone — one struct allocation; the match stacks are
		// reallocated per match regardless.
		matcher := clause.Matcher.CloneForMatch()
		// Try to match the pattern with R7RS binding checking
		err := matcher.MatchWithBindingChecker(mc.Context(), input, bindingChecker)
		if err == nil {
			// Create a fresh scope for this macro invocation
			// This prevents variable capture between the macro and its use site
			introScope := syntax.NewScopeWithLabel("intro")

			// Convert freeIds to match.FreeIdResolver map
			freeIds := make(map[string]match.FreeIdResolver, len(clause.FreeIds))
			for k, v := range clause.FreeIds {
				freeIds[k] = v
			}

			expanded, err := matcher.Expand(clause.Template, match.ExpandOptions{
				IntroScope:       introScope,
				FreeIds:          freeIds,
				UseSiteCtx:       useSiteCtx,
				Origin:           origin,
				PatternVarSyntax: clause.PatternVarSyntax,
			})
			if err != nil {
				return nil, mc.WrapError(err, fmt.Sprintf("syntax-rules: expansion error in clause %d", i+1))
			}

			// Set the expanded result as the value
			mc.SetValue(expanded)
			mc.IncrPC() // Important: increment PC to avoid infinite loop
			return mc, nil
		}
		// If no match, try next clause
	}

	// No clauses matched
	return nil, mc.WrapError(werr.ErrInvalidSyntax, "syntax-rules: no matching clause for input")
}

func (p *OperationSyntaxRulesTransform) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationSyntaxRulesTransform)
	return machine.SameType(p, v, ok)
}
