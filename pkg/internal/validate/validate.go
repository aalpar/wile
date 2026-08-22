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

package validate

import (
	"context"
	"errors"
	"fmt"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// ValidateExpression validates a syntax expression and returns
// a validated form or a list of errors.
// The env parameter provides the environment context for checking local variable
// shadowing of special forms (R7RS §4.2.2).
func ValidateExpression(ctx context.Context, env *environment.EnvironmentFrame, expr syntax.SyntaxValue) *ValidationResult {
	result := &ValidationResult{}
	validated := validateExpr(ctx, env, expr, result)
	result.Expr = validated
	result.finalizeStability()
	return result
}

// validateBodySlice validates a contiguous slice of elements as body expressions.
// Returns (body, true) if all elements validated, (nil, false) if any failed.
func validateBodySlice(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	elements []syntax.SyntaxValue,
	start int,
	result *ValidationResult,
) ([]ValidatedExpr, bool) {
	var body []ValidatedExpr
	bodyEnv := env
	for i := start; i < len(elements); i++ {
		expr := validateExpr(ctx, bodyEnv, elements[i], result)
		if expr != nil {
			body = append(body, expr)
			bodyEnv = bindBodyDefineNames(bodyEnv, expr)
		}
	}
	if len(body) != len(elements)-start {
		return nil, false
	}
	return body, true
}

// bindBodyDefineNames extends env with the names an ALREADY-VALIDATED body
// expression defines, so a later expression in the same body sees them as local
// variables. Without it an internal define cannot shadow a special form
// (R7RS §4.2.2, §4.3): (let () (define quote (lambda (x) (* x x x))) (quote 9))
// read `quote` as the special form and yielded 9.
//
// This is INCREMENTAL where its compile-side analogue
// (compilation.predeclareDefineFromValidatedRecursive) is a PRE-pass over the
// whole body. A pre-pass is impossible here: validation is what PRODUCES the
// *ValidatedDefine to scan. So it covers the backward direction only — a
// reference AFTER the define.
//
// THE FORWARD DIRECTION IS A KNOWN, UNCLOSED HOLE, not a case the compiler
// rescues. R7RS §5.3.2 makes an internal define's region the whole body, so in
// (define (f) (define (g) (quote 9)) (define quote cube) (g)) the `quote`
// inside g denotes the local and 729 is the meaning; Wile yields 9. The
// compiler's pre-pass cannot repair it: it creates the VARIABLE, but validation
// has already frozen the head into a *ValidatedQuote, and nothing downstream
// re-asks. Closing it needs a name-collecting scan over the raw body syntax
// before any element is validated, which is a separate change. Master answers 9
// here too, so this is not a regression.
//
// The nil-env guard is required, not defensive: createChildEnvWithSymbols
// bottoms out in NewEnvironmentFrameWithParent, which PANICS on a nil parent,
// and this package's callers routinely validate with no environment at all.
func bindBodyDefineNames(env *environment.EnvironmentFrame, expr ValidatedExpr) *environment.EnvironmentFrame {
	if env == nil {
		return nil
	}
	switch v := expr.(type) {
	case *ValidatedDefine:
		return createChildEnvWithSymbols(env, []*syntax.SyntaxSymbol{v.Name()})

	case *ValidatedBegin:
		// define-values and friends expand to (begin (define …) …), so the
		// defines a body actually introduces can sit one level down.
		for _, sub := range v.Body() {
			env = bindBodyDefineNames(env, sub)
		}
		return env
	}
	return env
}

func validateExpr(ctx context.Context, env *environment.EnvironmentFrame, expr syntax.SyntaxValue, result *ValidationResult) ValidatedExpr {
	switch e := expr.(type) {
	case *syntax.SyntaxPair:
		// Empty list '() is accepted here as a self-evaluating literal.
		// R7RS §4.1.3 makes () a syntax error (a combination needs at least one
		// subexpression); Wile deliberately admits it.
		if e.IsEmptyList() {
			return newLiteralExpr(e.SourceContext(), e)
		}
		return validateForm(ctx, env, e, result)
	case *syntax.SyntaxSymbol:
		return &ValidatedSymbol{formName: "@symbol", source: e.SourceContext(), Symbol: e}
	case *syntax.SyntaxObject:
		return validateSyntaxObject(e, result)
	default:
		// Self-evaluating: numbers, strings, booleans, etc.
		return newLiteralExpr(nil, expr)
	}
}

func validateSyntaxObject(obj *syntax.SyntaxObject, result *ValidationResult) ValidatedExpr {
	wrapped := obj.Unwrap()
	switch wrapped.(type) {
	case *values.Symbol:
		// This shouldn't happen - symbols should be SyntaxSymbol, not SyntaxObject
		// But handle it defensively
		result.addError(obj.SourceContext(), "expression", "unexpected symbol wrapped in SyntaxObject")
		return nil
	default:
		// Self-evaluating literal wrapped in syntax
		return newLiteralExpr(obj.SourceContext(), obj)
	}
}

func validateForm(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	// Get the first element to determine the form type
	car := pair.SyntaxCar()

	// Check if it's a special form by looking at the head
	sym, ok := car.(*syntax.SyntaxSymbol)
	if ok {
		symVal, ok := sym.Unwrap().(*values.Symbol)
		if ok {
			// The registry answers CANDIDACY by name; headDenotesSpecialForm
			// answers whether the head really denotes that form here, by the
			// binding it resolves to. The name test comes first so an ordinary
			// call pays no resolution.
			spec := forms.RegistryFor(env).Lookup(symVal.Key)
			if spec != nil && spec.Validate != nil && headDenotesSpecialForm(env, symVal, sym) {
				expr := spec.Validate(ctx, env, pair, result)
				if expr != nil {
					// Override formName only for passthrough forms (prefixed with "@")
					// that haven't been given a proper form name by the validator.
					// This allows validators like validateNamedLet to return a type
					// with a different formName than the keyword (e.g., *ValidatedLet
					// with formName "letrec" for the keyword "let").
					fn := expr.FormName()
					if fn == "" || fn[0] == '@' {
						expr.SetFormName(symVal.Key)
					}
				}
				return expr
			}
		}
	}

	// Not a special form - it's a function call
	return validateCall(ctx, env, pair, result)
}

// headDenotesSpecialForm reports whether a form head the validator's table
// already recognizes BY NAME actually denotes that special form here, rather
// than an ordinary operator that happens to share the spelling.
//
// R7RS §4.3 makes a variable binding shadow a syntactic one, and §5.3.1 lets a
// top-level define do it too. A name cannot answer that question; only the
// binding the head RESOLVES to can. So it is asked as an identity compare: the
// head denotes the form when it resolves to the binding the startup set
// installed, or to no binding at all, and denotes a variable otherwise.
//
// Both resolutions run in ONE call off ONE env, and neither may be hoisted out:
// a local binding is a pointer into a frame's []Binding and EnsureLocalBinding
// can reallocate it, so a *Binding is an identity only inside a window that
// creates no bindings.
//
// THE BindingTypeVariable ARM IS THE LIBRARY CASE, not an optimization.
// (scheme base) exports the special forms themselves, so inside a library body
// that imports it, `define` resolves to an IMPORTED binding — a
// BindingTypePrimitive with no sealed twin at phase 0 — and a rule that only
// compared against the sealed binding would route every library define to
// validateCall. An imported re-export of a special form still denotes the form;
// only a VARIABLE shadows one.
//
// RESOLVING BY SCOPES IS NOT ENOUGH ON ITS OWN, because a special form has no
// binding to pin. Wile keeps a free template identifier hygienic by recording
// its definition-site *GlobalIndex on the symbol (SyntaxSymbol.ResolvedBinding,
// consulted by CompileSymbol.tryResolvedBinding), and collectFreeIdentifiers
// records nothing when the name was unbound at definition time — which is
// exactly the case for a special form. So a template's `set!` arrives here
// carrying only an intro scope, and a user's ∅-scoped top-level (define set! …)
// satisfies ScopesCompatible(∅, {intro}) and would capture it: (inc! n) with
// inc! expanding to (set! v (+ v 1)) silently discarded the assignment.
// referenceReachesBinderDirectly is the guard; see its own comment for the
// three footings it admits and the one it gives up.
//
// A DEFINITION'S OWN HEAD NEEDS NO SPECIAL CASE, and the one that used to be
// here is deleted. The FIRST (define define 3) is a definition — R7RS §5.3.1
// (p.26) contemplates defining a variable whose name is a syntactic keyword, and
// both peers answer 3 — but it reaches that answer through the ARMS ABOVE, not
// through an exemption:
//
//	top level, and a library body   the Predeclared arm: the letrec* pre-scan has
//	                                minted the slot but no define has compiled, so
//	                                the head still means the form
//	a lambda or let body            the nil-binding arm: bindBodyDefineNames is
//	                                incremental, so at the moment this head is
//	                                validated the name is not yet locally bound
//
// Nothing else pins that correspondence, so it is ratcheted per context by the
// three "first ... define of a keyword name binds" rows in
// pkg/wile/binding_model_matrix_test.go. A change to the pre-scan or to
// bindBodyDefineNames can break one context and not the others.
//
// A REDEFINITION IS A CALL, which is what falling through to
// referenceReachesBinderDirectly gives. Once `define` denotes a variable, a
// following (define define 4) applies it and raises ErrNotAProcedure. Wile used
// to answer 4 here, alone: measured 2026-08-11, Petite Chez 10.4.1 and Racket 9.2
// both raise. R7RS does not settle it in terms — §5.3.1's keyword clause licenses
// the first definition and §5.3.2 p.27 says only "it is an error" — so the peers
// are the oracle. Deleting the exemption also removed body_entry_env.go, whose
// only job was to refuse the exemption inside (let ((define f)) (define define 2)).
//
// STILL DIVERGENT, deliberately, and not this function's business: a top-level
// shadow reaching into a lambda body, and both macro-mediated shapes, still
// answer 4 where the peers raise. They turn on referenceReachesBinderDirectly's
// hygiene guard below, which exists to stop a user's (define set! …) capturing a
// template's set!. Relaxing it is its own arc.
//
// GetBinding panics with werr.ErrAmbiguousBinding on an incomparable scope-set
// tie. That is deliberately not caught: it reaches the compile path's recover
// boundary and surfaces as a CompilationError chaining the sentinel, which is
// the answer an ambiguous identifier is supposed to get.
func headDenotesSpecialForm(
	env *environment.EnvironmentFrame,
	symVal *values.Symbol,
	sym *syntax.SyntaxSymbol,
) bool {
	if env == nil {
		return true
	}
	ge := env.GlobalEnvironment()
	if ge == nil {
		return true
	}
	scopes := sym.Scopes()
	q := syntax.ScopesOf(scopes)
	b := env.GetBinding(symVal, q)
	if b == nil {
		// Measured: if, lambda, quote, set!, let, begin, define and
		// with-continuation-mark have no phase-0 binding of any kind. The table
		// is the only thing that knows them, so a miss means the form.
		return true
	}
	if b.BindingType() != environment.BindingTypeVariable {
		return true
	}
	// A slot the letrec* pre-scan minted is a LOCATION with no denotation yet:
	// its define has not been compiled, so the head still means what it meant
	// before the body. This is the P1 arm — see BindingMeta.Predeclared.
	m := b.Meta()
	if m != nil && m.Predeclared {
		return true
	}
	if b == ge.SealedBindingAt(symVal, q, env.PhaseLevel()) {
		return true
	}
	return !referenceReachesBinderDirectly(env, symVal, scopes, b)
}

// referenceReachesBinderDirectly reports whether the reference sym reaches the
// variable binding b at its OWN hygienic footing, rather than only through the
// scope-set widening a macro expansion produces.
//
// It is the hygiene half of headDenotesSpecialForm's identity rule. Subset
// resolution (bindingScopes ⊆ useScopes) is deliberately permissive so a
// template identifier can name an outer global at all; that permissiveness is
// safe for ordinary names because they carry a definition-site pin, and unsafe
// for a special form because it has none. Three footings are admitted, and each
// is one of the shapes the shadow is claimed for (scope sets measured):
//
//   - THE REFERENCE HAS NO SCOPES. A scope-less identifier can only have been
//     written at top level (CompileSymbol's empty-scope fast path states the
//     same invariant), so nothing widened it. Top-level (define set! list) then
//     (set! 1 2 3 4): reference ∅, binder ∅.
//   - THE BINDER HAS SCOPES. It was introduced by a binding form or an expansion
//     the reference is inside, which is the same test CompileSymbol's
//     co-introduced arm makes (coIntroducedByExpansion: "a ∅-scoped global
//     qualifies for nothing"). A define-function-form parameter and an internal
//     define both land here: reference {lambda}, binder {lambda}.
//   - THE REFERENCE NAMES THE LIBRARY THAT BINDS IT. A library env is a flat
//     island whose bindings carry ∅ scopes while every identifier in its body
//     carries the library scope, so the arm above can never fire there.
//     Redirecting through that scope (the same lookup CompileSymbol's
//     library-scope arm uses) asks the stronger question, and a template
//     identifier substituted with ANOTHER library's definition-site scopes
//     cannot answer it. Imported rename: reference {lambda,
//     library:(lib-setbang)}, binder ∅ in that library.
//
// "THE BINDER HAS SCOPES" IS NOT "THE BINDER IS LOCAL", and the difference is
// load-bearing: bindBodyDefineNames fabricates a local frame for a body's
// internal defines, and at the top level of a program (EvalProgram wraps a file
// in one begin, whose body validateBegin now threads) that frame holds an
// ordinary ∅-scoped top-level define. Testing locality accepted it and
// (define let 3) went back to capturing every `or` expansion's template `let`.
//
// GIVEN UP: a top-level (define set! list) does NOT shadow inside a lambda body
// ({lambda} vs ∅), because that shape is bit-for-bit identical to the captured
// template identifier ({intro} vs ∅) at this point in the pipeline — same
// cardinalities, same binding type, same imported flag, same owner env. Master
// did not shadow there either, so this is a narrowing of the fix, not a
// regression. Widening it needs the definition-site meaning recorded on the
// symbol the way ResolvedBinding records a bound one.
func referenceReachesBinderDirectly(
	env *environment.EnvironmentFrame,
	symVal *values.Symbol,
	scopes []*syntax.Scope,
	b *environment.Binding,
) bool {
	if len(scopes) == 0 {
		return true
	}
	if len(b.Scopes()) > 0 {
		return true
	}
	libGI := env.GetGlobalIndexFromLibraryScopes(symVal, scopes)
	if libGI == nil || libGI.Env == nil {
		return false
	}
	// Identity, not merely "a binding of that name exists in some named
	// library": the redirect has to land on the SAME binding the ranked probe
	// chose, or it is answering about a different one.
	return libGI.Env.GetOwnGlobalBinding(libGI) == b
}

// collectList converts a syntax list to a slice of elements, reporting whether
// the list is improper. It shares the traversal primitive used by
// syntax.FormParts: a *SyntaxPair's car and cdr are always SyntaxValues (the
// Values array is typed [2]SyntaxValue and SetCar/SetCdr enforce it), so the
// elements need no wrapping and SyntaxForEach's returned tail is non-empty
// exactly when the list is improper.
func collectList(pair *syntax.SyntaxPair) ([]syntax.SyntaxValue, bool) {
	var elements []syntax.SyntaxValue
	tail, _ := pair.SyntaxForEach(context.Background(),
		func(_ context.Context, _ int, _ bool, v syntax.SyntaxValue) error {
			elements = append(elements, v)
			return nil
		})
	return elements, !syntax.IsSyntaxEmptyList(tail)
}

// formPrologue collects list elements, validates the list is proper,
// and checks argument count (excluding the form keyword at elements[0]).
// minArgs and maxArgs define acceptable argument counts.
// Use maxArgs < 0 for unlimited.
func formPrologue(
	pair *syntax.SyntaxPair,
	formName string,
	minArgs, maxArgs int,
	result *ValidationResult,
) (*syntax.SourceContext, []syntax.SyntaxValue, bool) {
	source := pair.SourceContext()

	// FormParts counts the form keyword at element 0; formPrologue's bounds
	// are keyword-exclusive ("arguments"), so shift them by one.
	maxLen := maxArgs
	if maxArgs >= 0 {
		maxLen = maxArgs + 1
	}
	elements, err := syntax.FormParts(pair, formName, minArgs+1, maxLen)
	if err != nil {
		var ae *syntax.FormArityError
		if errors.As(err, &ae) {
			result.addError(source, formName, arityArgMessage(ae))
		} else {
			// Structural failure (improper list); no data to restate.
			result.addError(source, formName, formName+" form must be a proper list")
		}
		return nil, nil, false
	}
	return source, elements, true
}

// arityArgMessage restates a FormArityError in the validator's keyword-exclusive
// "argument" vocabulary — the form keyword is not an argument, so element counts
// drop by one. The wording matches the messages formPrologue emitted before it
// delegated structural checks to syntax.FormParts.
func arityArgMessage(ae *syntax.FormArityError) string {
	argCount := ae.Got - 1
	if ae.Min == ae.Max && ae.Max >= 0 {
		return fmt.Sprintf("%s requires exactly %d argument(s), got %d", ae.Name, ae.Min-1, argCount)
	}
	if ae.Got < ae.Min {
		return fmt.Sprintf("%s requires at least %d argument(s), got %d", ae.Name, ae.Min-1, argCount)
	}
	return fmt.Sprintf("%s requires at most %d argument(s), got %d", ae.Name, ae.Max-1, argCount)
}

// getSourceContext extracts SourceContext from a SyntaxValue if available
func getSourceContext(v syntax.SyntaxValue) *syntax.SourceContext {
	switch sv := v.(type) {
	case *syntax.SyntaxPair, *syntax.SyntaxSymbol, *syntax.SyntaxObject:
		return sv.SourceContext()
	default:
		return nil
	}
}
