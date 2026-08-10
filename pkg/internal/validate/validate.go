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
// *ValidatedDefine to scan. So this covers the backward direction only — a
// reference after the define — which is the only direction a shadow needs; a
// forward reference to an internal binding is not evaluated before the define
// runs (R7RS §5.3.2), and the compiler's pre-pass is what makes it resolve.
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
		return &ValidatedSymbol{validatedBase: validatedBase{formName: "@symbol", source: e.SourceContext()}, Symbol: e}
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
// GetBinding panics with werr.ErrAmbiguousBinding on an incomparable scope-set
// tie. That is deliberately not caught: it reaches the compile path's recover
// boundary and surfaces as a CompilationError chaining the sentinel, which is
// the answer an ambiguous identifier is supposed to get.
func headDenotesSpecialForm(env *environment.EnvironmentFrame, symVal *values.Symbol, sym *syntax.SyntaxSymbol) bool {
	if env == nil {
		return true
	}
	ge := env.GlobalEnvironment()
	if ge == nil {
		return true
	}
	q := syntax.ScopesOf(sym.Scopes())
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
	return b == ge.SealedBindingAt(symVal, q, env.PhaseLevel())
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
