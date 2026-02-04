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

// operation_syntax_rules_transform.go implements the runtime behavior of syntax-rules.
//
// This VM operation executes when a macro is invoked during expansion. It:
//   1. Retrieves compiled clauses from the value register
//   2. Pops the input form from the eval stack
//   3. Tries each pattern against the input (first match wins, per R7RS)
//   4. Expands the matching template with captured bindings
//   5. Adds an "intro scope" to the expansion for hygiene
//
// Hygiene Implementation (Flatt's "sets of scopes" model):
//   - Each macro invocation creates a fresh "intro scope"
//   - This scope is added to ALL identifiers in the macro expansion
//   - When resolving variables, the scope sets must be compatible:
//     bindingScopes ⊆ useScopes (see syntax/scope_utils.go:ScopesMatch)
//   - This prevents a macro's internal "tmp" from capturing a user's "tmp"
//
// Example: The swap! macro introduces a "tmp" variable:
//   (define-syntax swap!
//     (syntax-rules ()
//       ((swap! x y) (let ((tmp x)) (set! x y) (set! y tmp)))))
//
// Without hygiene, (let ((tmp 5)) (swap! a b) tmp) would return b, not 5.
// With hygiene, the macro's "tmp" gets an intro scope that distinguishes it
// from the user's "tmp", so the user's "tmp" is correctly returned.
//
// Reference: "Binding as Sets of Scopes" (Flatt, 2016)

import (
	"context"
	"fmt"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/match"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
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
	binding := p.env.GetBindingWithScopes(s, scopes)
	return binding != nil
}

// GetBinding returns the binding for the given symbol with scopes.
// This is used for R7RS §4.3.2 auxiliary syntax hygiene: we compare the
// actual bindings (not just whether they exist) to determine if a literal
// matches. Two identifiers match only if they have the same binding.
func (p *envBindingChecker) GetBinding(sym string, scopes []*syntax.Scope) any {
	if p.env == nil {
		return nil
	}
	s := values.NewSymbol(sym)
	binding := p.env.GetBindingWithScopes(s, scopes)
	return binding
}

// OperationSyntaxRulesTransform is a VM operation that performs macro expansion.
//
// Execution context:
//   - wrt register: contains clausesWrapper with compiled pattern/template pairs
//   - Local parameter 0: contains the input form (the macro invocation)
//
// The operation is part of the transformer closure created by CompileSyntaxRules.
type OperationSyntaxRulesTransform struct{}

func NewOperationSyntaxRulesTransform() *OperationSyntaxRulesTransform {
	return &OperationSyntaxRulesTransform{}
}

func (p *OperationSyntaxRulesTransform) Apply(ctx context.Context, mctx *MachineContext) (*MachineContext, error) {
	// Get the clauses from the value register
	clausesVal := mctx.GetValue()
	if clausesVal == nil {
		return nil, mctx.Error("syntax-rules: no clauses in value register")
	}

	// Extract from wrapper
	wrapper, ok := clausesVal.(*clausesWrapper)
	if !ok {
		// The value register might have the input if operations aren't running correctly
		// Check if this is actually being called as the second operation
		return nil, mctx.Error(fmt.Sprintf("syntax-rules: expected clauses wrapper in value register, got %T (PC=%d)", clausesVal, mctx.pc))
	}
	clauses := wrapper.clauses

	// Get the input form from local parameter 0 (transformer is called with input as argument)
	inputVal := mctx.env.GetLocalBindingByIndex(0).Value()
	if inputVal == nil {
		return nil, mctx.Error("syntax-rules: invalid input form")
	}

	// Convert input to syntax value if needed
	var input syntax.SyntaxValue
	if stx, ok := inputVal.(syntax.SyntaxValue); ok {
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
	if inputPair, ok := input.(*syntax.SyntaxPair); ok {
		car := inputPair.Car()
		if car != nil {
			if sym, ok := car.(*syntax.SyntaxSymbol); ok {
				macroName = sym.Sym.Key
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
	// than mctx.env (the macro definition-time environment). This is critical for
	// checking if identifiers like => are bound by enclosing forms (like lambda
	// from let expansion) at the macro use site.
	bindingEnv := mctx.env
	if mctx.expanderCtx != nil && mctx.expanderCtx.Env() != nil {
		bindingEnv = mctx.expanderCtx.Env()
	}
	bindingChecker := &envBindingChecker{env: bindingEnv}

	// Try each clause in order
	for i, clause := range clauses {
		// Try to match the pattern with R7RS binding checking
		err := clause.matcher.MatchWithBindingChecker(input, bindingChecker)
		if err == nil {
			// Create a fresh scope for this macro invocation
			// This prevents variable capture between the macro and its use site
			introScope := syntax.NewScope()

			// Convert freeIds from map[string]*environment.GlobalIndex to map[string]any
			// This is needed because the match package uses any to avoid circular imports
			freeIdsAny := make(map[string]any, len(clause.freeIds))
			for k, v := range clause.freeIds {
				freeIdsAny[k] = v
			}

			// Expand the template with hygiene support:
			// - Pattern variable substitutions preserve original syntax (with original scopes)
			// - Newly created symbols from template get the intro scope
			// - Free identifiers (like 'if', 'lambda') don't get intro scope but carry resolved bindings
			// - Use-site context is used for newly created syntax objects (better error messages)
			// - Origin info tracks the macro expansion chain
			// - Pattern variable syntax enables nested macro hygiene via scope comparison
			expanded, err := clause.matcher.ExpandWithPatternVarSyntax(
				clause.template, introScope, freeIdsAny, useSiteCtx, origin, clause.patternVarSyntax)
			if err != nil {
				return nil, mctx.WrapError(err, fmt.Sprintf("syntax-rules: expansion error in clause %d", i+1))
			}

			// Set the expanded result as the value
			mctx.SetValue(expanded)
			mctx.pc++ // Important: increment PC to avoid infinite loop
			return mctx, nil
		}
		// If no match, try next clause
	}

	// No clauses matched
	return nil, mctx.Error("syntax-rules: no matching clause for input")
}

func (p *OperationSyntaxRulesTransform) String() string {
	return "SyntaxRulesTransform"
}

func (p *OperationSyntaxRulesTransform) SchemeString() string {
	return "#<operation:syntax-rules-transform>"
}

func (p *OperationSyntaxRulesTransform) EqualTo(other values.Value) bool {
	_, ok := other.(*OperationSyntaxRulesTransform)
	return ok
}

func (p *OperationSyntaxRulesTransform) IsVoid() bool {
	return false
}

// addScopeToSyntaxSkipFreeIds recursively adds an intro scope to syntax objects,
// but SKIPS identifiers that are in the freeIds set.
//
// This is the key operation for implementing hygiene in Flatt's "sets of scopes" model,
// with support for recursive macros.
//
// Free identifiers (identifiers that are NOT pattern variables) refer to bindings
// outside the macro - like the macro's own name in a recursive call, or references
// to other macros/functions like "if", "let", etc. These should NOT get the intro
// scope because they need to resolve to their original bindings.
//
// Identifiers introduced by the macro (like "tmp" in swap!) DO get the intro scope,
// which distinguishes them from identifiers at the macro's use site.
func addScopeToSyntaxSkipFreeIds(val values.Value, scope *syntax.Scope, freeIds map[string]struct{}) values.Value {
	if val == nil {
		return nil
	}

	// If it's a syntax value, process it
	if stx, ok := val.(syntax.SyntaxValue); ok {
		switch s := stx.(type) {
		case *syntax.SyntaxSymbol:
			// Check if this symbol is a free identifier
			sym := s.Unwrap()
			if symVal, ok := sym.(*values.Symbol); ok {
				if _, isFree := freeIds[symVal.Key]; isFree {
					// Free identifier - don't add intro scope
					return s
				}
			}
			// Not a free identifier - add the scope
			return s.AddScope(scope)

		case *syntax.SyntaxPair:
			// For pairs, we need to recursively process car and cdr
			// while skipping free identifiers
			return addScopeToPairSkipFreeIds(s, scope, freeIds)

		default:
			// Other syntax types (SyntaxObject, etc.) don't need scopes
			return stx
		}
	}

	// For non-syntax values, return as-is
	return val
}

// addScopeToPairSkipFreeIds recursively adds scope to a pair structure,
// skipping free identifiers.
func addScopeToPairSkipFreeIds(pair *syntax.SyntaxPair, scope *syntax.Scope, freeIds map[string]struct{}) syntax.SyntaxValue {
	if pair == nil || syntax.IsSyntaxEmptyList(pair) {
		return pair
	}

	// Process car
	var newCar syntax.SyntaxValue
	car := pair.Car()
	if car != nil {
		if carStx, ok := car.(syntax.SyntaxValue); ok {
			newCarVal := addScopeToSyntaxSkipFreeIds(carStx, scope, freeIds)
			if ncs, ok := newCarVal.(syntax.SyntaxValue); ok {
				newCar = ncs
			}
		}
	}

	// Process cdr
	var newCdr syntax.SyntaxValue
	cdr := pair.Cdr()
	if cdr != nil {
		if cdrStx, ok := cdr.(syntax.SyntaxValue); ok {
			newCdrVal := addScopeToSyntaxSkipFreeIds(cdrStx, scope, freeIds)
			if ncs, ok := newCdrVal.(syntax.SyntaxValue); ok {
				newCdr = ncs
			}
		}
	}

	// Create new pair with processed car and cdr
	// Preserve source context from original pair
	return syntax.NewSyntaxCons(newCar, newCdr, pair.SourceContext())
}

// addScopeToSyntax recursively adds an intro scope to all syntax objects in a tree.
// This is the legacy version that adds scope to ALL identifiers.
//
// This is the key operation for implementing hygiene in Flatt's "sets of scopes" model.
// By adding a fresh scope to every identifier in the macro expansion, we ensure that:
//
//  1. Identifiers introduced by the macro (like "tmp" in swap!) get a scope
//     that distinguishes them from identifiers in the macro's use site.
//
//  2. When the compiler resolves variable references, it checks if the binding's
//     scope set is a subset of the reference's scope set (see ScopesMatch).
//
//  3. A user's "tmp" (with scope set {user-scope}) won't match the macro's "tmp"
//     binding (with scope set {user-scope, intro-scope}), because the binding
//     has an extra scope the reference doesn't have.
//
// The AddScope method on each syntax type creates a new syntax object with the
// additional scope - syntax objects are immutable for this reason.
func addScopeToSyntax(val values.Value, scope *syntax.Scope) values.Value {
	if val == nil {
		return nil
	}

	// If it's a syntax value, add the scope
	if stx, ok := val.(syntax.SyntaxValue); ok {
		// Check for specific syntax types that have AddScope method
		switch s := stx.(type) {
		case *syntax.SyntaxSymbol:
			return s.AddScope(scope)
		case *syntax.SyntaxPair:
			return s.AddScope(scope)
		default:
			// Other syntax types (SyntaxObject, etc.) don't need scopes
			return stx
		}
	}

	// For non-syntax values, return as-is
	return val
}
