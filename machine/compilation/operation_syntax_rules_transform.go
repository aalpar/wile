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
	"fmt"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/match"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
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
	binding := p.env.GetBinding(s, scopes)
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
	return p.env.GetBinding(s, scopes)
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
		// Try to match the pattern with R7RS binding checking
		err := clause.Matcher.MatchWithBindingChecker(mc.Context(), input, bindingChecker)
		if err == nil {
			// Create a fresh scope for this macro invocation
			// This prevents variable capture between the macro and its use site
			introScope := syntax.NewScopeWithLabel("intro")

			// Convert freeIds to match.FreeIdResolver map
			freeIds := make(map[string]match.FreeIdResolver, len(clause.FreeIds))
			for k, v := range clause.FreeIds {
				freeIds[k] = v
			}

			expanded, err := clause.Matcher.Expand(clause.Template, match.ExpandOptions{
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
