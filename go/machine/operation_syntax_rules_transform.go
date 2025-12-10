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
	"skeme/syntax"
	"skeme/values"
)

// OperationSyntaxRulesTransform is a VM operation that performs macro expansion.
//
// Execution context:
//   - Value register: contains clausesWrapper with compiled pattern/template pairs
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
		return nil, values.NewForeignErrorf("syntax-rules: no clauses in value register")
	}

	// Extract from wrapper
	wrapper, ok := clausesVal.(*clausesWrapper)
	if !ok {
		// The value register might have the input if operations aren't running correctly
		// Check if this is actually being called as the second operation
		return nil, values.NewForeignErrorf("syntax-rules: expected clauses wrapper in value register, got %T (PC=%d)", clausesVal, mctx.pc)
	}
	clauses := wrapper.clauses

	// Get the input form from local parameter 0 (transformer is called with input as argument)
	inputVal := mctx.env.GetLocalBindingByIndex(0).Value()
	if inputVal == nil {
		return nil, values.NewForeignErrorf("syntax-rules: invalid input form")
	}

	// Convert input to syntax value if needed
	var input syntax.SyntaxValue
	if stx, ok := inputVal.(syntax.SyntaxValue); ok {
		input = stx
	} else {
		// Wrap raw value in syntax
		input = syntax.NewSyntaxObject(inputVal, nil)
	}

	// Try each clause in order
	for i, clause := range clauses {
		// Try to match the pattern
		err := clause.matcher.Match(input)
		if err == nil {
			// Create a fresh scope for this macro invocation
			// This prevents variable capture between the macro and its use site
			introScope := syntax.NewScope(nil)

			// Expand the template with hygiene support:
			// - Pattern variable substitutions preserve original syntax (with original scopes)
			// - Newly created symbols from template get the intro scope
			// - Free identifiers (like 'if', 'lambda') don't get intro scope
			expanded, err := clause.matcher.ExpandWithIntroScope(clause.template, introScope, clause.freeIds)
			if err != nil {
				return nil, values.WrapForeignErrorf(err, "syntax-rules: expansion error in clause %d", i+1)
			}

			// Set the expanded result as the value
			mctx.SetValue(expanded)
			mctx.pc++ // Important: increment PC to avoid infinite loop
			return mctx, nil
		}
		// If no match, try next clause
	}

	// No clauses matched
	return nil, values.NewForeignErrorf("syntax-rules: no matching clause for input")
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

		case *syntax.SyntaxObject:
			return s.AddScope(scope)

		default:
			// For other syntax types, return as-is
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
	if car := pair.Car(); car != nil {
		if carStx, ok := car.(syntax.SyntaxValue); ok {
			newCarVal := addScopeToSyntaxSkipFreeIds(carStx, scope, freeIds)
			if ncs, ok := newCarVal.(syntax.SyntaxValue); ok {
				newCar = ncs
			}
		}
	}

	// Process cdr
	var newCdr syntax.SyntaxValue
	if cdr := pair.Cdr(); cdr != nil {
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
//   1. Identifiers introduced by the macro (like "tmp" in swap!) get a scope
//      that distinguishes them from identifiers in the macro's use site.
//
//   2. When the compiler resolves variable references, it checks if the binding's
//      scope set is a subset of the reference's scope set (see ScopesMatch).
//
//   3. A user's "tmp" (with scope set {user-scope}) won't match the macro's "tmp"
//      binding (with scope set {user-scope, intro-scope}), because the binding
//      has an extra scope the reference doesn't have.
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
		case *syntax.SyntaxObject:
			return s.AddScope(scope)
		default:
			// For other syntax types, return as-is
			return stx
		}
	}

	// For non-syntax values, return as-is
	return val
}
