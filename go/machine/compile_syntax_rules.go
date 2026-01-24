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

// compile_syntax_rules.go implements R7RS syntax-rules macro compilation.
//
// This file is part of the macro system's compile-time phase. It transforms
// (syntax-rules (literal ...) (pattern template) ...) forms into transformer
// closures that can be invoked during macro expansion.
//
// Design: The macro system uses a layered architecture (see DESIGN.md):
//   1. Pattern Matching VM (match/) - unhygienic core engine
//   2. Syntax Adapter Layer - bridges syntax objects and raw values
//   3. Hygienic Layer - scope management per Flatt's "sets of scopes" model
//
// This file handles layer 2: compiling syntax-rules into a transformer closure.
// The transformer is stored in the environment with BindingTypeSyntax, allowing
// the expander to recognize and invoke it during macro expansion.
//
// Reference: R7RS Section 4.3.2 (syntax-rules)

import (
	"context"

	"wile/environment"
	"wile/match"
	"wile/syntax"
	"wile/values"
)

// SyntaxRulesClause represents a single pattern-template pair in syntax-rules.
//
// Per R7RS, a syntax-rules form contains:
//
//	(syntax-rules (literal ...) clause ...)
//	(syntax-rules <ellipsis> (literal ...) clause ...)  ; with custom ellipsis
//
// where each clause is (pattern template).
//
// The pattern is compiled to bytecode for efficient matching. The template
// is stored as-is and expanded by substituting captured pattern variables.
//
// The macroScope field supports hygiene: when the transformer runs, it creates
// a fresh "intro scope" that marks all identifiers introduced by the macro.
// This prevents variable capture between the macro and its use site.
type SyntaxRulesClause struct {
	pattern      syntax.SyntaxValue                    // The pattern to match against input
	template     syntax.SyntaxValue                    // The template to expand on match
	bytecode     []match.SyntaxCommand                 // Compiled pattern bytecode
	matcher      *match.SyntaxMatcher                  // Pattern matcher instance
	patternVars  map[string]struct{}                   // Variables extracted from pattern
	ellipsisVars map[int]map[string]struct{}           // ellipsisID -> captured pattern variables
	freeIds      map[string]*environment.GlobalIndex   // Free identifiers resolved to definition-time bindings
	macroScope   *syntax.Scope                         // Hygiene scope for this macro (Flatt's model)
	ellipsis     string                                // Custom ellipsis identifier (default "...")
}

// clausesWrapper wraps clauses as a values.Value for storing in literals
type clausesWrapper struct {
	clauses []*SyntaxRulesClause
}

func (c *clausesWrapper) EqualTo(other values.Value) bool {
	_, ok := other.(*clausesWrapper)
	if !ok {
		return false
	}
	// Clauses are not comparable
	return false
}

func (c *clausesWrapper) IsVoid() bool {
	return false
}

func (c *clausesWrapper) SchemeString() string {
	return "#<syntax-rules-clauses>"
}

// CompileSyntaxRules compiles a syntax-rules form into a transformer procedure.
//
// R7RS Forms:
//
//	(syntax-rules (literal ...) (pattern template) ...)
//	(syntax-rules <ellipsis> (literal ...) (pattern template) ...)  ; custom ellipsis
//
// The compilation process:
//  1. Parse optional custom ellipsis identifier
//  2. Parse the literals list - these symbols are matched literally, not as variables
//  3. For each clause, identify pattern variables (symbols not in literals list)
//  4. Compile each pattern to bytecode (see match/syntax_compiler.go)
//  5. Create a MachineClosure that, when invoked:
//     - Tries each pattern in order against the input form
//     - On first match, expands the template with captured bindings
//     - Adds an "intro scope" to the expansion for hygiene
//
// The returned closure is stored in the environment with BindingTypeSyntax,
// allowing the expander to recognize it as a macro transformer.
func CompileSyntaxRules(ctx context.Context, env *environment.EnvironmentFrame, syntaxRulesForm syntax.SyntaxValue) (*MachineClosure, error) {
	// syntaxRulesForm should be (syntax-rules (literals...) clause1 clause2 ...)
	// or (syntax-rules <ellipsis> (literals...) clause1 clause2 ...)
	formPair, ok := syntaxRulesForm.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.NewForeignErrorf("syntax-rules: expected a list")
	}

	// Skip 'syntax-rules' keyword
	cdr := formPair.SyntaxCdr()
	if cdr == nil {
		return nil, values.NewForeignErrorf("syntax-rules: missing literals list and clauses")
	}

	argsPair, ok := cdr.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.NewForeignErrorf("syntax-rules: expected literals list and clauses")
	}

	// Check for optional custom ellipsis identifier
	// R7RS §4.3.2: (syntax-rules <ellipsis> (literal ...) clause ...)
	ellipsis := match.DefaultEllipsis
	firstArg := argsPair.SyntaxCar()
	if firstArg == nil {
		return nil, values.NewForeignErrorf("syntax-rules: missing literals list")
	}

	var literalsStx syntax.SyntaxValue
	var clausesCdr syntax.SyntaxValue

	// If firstArg is a symbol, it's a custom ellipsis identifier
	if sym, ok := firstArg.(*syntax.SyntaxSymbol); ok {
		symVal := sym.Unwrap()
		if symValSym, ok := symVal.(*values.Symbol); ok {
			ellipsis = symValSym.Key
		}

		// Move to next element for literals list
		nextCdr := argsPair.SyntaxCdr()
		if nextCdr == nil {
			return nil, values.NewForeignErrorf("syntax-rules: missing literals list after ellipsis identifier")
		}
		nextPair, ok := nextCdr.(*syntax.SyntaxPair)
		if !ok {
			return nil, values.NewForeignErrorf("syntax-rules: expected literals list after ellipsis identifier")
		}

		literalsStx = nextPair.SyntaxCar()
		clausesCdr = nextPair.SyntaxCdr()
	} else {
		// No custom ellipsis, firstArg is the literals list
		literalsStx = firstArg
		clausesCdr = argsPair.SyntaxCdr()
	}

	if literalsStx == nil {
		return nil, values.NewForeignErrorf("syntax-rules: missing literals list")
	}

	literals := make(map[string]struct{})

	// Process literals list
	literalsList, ok := literalsStx.(*syntax.SyntaxPair)
	if ok && !syntax.IsSyntaxEmptyList(literalsList) {
		err := extractLiterals(literalsList, literals, ellipsis)
		if err != nil {
			return nil, values.WrapForeignErrorf(err, "syntax-rules: invalid literals list")
		}
	}
	// Empty literals list is also valid

	// Process clauses
	if clausesCdr == nil {
		return nil, values.NewForeignErrorf("syntax-rules: no clauses provided")
	}

	clausesList, ok := clausesCdr.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.NewForeignErrorf("syntax-rules: expected clause list")
	}

	// Compile each clause
	var clauses []*SyntaxRulesClause
	v, err := clausesList.SyntaxForEach(ctx, func(_ context.Context, _ int, _ bool, clause syntax.SyntaxValue) error {
		clausePair, ok := clause.(*syntax.SyntaxPair)
		if !ok {
			return values.NewForeignErrorf("syntax-rules: clause must be a list")
		}

		// Extract pattern and template
		pattern := clausePair.SyntaxCar()
		if pattern == nil {
			return values.NewForeignErrorf("syntax-rules: missing pattern in clause")
		}

		cdrVal := clausePair.SyntaxCdr()
		if cdrVal == nil {
			return values.NewForeignErrorf("syntax-rules: missing template in clause")
		}
		templateCdr, ok := cdrVal.(*syntax.SyntaxPair)
		if !ok {
			return values.NewForeignErrorf("syntax-rules: template must be in a list")
		}

		template := templateCdr.SyntaxCar()
		if template == nil {
			return values.NewForeignErrorf("syntax-rules: missing template in clause")
		}

		// Compile the pattern with custom ellipsis
		// Pass env so free identifiers can be resolved to their definition-time bindings
		compiledClause, err := compileClauseWithEllipsis(ctx, env, pattern, template, literals, ellipsis)
		if err != nil {
			return values.WrapForeignErrorf(err, "syntax-rules: error compiling clause")
		}

		clauses = append(clauses, compiledClause)
		return nil
	})
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "syntax-rules: error compiling clause")
	}
	if !syntax.IsSyntaxEmptyList(v) {
		return nil, values.NewForeignErrorf("syntax-rules: expected proper list of clauses")
	}

	if len(clauses) == 0 {
		return nil, values.NewForeignErrorf("syntax-rules: no valid clauses")
	}

	// Create transformer closure
	return createTransformerClosure(env, clauses, literals)
}

// compileClause compiles a single pattern-template pair using the default ellipsis.
func compileClause(ctx context.Context, env *environment.EnvironmentFrame, pattern, template syntax.SyntaxValue, literals map[string]struct{}) (*SyntaxRulesClause, error) {
	return compileClauseWithEllipsis(ctx, env, pattern, template, literals, match.DefaultEllipsis)
}

// compileClauseWithEllipsis compiles a single pattern-template pair with a custom ellipsis.
// The env parameter is used to resolve free identifiers to their definition-time bindings.
func compileClauseWithEllipsis(ctx context.Context, env *environment.EnvironmentFrame, pattern, template syntax.SyntaxValue, literals map[string]struct{}, ellipsis string) (*SyntaxRulesClause, error) {
	// Determine pattern variables (anything not a literal, keyword, or ellipsis)
	variables := make(map[string]struct{})
	err := collectPatternVariablesWithEllipsis(pattern, literals, true, variables, ellipsis)
	if err != nil {
		return nil, err
	}
	// Compile pattern to bytecode with ellipsis variable mapping and literals
	// Literals are needed so the compiler knows to match _ literally if it's in the literals list
	compiled, err := match.CompileSyntaxPatternWithLiterals(ctx, pattern, variables, literals, ellipsis)
	if err != nil {
		return nil, err
	}

	// Create matcher with ellipsis variable mapping and custom ellipsis
	matcher := match.NewSyntaxMatcherFull(variables, compiled.Codes, compiled.EllipsisVars, ellipsis)

	// Collect free identifiers from template (identifiers that are NOT pattern variables)
	// These should NOT get the intro scope during expansion, so they can resolve
	// to bindings outside the macro (including recursive references to the macro itself)
	// Resolve each free identifier to its definition-time GlobalIndex for cross-library hygiene.
	freeIds := make(map[string]*environment.GlobalIndex)
	collectFreeIdentifiersWithEllipsis(env, template, variables, freeIds, ellipsis)

	return &SyntaxRulesClause{
		pattern:      pattern,
		template:     template,
		bytecode:     compiled.Codes,
		matcher:      matcher,
		patternVars:  variables,
		ellipsisVars: compiled.EllipsisVars,
		freeIds:      freeIds,
		macroScope:   nil, // Will be set when macro is defined
		ellipsis:     ellipsis,
	}, nil
}

// collectFreeIdentifiers walks the template and collects all identifiers that
// are NOT pattern variables. These "free identifiers" refer to bindings outside
// the macro and should NOT get the intro scope during expansion.
// Uses the default ellipsis identifier ("...").
//
// This is critical for recursive macros: the macro's own name (e.g., "and" in
// (and test2 ...)) must resolve to the macro's binding, not get an intro scope
// that would break the lookup.
//
// The env parameter is used to resolve free identifiers to their definition-time
// bindings (GlobalIndex), enabling proper resolution when the macro is used in
// a different library context.
func collectFreeIdentifiers(env *environment.EnvironmentFrame, template syntax.SyntaxValue, patternVars map[string]struct{}, freeIds map[string]*environment.GlobalIndex) {
	collectFreeIdentifiersWithEllipsis(env, template, patternVars, freeIds, match.DefaultEllipsis)
}

// collectFreeIdentifiersWithEllipsis walks the template and collects all identifiers that
// are NOT pattern variables, using a custom ellipsis identifier.
// Resolves each free identifier to its GlobalIndex in the definition environment.
func collectFreeIdentifiersWithEllipsis(env *environment.EnvironmentFrame, template syntax.SyntaxValue, patternVars map[string]struct{}, freeIds map[string]*environment.GlobalIndex, ellipsis string) {
	switch t := template.(type) {
	case *syntax.SyntaxSymbol:
		sym := t.Unwrap()
		if symVal, ok := sym.(*values.Symbol); ok {
			// Skip ellipsis marker
			if symVal.Key == ellipsis {
				return
			}
			// If it's not a pattern variable, it's a free identifier
			if _, isPatternVar := patternVars[symVal.Key]; !isPatternVar {
				// Resolve the free identifier to its definition-time binding
				// Use the interned symbol for consistent lookup
				internedSym := env.InternSymbol(symVal)
				gi := env.GetGlobalIndex(internedSym)
				// Store the resolved GlobalIndex (may be nil if unbound, which is ok -
				// unbound free identifiers like special forms will be handled normally)
				freeIds[symVal.Key] = gi
			}
		}

	case *syntax.SyntaxPair:
		if !syntax.IsSyntaxEmptyList(t) {
			// Recurse into car
			car := t.SyntaxCar()
			if car != nil {
				carStx := car
				collectFreeIdentifiersWithEllipsis(env, carStx, patternVars, freeIds, ellipsis)
			}
			// Recurse into cdr
			cdr := t.SyntaxCdr()
			if cdr != nil {
				cdrStx := cdr
				collectFreeIdentifiersWithEllipsis(env, cdrStx, patternVars, freeIds, ellipsis)
			}
		}

	case *syntax.SyntaxObject:
		// Self-evaluating literals don't contain identifiers
		// Do nothing
	}
}

// collectPatternVariables walks the pattern and identifies all pattern variables.
// A pattern variable is any symbol that is not a literal, not the first element,
// and not the ellipsis identifier.
// Uses the default ellipsis identifier ("...").
func collectPatternVariables(pattern syntax.SyntaxValue, literals map[string]struct{}, isFirst bool, variables map[string]struct{}) error {
	return collectPatternVariablesWithEllipsis(pattern, literals, isFirst, variables, match.DefaultEllipsis)
}

// collectPatternVariablesWithEllipsis walks the pattern and identifies all pattern variables,
// using a custom ellipsis identifier.
func collectPatternVariablesWithEllipsis(pattern syntax.SyntaxValue, literals map[string]struct{}, isFirst bool, variables map[string]struct{}, ellipsis string) error {
	switch p := pattern.(type) {
	case *syntax.SyntaxSymbol:
		sym := p.Unwrap()
		if symVal, ok := sym.(*values.Symbol); ok {
			// Skip if it's a keyword (first position), ellipsis, or literal
			if !isFirst && symVal.Key != ellipsis {
				if _, isLiteral := literals[symVal.Key]; !isLiteral {
					variables[symVal.Key] = struct{}{}
				}
			}
		}

	case *syntax.SyntaxPair:
		if !syntax.IsSyntaxEmptyList(p) {
			// First element in a form is considered a keyword
			err := collectPatternVariablesWithEllipsis(p.SyntaxCar(), literals, isFirst, variables, ellipsis)
			if err != nil {
				return err
			}

			// Rest of the form
			cdr := p.SyntaxCdr()
			if cdr != nil {
				err = collectPatternVariablesWithEllipsis(cdr, literals, false, variables, ellipsis)
				if err != nil {
					return err
				}
			}
		}

	case *syntax.SyntaxObject:
		// Self-evaluating literals are not pattern variables
		// Do nothing

	default:
		// Other syntax types are not pattern variables
	}

	return nil
}

// extractLiterals extracts literal symbols from the literals list.
// R7RS §4.3.2: It is a syntax violation if the ellipsis appears in <literals>.
func extractLiterals(literalsList *syntax.SyntaxPair, literals map[string]struct{}, ellipsis string) error {
	v, err := literalsList.SyntaxForEach(context.TODO(), func(_ context.Context, _ int, _ bool, literal syntax.SyntaxValue) error {
		sym, ok := literal.(*syntax.SyntaxSymbol)
		if !ok {
			return values.NewForeignErrorf("extractLiterals: literal must be a symbol")
		}

		symVal := sym.Unwrap()
		if symbol, ok := symVal.(*values.Symbol); ok {
			// R7RS §4.3.2: ellipsis cannot appear in literals list
			if symbol.Key == ellipsis {
				return values.NewForeignErrorf("ellipsis %q cannot appear in literals list", ellipsis)
			}
			literals[symbol.Key] = struct{}{}
		} else {
			return values.NewForeignErrorf("extractLiterals: literal must be a symbol")
		}
		return nil
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "extractLiterals: literals must be a symbol")
	}
	if !syntax.IsSyntaxEmptyList(v) {
		return values.NewForeignErrorf("extractLiterals: literals must be a list")
	}
	return nil
}

// createTransformerClosure creates a closure that implements the transformer
func createTransformerClosure(env *environment.EnvironmentFrame, clauses []*SyntaxRulesClause, literals map[string]struct{}) (*MachineClosure, error) {
	// Create a native template that implements the transformer logic
	// This will be called with the input form on the eval stack

	// For now, create a simple template that will be filled in
	// In a complete implementation, this would generate bytecode that:
	// 1. Gets the input form from parameter 0
	// 2. Tries each clause's pattern in order
	// 3. On first match, expands the template
	// 4. Returns the expanded result

	// Takes 1 parameter - the input form to transform
	template := NewNativeTemplate(1, 0, false)

	// Add transformer logic operations
	// This is a placeholder - the actual implementation would generate
	// operations that implement the pattern matching and expansion

	// For now, store the clauses as a literal and use a special operation
	// Need to create a values.Value wrapper for the clauses
	clausesValue := &clausesWrapper{clauses: clauses}
	clausesIdx := template.MaybeAppendLiteral(clausesValue)
	template.AppendOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(clausesIdx),
		NewOperationSyntaxRulesTransform(), // New operation type needed
	)

	// Create closure with a local environment frame for the input parameter
	// This is required because MachineContext.Apply expects LocalEnvironment() to be non-nil
	lenv := environment.NewLocalEnvironment(1) // 1 parameter: the input form
	closureEnv := environment.NewEnvironmentFrameWithParent(lenv, env)
	return NewClosureWithTemplate(template, closureEnv), nil
}
