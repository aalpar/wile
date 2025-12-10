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
	"wile/environment"
	"wile/match"
	"wile/syntax"
	"wile/values"
)

// SyntaxRulesClause represents a single pattern-template pair in syntax-rules.
//
// Per R7RS, a syntax-rules form contains:
//   (syntax-rules (literal ...) clause ...)
// where each clause is (pattern template).
//
// The pattern is compiled to bytecode for efficient matching. The template
// is stored as-is and expanded by substituting captured pattern variables.
//
// The macroScope field supports hygiene: when the transformer runs, it creates
// a fresh "intro scope" that marks all identifiers introduced by the macro.
// This prevents variable capture between the macro and its use site.
type SyntaxRulesClause struct {
	pattern      syntax.SyntaxValue             // The pattern to match against input
	template     syntax.SyntaxValue             // The template to expand on match
	bytecode     []match.SyntaxCommand          // Compiled pattern bytecode
	matcher      *match.SyntaxMatcher           // Pattern matcher instance
	patternVars  map[string]struct{}            // Variables extracted from pattern
	ellipsisVars map[int]map[string]struct{}    // ellipsisID -> captured pattern variables
	freeIds      map[string]struct{}            // Free identifiers in template (not pattern vars)
	macroScope   *syntax.Scope                  // Hygiene scope for this macro (Flatt's model)
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
// R7RS Form: (syntax-rules (literal ...) (pattern template) ...)
//
// The compilation process:
//   1. Parse the literals list - these symbols are matched literally, not as variables
//   2. For each clause, identify pattern variables (symbols not in literals list)
//   3. Compile each pattern to bytecode (see match/syntax_compiler.go)
//   4. Create a MachineClosure that, when invoked:
//      - Tries each pattern in order against the input form
//      - On first match, expands the template with captured bindings
//      - Adds an "intro scope" to the expansion for hygiene
//
// The returned closure is stored in the environment with BindingTypeSyntax,
// allowing the expander to recognize it as a macro transformer.
func CompileSyntaxRules(env *environment.EnvironmentFrame, syntaxRulesForm syntax.SyntaxValue) (*MachineClosure, error) {
	// syntaxRulesForm should be (syntax-rules (literals...) clause1 clause2 ...)
	formPair, ok := syntaxRulesForm.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.NewForeignErrorf("syntax-rules: expected a list")
	}

	// Skip 'syntax-rules' keyword
	cdr := formPair.Cdr()
	if cdr == nil {
		return nil, values.NewForeignErrorf("syntax-rules: missing literals list and clauses")
	}

	argsPair, ok := cdr.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.NewForeignErrorf("syntax-rules: expected literals list and clauses")
	}

	// Extract literals list
	literalsStx := argsPair.Car()
	if literalsStx == nil {
		return nil, values.NewForeignErrorf("syntax-rules: missing literals list")
	}

	literals := make(map[string]struct{})

	// Process literals list
	literalsList, ok := literalsStx.(*syntax.SyntaxPair)
	if ok && !syntax.IsSyntaxEmptyList(literalsList) {
		err := extractLiterals(literalsList, literals)
		if err != nil {
			return nil, values.WrapForeignErrorf(err, "syntax-rules: invalid literals list")
		}
	}
	// Empty literals list is also valid

	// Process clauses
	clausesCdr := argsPair.Cdr()
	if clausesCdr == nil {
		return nil, values.NewForeignErrorf("syntax-rules: no clauses provided")
	}

	clausesList, ok := clausesCdr.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.NewForeignErrorf("syntax-rules: expected clause list")
	}

	// Compile each clause
	var clauses []*SyntaxRulesClause
	for current := clausesList; current != nil && !syntax.IsSyntaxEmptyList(current); {
		clause := current.Car()
		if clause == nil {
			return nil, values.NewForeignErrorf("syntax-rules: invalid clause")
		}

		clausePair, ok := clause.(*syntax.SyntaxPair)
		if !ok {
			return nil, values.NewForeignErrorf("syntax-rules: clause must be a list")
		}

		// Extract pattern and template
		pattern := clausePair.Car().(syntax.SyntaxValue)
		if pattern == nil {
			return nil, values.NewForeignErrorf("syntax-rules: missing pattern in clause")
		}

		cdrVal := clausePair.Cdr()
		if cdrVal == nil {
			return nil, values.NewForeignErrorf("syntax-rules: missing template in clause")
		}
		templateCdr, ok := cdrVal.(*syntax.SyntaxPair)
		if !ok {
			return nil, values.NewForeignErrorf("syntax-rules: template must be in a list")
		}

		template := templateCdr.Car().(syntax.SyntaxValue)
		if template == nil {
			return nil, values.NewForeignErrorf("syntax-rules: missing template in clause")
		}

		// Compile the pattern
		compiledClause, err := compileClause(pattern, template, literals)
		if err != nil {
			return nil, values.WrapForeignErrorf(err, "syntax-rules: error compiling clause")
		}

		clauses = append(clauses, compiledClause)

		// Move to the next clause
		currentCdr := current.Cdr()
		if currentCdr == nil {
			break
		}
		current, _ = currentCdr.(*syntax.SyntaxPair)
		if current == nil {
			break
		}
	}

	if len(clauses) == 0 {
		return nil, values.NewForeignErrorf("syntax-rules: no valid clauses")
	}

	// Create transformer closure
	return createTransformerClosure(env, clauses, literals)
}

// compileClause compiles a single pattern-template pair
func compileClause(pattern, template syntax.SyntaxValue, literals map[string]struct{}) (*SyntaxRulesClause, error) {
	// Determine pattern variables (anything not a literal or keyword)
	variables := make(map[string]struct{})
	err := collectPatternVariables(pattern, literals, true, variables)
	if err != nil {
		return nil, err
	}
	// Compile pattern to bytecode with ellipsis variable mapping
	compiled, err := match.CompileSyntaxPatternFull(pattern, variables)
	if err != nil {
		return nil, err
	}

	// Create matcher with ellipsis variable mapping
	matcher := match.NewSyntaxMatcherWithEllipsisVars(variables, compiled.Codes, compiled.EllipsisVars)

	// Collect free identifiers from template (identifiers that are NOT pattern variables)
	// These should NOT get the intro scope during expansion, so they can resolve
	// to bindings outside the macro (including recursive references to the macro itself)
	freeIds := make(map[string]struct{})
	collectFreeIdentifiers(template, variables, freeIds)

	return &SyntaxRulesClause{
		pattern:      pattern,
		template:     template,
		bytecode:     compiled.Codes,
		matcher:      matcher,
		patternVars:  variables,
		ellipsisVars: compiled.EllipsisVars,
		freeIds:      freeIds,
		macroScope:   nil, // Will be set when macro is defined
	}, nil
}

// collectFreeIdentifiers walks the template and collects all identifiers that
// are NOT pattern variables. These "free identifiers" refer to bindings outside
// the macro and should NOT get the intro scope during expansion.
//
// This is critical for recursive macros: the macro's own name (e.g., "and" in
// (and test2 ...)) must resolve to the macro's binding, not get an intro scope
// that would break the lookup.
func collectFreeIdentifiers(template syntax.SyntaxValue, patternVars map[string]struct{}, freeIds map[string]struct{}) {
	switch t := template.(type) {
	case *syntax.SyntaxSymbol:
		sym := t.Unwrap()
		if symVal, ok := sym.(*values.Symbol); ok {
			// Skip ellipsis marker
			if symVal.Key == "..." {
				return
			}
			// If it's not a pattern variable, it's a free identifier
			if _, isPatternVar := patternVars[symVal.Key]; !isPatternVar {
				freeIds[symVal.Key] = struct{}{}
			}
		}

	case *syntax.SyntaxPair:
		if !syntax.IsSyntaxEmptyList(t) {
			// Recurse into car
			if car := t.Car(); car != nil {
				if carStx, ok := car.(syntax.SyntaxValue); ok {
					collectFreeIdentifiers(carStx, patternVars, freeIds)
				}
			}
			// Recurse into cdr
			if cdr := t.Cdr(); cdr != nil {
				if cdrStx, ok := cdr.(syntax.SyntaxValue); ok {
					collectFreeIdentifiers(cdrStx, patternVars, freeIds)
				}
			}
		}

	case *syntax.SyntaxObject:
		// Self-evaluating literals don't contain identifiers
		// Do nothing
	}
}

// collectPatternVariables walks the pattern and identifies all pattern variables
// A pattern variable is any symbol that is not a literal and not the first element
func collectPatternVariables(pattern syntax.SyntaxValue, literals map[string]struct{}, isFirst bool, variables map[string]struct{}) error {
	switch p := pattern.(type) {
	case *syntax.SyntaxSymbol:
		sym := p.Unwrap()
		if symVal, ok := sym.(*values.Symbol); ok {
			// Skip if it's a keyword (first position) or literal
			if !isFirst && symVal.Key != "..." {
				if _, isLiteral := literals[symVal.Key]; !isLiteral {
					variables[symVal.Key] = struct{}{}
				}
			}
		}

	case *syntax.SyntaxPair:
		if !syntax.IsSyntaxEmptyList(p) {
			// First element in a form is considered a keyword
			err := collectPatternVariables(p.Car().(syntax.SyntaxValue), literals, isFirst, variables)
			if err != nil {
				return err
			}

			// Rest of the form
			if cdr := p.Cdr(); cdr != nil {
				err = collectPatternVariables(cdr.(syntax.SyntaxValue), literals, false, variables)
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

// extractLiterals extracts literal symbols from the literals list
func extractLiterals(literalsList *syntax.SyntaxPair, literals map[string]struct{}) error {
	for current := literalsList; current != nil && !syntax.IsSyntaxEmptyList(current); {
		literal := current.Car()
		if literal == nil {
			return values.NewForeignErrorf("invalid literal")
		}

		sym, ok := literal.(*syntax.SyntaxSymbol)
		if !ok {
			return values.NewForeignErrorf("literal must be a symbol")
		}

		symVal := sym.Unwrap()
		if symbol, ok := symVal.(*values.Symbol); ok {
			literals[symbol.Key] = struct{}{}
		} else {
			return values.NewForeignErrorf("literal must be a symbol")
		}

		// Move to next literal
		cdr := current.Cdr()
		if cdr == nil {
			break
		}
		current, _ = cdr.(*syntax.SyntaxPair)
		if current == nil {
			break
		}
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
