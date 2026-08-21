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
	"github.com/aalpar/wile/pkg/internal/match"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// CompileSyntax compiles the (syntax template) form.
//
// Unlike quote which unwraps syntax to raw values, syntax preserves the syntax
// structure. When used inside syntax-case, pattern variables in the template
// are substituted with their matched values.
//
// For templates containing ellipsis (...), runtime expansion is used because
// ellipsis patterns capture variable-length lists that must be expanded dynamically.
//
// (syntax template) -> syntax-object
func (p *CompileTimeContinuation) CompileSyntax(_ CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// expr is the CDR of the form (keyword stripped by syntaxCompiler in register.go).
	// So expr = (template)
	template, err := formSingleArg(expr, "syntax")
	if err != nil {
		return p.wrapCompilationError(err)
	}

	// The scopes binding forms inside the enclosing syntax-case clause body
	// minted. Both template paths gate substitution on them; see
	// match.TemplateDenotesPatternVariable.
	bodyScopes := p.clauseBinderScopes

	// Check if template contains ellipsis - if so, use runtime expansion
	if templateContainsEllipsis(template) {
		// Compute hygiene data at compile time, mirroring CompileSyntaxRules:
		// which template identifiers are free (resolve at the definition site)
		// vs. pattern variables (substituted from the match). This is what makes
		// the runtime ellipsis expansion hygienic. p.patternVars,
		// p.patternVarSyntax, and p.libraryScope are set on the body compiler by
		// compileSyntaxCaseClause; outside syntax-case they are nil (and a
		// captureless (syntax ...) errors at runtime regardless).
		freeIds := make(map[string]*FreeIdResolution)
		collectFreeIdentifiersWithEllipsis(p.env, template, p.patternVars, freeIds, match.DefaultEllipsis, p.libraryScope)
		litIdx := p.template.MaybeAppendLiteral(template)
		p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
		p.AppendOperations(NewOperationSyntaxTemplateExpand(freeIds, p.patternVarSyntax, bodyScopes))
		return nil
	}

	// No ellipsis - compile the template to bytecode that constructs the syntax object
	return p.compileSyntaxTemplateToOps(template, bodyScopes)
}

// templateContainsEllipsis checks if a syntax template contains unescaped ellipsis "...".
// R7RS §4.3.2: A template of the form (... <template>) is an escape form where
// ellipses within have no special meaning. This function recognizes escape forms
// and doesn't count their contents as containing ellipsis.
func templateContainsEllipsis(stx syntax.SyntaxValue) bool {
	switch v := stx.(type) {
	case *syntax.SyntaxSymbol:
		sym, ok := v.Unwrap().(*values.Symbol)
		if ok {
			return sym.Key == "..."
		}
		return false

	case *syntax.SyntaxPair:
		if v.IsEmptyList() {
			return false
		}

		// Check for escape form (... <template>)
		// An escape form requires BOTH the ellipsis AND a template after it
		car := v.SyntaxCar()
		if isEllipsisSymbol(car) {
			cdr := v.SyntaxCdr()
			cdrPair, ok := cdr.(*syntax.SyntaxPair)
			if ok && !cdrPair.IsEmptyList() {
				// This is a valid escape form (... <template> ...)
				// The escaped template (car of cdr) doesn't count as containing ellipsis
				// But we need to check the rest of the list (cdr of cdr)
				rest := cdrPair.SyntaxCdr()
				return templateContainsEllipsis(rest)
			}
			// Just (...) with no template - this is NOT an escape form
			// The ellipsis itself is unescaped
			return true
		}

		// Not an escape form - check car and cdr normally
		if templateContainsEllipsis(car) {
			return true
		}
		cdr := v.SyntaxCdr()
		return templateContainsEllipsis(cdr)

	default:
		return false
	}
}

// isEllipsisSymbol checks if a syntax value is the ellipsis symbol "...".
func isEllipsisSymbol(stx syntax.SyntaxValue) bool {
	sym, ok := stx.(*syntax.SyntaxSymbol)
	if ok {
		s, ok := sym.Unwrap().(*values.Symbol)
		if ok {
			return s.Key == "..."
		}
	}
	return false
}

// compileSyntaxTemplateToOps emits bytecode operations that build a syntax object.
// Pattern variables are looked up; literals are loaded directly.
// The result is left in the value register.
func (p *CompileTimeContinuation) compileSyntaxTemplateToOps(stx syntax.SyntaxValue, bodyScopes []*syntax.Scope) error {
	switch v := stx.(type) {
	case *syntax.SyntaxSymbol:
		// Check if this symbol is a local binding (pattern variable)
		symVal, ok := v.Unwrap().(*values.Symbol)
		if !ok {
			// Not a symbol - load as literal
			litIdx := p.template.MaybeAppendLiteral(v)
			p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
			return nil
		}
		// This clause's pattern variables are gated on scopes before the nominal
		// lookup below can reach them: an outer expansion's identifier that merely
		// SPELLS one of them denotes its own definition-site binding, and emitting
		// it as a literal is what lets it resolve there. Only names this clause
		// actually binds are gated — a name absent from patternVarSyntax may still
		// be an ENCLOSING clause's pattern variable, which the nominal lookup finds
		// and this must not intercept.
		patSym, isPatternVar := p.patternVarSyntax[symVal.Key]
		if isPatternVar && !match.TemplateDenotesPatternVariable(v.Scopes(), patSym.Scopes(), bodyScopes) {
			litIdx := p.template.MaybeAppendLiteral(v)
			p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
			return nil
		}
		// AllScopes (match-any, first-match) is deliberate, not an unthreaded
		// scope set. Pattern variables are bound scopeless in the innermost
		// createPatternVarEnvironment frame (compile_syntax_case.go), keyed by
		// bare name — a nominal namespace, not a hygienic one. Under a wildcard
		// query GetLocalIndex returns the FIRST compatible binding walking
		// innermost-out, so the scopeless pattern variable wins. A scoped query
		// (ScopesOf(v.Scopes())) would switch to maximal resolution, where an
		// enclosing scoped lexical of the same name (larger scope count) could
		// outrank the pattern variable — a regression, not a hygiene fix.
		li := p.env.GetLocalIndex(symVal, syntax.AllScopes())
		if li != nil {
			// This is a pattern variable - load its value. Translated for
			// let-slot merging like every other local-index emit: the walk can
			// cross a merged `let` frame on its way out to the pattern frame,
			// and that frame does not exist at run time.
			p.AppendOperations(machine.NewOperationLoadLocalByLocalIndexImmediate(p.runtimeIndex(li)))
			return nil
		}
		// Not a pattern variable - load as syntax literal
		litIdx := p.template.MaybeAppendLiteral(v)
		p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
		return nil

	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(v) {
			// Empty list - load as literal
			litIdx := p.template.MaybeAppendLiteral(v)
			p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
			return nil
		}

		// Check for escape form (... <template>)
		// R7RS §4.3.2: The result is just <template> with ellipsis having no special meaning
		car := v.SyntaxCar()
		if isEllipsisSymbol(car) {
			cdr := v.SyntaxCdr()
			cdrPair, ok := cdr.(*syntax.SyntaxPair)
			if ok && !cdrPair.IsEmptyList() {
				// Get the escaped template and compile it directly
				escapedTemplate := cdrPair.SyntaxCar()
				return p.compileSyntaxTemplateToOps(escapedTemplate, bodyScopes)
			}
		}

		// Compile list elements and build a syntax list
		return p.compileSyntaxTemplateListToOps(v, bodyScopes)

	default:
		// Other values - load as literal
		litIdx := p.template.MaybeAppendLiteral(stx)
		p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
		return nil
	}
}

// compileSyntaxTemplateListToOps compiles a list template to bytecode.
// Each element is compiled and pushed to the stack, then BuildSyntaxList is called.
func (p *CompileTimeContinuation) compileSyntaxTemplateListToOps(pair *syntax.SyntaxPair, bodyScopes []*syntax.Scope) error {
	// First, collect all elements to count them
	var elements []syntax.SyntaxValue
	current := pair

	for !syntax.IsSyntaxEmptyList(current) {
		car := current.SyntaxCar()
		carSyntax := car
		elements = append(elements, carSyntax)
		cdr := current.SyntaxCdr()
		if syntax.IsSyntaxEmptyList(cdr) {
			break
		}
		nextPair, ok := cdr.(*syntax.SyntaxPair)
		if ok {
			current = nextPair
		} else {
			// Improper list - the last cdr is not a pair
			cdrSyntax := cdr
			elements = append(elements, cdrSyntax)
			break
		}
	}

	// Compile each element and push to stack (in order)
	for _, elem := range elements {
		err := p.compileSyntaxTemplateToOps(elem, bodyScopes)
		if err != nil {
			return err
		}
		p.AppendOperations(machine.NewOperationPush())
	}

	// Build the list
	p.AppendOperations(NewOperationBuildSyntaxList(len(elements)))
	return nil
}
