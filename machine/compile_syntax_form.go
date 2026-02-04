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

import (
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
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
	// expr is the CDR of the form (already has keyword stripped by CompilePrimitiveOrProcedureCall).
	// So expr = (template)
	argsPair, ok := expr.(*syntax.SyntaxPair)
	if !ok || argsPair.IsEmptyList() {
		return values.NewForeignError("syntax: expected exactly one argument")
	}

	// Get the template (CAR of the args list)
	template := argsPair.SyntaxCar()

	// Check no extra arguments (CDR should be empty list)
	rest, ok := argsPair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok || !rest.IsEmptyList() {
		return values.NewForeignError("syntax: expected exactly one argument")
	}

	// Check if template contains ellipsis - if so, use runtime expansion
	if templateContainsEllipsis(template) {
		// Store template in literals and emit runtime expansion operation
		litIdx := p.template.MaybeAppendLiteral(template)
		p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
		p.AppendOperations(NewOperationSyntaxTemplateExpand())
		return nil
	}

	// No ellipsis - compile the template to bytecode that constructs the syntax object
	return p.compileSyntaxTemplateToOps(template)
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
			if cdrPair, ok := cdr.(*syntax.SyntaxPair); ok && !cdrPair.IsEmptyList() {
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
	if sym, ok := stx.(*syntax.SyntaxSymbol); ok {
		if s, ok := sym.Unwrap().(*values.Symbol); ok {
			return s.Key == "..."
		}
	}
	return false
}

// compileSyntaxTemplateToOps emits bytecode operations that build a syntax object.
// Pattern variables are looked up; literals are loaded directly.
// The result is left in the value register.
func (p *CompileTimeContinuation) compileSyntaxTemplateToOps(stx syntax.SyntaxValue) error {
	switch v := stx.(type) {
	case *syntax.SyntaxSymbol:
		// Check if this symbol is a local binding (pattern variable)
		symVal, ok := v.Unwrap().(*values.Symbol)
		if !ok {
			// Not a symbol - load as literal
			litIdx := p.template.MaybeAppendLiteral(v)
			p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
			return nil
		}
		symVal = p.env.InternSymbol(symVal)
		li := p.env.GetLocalIndex(symVal)
		if li != nil {
			// This is a pattern variable - load its value
			p.AppendOperations(NewOperationLoadLocalByLocalIndexImmediate(li))
			return nil
		}
		// Not a pattern variable - load as syntax literal
		litIdx := p.template.MaybeAppendLiteral(v)
		p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
		return nil

	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(v) {
			// Empty list - load as literal
			litIdx := p.template.MaybeAppendLiteral(v)
			p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
			return nil
		}

		// Check for escape form (... <template>)
		// R7RS §4.3.2: The result is just <template> with ellipsis having no special meaning
		car := v.SyntaxCar()
		if isEllipsisSymbol(car) {
			cdr := v.SyntaxCdr()
			if cdrPair, ok := cdr.(*syntax.SyntaxPair); ok && !cdrPair.IsEmptyList() {
				// Get the escaped template and compile it directly
				escapedTemplate := cdrPair.SyntaxCar()
				return p.compileSyntaxTemplateToOps(escapedTemplate)
			}
		}

		// Compile list elements and build a syntax list
		return p.compileSyntaxTemplateListToOps(v)

	default:
		// Other values - load as literal
		litIdx := p.template.MaybeAppendLiteral(stx)
		p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
		return nil
	}
}

// compileSyntaxTemplateListToOps compiles a list template to bytecode.
// Each element is compiled and pushed to the stack, then BuildSyntaxList is called.
func (p *CompileTimeContinuation) compileSyntaxTemplateListToOps(pair *syntax.SyntaxPair) error {
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
		err := p.compileSyntaxTemplateToOps(elem)
		if err != nil {
			return err
		}
		p.AppendOperations(NewOperationPush())
	}

	// Build the list
	p.AppendOperations(NewOperationBuildSyntaxList(len(elements)))
	return nil
}
