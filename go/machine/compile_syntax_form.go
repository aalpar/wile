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
	"wile/syntax"
	"wile/values"
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
func (p *CompileTimeContinuation) CompileSyntax(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// expr is the CDR of the form (already has keyword stripped by CompilePrimitiveOrProcedureCall).
	// So expr = (template)
	argsPair, ok := expr.(*syntax.SyntaxPair)
	if !ok || argsPair.IsEmptyList() {
		return values.NewForeignError("syntax: expected exactly one argument")
	}

	// Get the template (CAR of the args list)
	template, ok := argsPair.SyntaxCar().(syntax.SyntaxValue)
	if !ok {
		return values.NewForeignError("syntax: expected syntax template")
	}

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

// templateContainsEllipsis checks if a syntax template contains the ellipsis marker "...".
func templateContainsEllipsis(stx syntax.SyntaxValue) bool {
	switch v := stx.(type) {
	case *syntax.SyntaxSymbol:
		if sym, ok := v.Unwrap().(*values.Symbol); ok {
			return sym.Key == "..."
		}
		return false

	case *syntax.SyntaxPair:
		if v.IsEmptyList() {
			return false
		}
		// Check car
		if car, ok := v.SyntaxCar().(syntax.SyntaxValue); ok {
			if templateContainsEllipsis(car) {
				return true
			}
		}
		// Check cdr
		if cdr, ok := v.SyntaxCdr().(syntax.SyntaxValue); ok {
			return templateContainsEllipsis(cdr)
		}
		return false

	default:
		return false
	}
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
	isProper := true

	for !syntax.IsSyntaxEmptyList(current) {
		car := current.SyntaxCar()
		if carSyntax, ok := car.(syntax.SyntaxValue); ok {
			elements = append(elements, carSyntax)
		}
		cdr := current.SyntaxCdr()
		if syntax.IsSyntaxEmptyList(cdr) {
			break
		}
		if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
			current = nextPair
		} else {
			// Improper list - the last cdr is not a pair
			isProper = false
			if cdrSyntax, ok := cdr.(syntax.SyntaxValue); ok {
				elements = append(elements, cdrSyntax)
			}
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
	if isProper {
		p.AppendOperations(NewOperationBuildSyntaxList(len(elements)))
	} else {
		// For improper lists, the last element becomes the cdr
		// We need a different operation for improper lists
		// For now, just build as proper list (this is a simplification)
		p.AppendOperations(NewOperationBuildSyntaxList(len(elements)))
	}

	return nil
}
