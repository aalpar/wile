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

// CompileQuasisyntax compiles the (quasisyntax template) form.
//
// quasisyntax is like quasiquote but for syntax objects. It supports:
//   - (unsyntax expr) - evaluate expr and splice result at depth 1
//   - (unsyntax-splicing expr) - evaluate and splice list at depth 1
//   - nested quasisyntax increases depth
//
// Like quasiquote, unsyntax only evaluates when depth reaches 0.
// The result is a syntax object, not a raw datum.
func (p *CompileTimeContinuation) CompileQuasisyntax(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// expr is the CDR of the form (already has keyword stripped).
	// So expr = (template)
	argsPair, ok := expr.(*syntax.SyntaxPair)
	if !ok || argsPair.IsEmptyList() {
		return values.NewForeignError("quasisyntax: expected exactly one argument")
	}

	// Get the template (CAR of the args list)
	template, ok := argsPair.Car().(syntax.SyntaxValue)
	if !ok {
		return values.NewForeignError("quasisyntax: expected syntax template")
	}

	// Check no extra arguments
	rest, ok := argsPair.Cdr().(*syntax.SyntaxPair)
	if !ok || !rest.IsEmptyList() {
		return values.NewForeignError("quasisyntax: expected exactly one argument")
	}

	// Compile the quasisyntax template at depth 1
	return p.compileQuasisyntaxTemplate(ccnt, template, 1)
}

// compileQuasisyntaxTemplate compiles a quasisyntax template at the given depth.
// At depth 1, unsyntax expressions are evaluated. At depth > 1, they become literals.
func (p *CompileTimeContinuation) compileQuasisyntaxTemplate(ccnt CompileTimeCallContext, stx syntax.SyntaxValue, depth int) error {
	srcCtx := stx.SourceContext()

	// Check if template needs runtime evaluation
	if !p.quasisyntaxNeedsRuntime(stx, depth) {
		// No unsyntax at current depth - just load as literal syntax
		litIdx := p.template.MaybeAppendLiteral(stx)
		p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
		return nil
	}

	// Transform to equivalent code and compile
	// The expansion produces regular Scheme values (list, etc), so we wrap with datum->syntax
	// to convert the result back to syntax objects.
	expanded := p.expandQuasisyntax(stx, depth)

	// Wrap: (datum->syntax #f expanded)
	wrapped := p.buildQuasiquoteSyntaxList(srcCtx,
		syntax.NewSyntaxSymbol("datum->syntax", srcCtx),
		syntax.NewSyntaxObject(values.FalseValue, srcCtx),
		expanded,
	)
	return p.CompileExpression(ccnt, wrapped)
}

// quasisyntaxNeedsRuntime checks if a quasisyntax template contains unsyntax at the given depth.
func (p *CompileTimeContinuation) quasisyntaxNeedsRuntime(stx syntax.SyntaxValue, depth int) bool {
	switch v := stx.(type) {
	case *syntax.SyntaxPair:
		if values.IsEmptyList(v) {
			return false
		}

		// Check for unsyntax/unsyntax-splicing/quasisyntax keywords
		if carSymName, ok := p.getSymbolName(v.Car()); ok {
			switch carSymName {
			case "unsyntax", "unsyntax-splicing":
				if depth == 1 {
					return true
				}
				// At depth > 1, check the argument at depth-1
				if v.Length() == 2 {
					cdr := v.Cdr().(*syntax.SyntaxPair)
					arg := cdr.Car().(syntax.SyntaxValue)
					return p.quasisyntaxNeedsRuntime(arg, depth-1)
				}
				return false
			case "quasisyntax":
				// Nested quasisyntax - check body at depth+1
				if v.Length() == 2 {
					cdr := v.Cdr().(*syntax.SyntaxPair)
					body := cdr.Car().(syntax.SyntaxValue)
					return p.quasisyntaxNeedsRuntime(body, depth+1)
				}
				return false
			}
		}

		// Check list elements
		current := v
		for !values.IsEmptyList(current) {
			car := current.Car()
			if carSyntax, ok := car.(syntax.SyntaxValue); ok {
				if p.quasisyntaxNeedsRuntime(carSyntax, depth) {
					return true
				}
			}
			cdr := current.Cdr()
			if values.IsEmptyList(cdr) {
				break
			}
			if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
				current = nextPair
			} else {
				break
			}
		}
		return false

	default:
		return false
	}
}

// expandQuasisyntax transforms quasisyntax template into equivalent Scheme code.
// Similar to expandQuasiquote but produces syntax objects.
func (p *CompileTimeContinuation) expandQuasisyntax(stx syntax.SyntaxValue, depth int) syntax.SyntaxValue {
	srcCtx := stx.SourceContext()

	switch v := stx.(type) {
	case *syntax.SyntaxPair:
		if values.IsEmptyList(v) {
			// Empty list - wrap in syntax
			return p.buildQuasiquoteSyntaxList(srcCtx,
				syntax.NewSyntaxSymbol("syntax", srcCtx),
				v,
			)
		}

		if carSymName, ok := p.getSymbolName(v.Car()); ok {
			switch carSymName {
			case "unsyntax":
				if depth == 1 {
					// At depth 1, evaluate the expression
					if v.Length() == 2 {
						cdr := v.Cdr().(*syntax.SyntaxPair)
						return cdr.Car().(syntax.SyntaxValue)
					}
				}
				// At depth > 1, produce literal unsyntax form with processed arg
				if v.Length() == 2 {
					cdr := v.Cdr().(*syntax.SyntaxPair)
					arg := cdr.Car().(syntax.SyntaxValue)
					processedArg := p.expandQuasisyntax(arg, depth-1)
					// Build: (list (syntax unsyntax) <processedArg>)
					return p.buildQuasiquoteSyntaxList(srcCtx,
						syntax.NewSyntaxSymbol("list", srcCtx),
						p.buildQuasiquoteSyntaxList(srcCtx,
							syntax.NewSyntaxSymbol("syntax", srcCtx),
							syntax.NewSyntaxSymbol("unsyntax", srcCtx),
						),
						processedArg,
					)
				}
				return p.buildQuasiquoteSyntaxList(srcCtx,
					syntax.NewSyntaxSymbol("syntax", srcCtx),
					v,
				)

			case "unsyntax-splicing":
				if depth > 1 && v.Length() == 2 {
					cdr := v.Cdr().(*syntax.SyntaxPair)
					arg := cdr.Car().(syntax.SyntaxValue)
					processedArg := p.expandQuasisyntax(arg, depth-1)
					return p.buildQuasiquoteSyntaxList(srcCtx,
						syntax.NewSyntaxSymbol("list", srcCtx),
						p.buildQuasiquoteSyntaxList(srcCtx,
							syntax.NewSyntaxSymbol("syntax", srcCtx),
							syntax.NewSyntaxSymbol("unsyntax-splicing", srcCtx),
						),
						processedArg,
					)
				}
				// At depth 1, unsyntax-splicing needs special list handling
				return p.buildQuasiquoteSyntaxList(srcCtx,
					syntax.NewSyntaxSymbol("syntax", srcCtx),
					v,
				)

			case "quasisyntax":
				// Nested quasisyntax - process body at depth+1
				if v.Length() == 2 {
					cdr := v.Cdr().(*syntax.SyntaxPair)
					body := cdr.Car().(syntax.SyntaxValue)
					processedBody := p.expandQuasisyntax(body, depth+1)
					return p.buildQuasiquoteSyntaxList(srcCtx,
						syntax.NewSyntaxSymbol("list", srcCtx),
						p.buildQuasiquoteSyntaxList(srcCtx,
							syntax.NewSyntaxSymbol("syntax", srcCtx),
							syntax.NewSyntaxSymbol("quasisyntax", srcCtx),
						),
						processedBody,
					)
				}
				return p.buildQuasiquoteSyntaxList(srcCtx,
					syntax.NewSyntaxSymbol("syntax", srcCtx),
					v,
				)
			}
		}

		// Regular list - expand elements and construct with list
		return p.expandQuasisyntaxList(v, depth)

	case *syntax.SyntaxSymbol:
		// Symbol - wrap in syntax
		return p.buildQuasiquoteSyntaxList(srcCtx,
			syntax.NewSyntaxSymbol("syntax", srcCtx),
			v,
		)

	default:
		// Other atoms - wrap in syntax
		return p.buildQuasiquoteSyntaxList(srcCtx,
			syntax.NewSyntaxSymbol("syntax", srcCtx),
			stx,
		)
	}
}

// expandQuasisyntaxList handles list expansion for quasisyntax.
func (p *CompileTimeContinuation) expandQuasisyntaxList(pair *syntax.SyntaxPair, depth int) syntax.SyntaxValue {
	srcCtx := pair.SourceContext()

	// Check if any element is unsyntax-splicing at depth 1
	hasSplice := false
	current := pair
	for !values.IsEmptyList(current) {
		car := current.Car()
		if carSyntax, ok := car.(syntax.SyntaxValue); ok {
			if carPair, ok := carSyntax.(*syntax.SyntaxPair); ok {
				if carSymName, ok := p.getSymbolName(carPair.Car()); ok {
					if carSymName == "unsyntax-splicing" && depth == 1 {
						hasSplice = true
						break
					}
				}
			}
		}
		cdr := current.Cdr()
		if values.IsEmptyList(cdr) {
			break
		}
		if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
			current = nextPair
		} else {
			break
		}
	}

	if !hasSplice {
		// Simple case: (list elem1 elem2 ...)
		var elems []syntax.SyntaxValue
		elems = append(elems, syntax.NewSyntaxSymbol("list", srcCtx))

		current := pair
		for !values.IsEmptyList(current) {
			car := current.Car()
			if carSyntax, ok := car.(syntax.SyntaxValue); ok {
				elems = append(elems, p.expandQuasisyntax(carSyntax, depth))
			}
			cdr := current.Cdr()
			if values.IsEmptyList(cdr) {
				break
			}
			if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
				current = nextPair
			} else {
				// Improper list - handle dotted pair
				// (list* elem1 elem2 ... cdr)
				elems[0] = syntax.NewSyntaxSymbol("list*", srcCtx)
				if cdrSyntax, ok := cdr.(syntax.SyntaxValue); ok {
					elems = append(elems, p.expandQuasisyntax(cdrSyntax, depth))
				}
				break
			}
		}
		return p.buildQuasiquoteSyntaxList(srcCtx, elems...)
	}

	// Has splicing - use append
	var appendArgs []syntax.SyntaxValue
	appendArgs = append(appendArgs, syntax.NewSyntaxSymbol("append", srcCtx))

	current = pair
	for !values.IsEmptyList(current) {
		car := current.Car()
		if carSyntax, ok := car.(syntax.SyntaxValue); ok {
			if carPair, ok := carSyntax.(*syntax.SyntaxPair); ok {
				if carSymName, ok := p.getSymbolName(carPair.Car()); ok && carSymName == "unsyntax-splicing" && depth == 1 {
					// unsyntax-splicing at depth 1 - splice the value directly
					if carPair.Length() == 2 {
						cdr := carPair.Cdr().(*syntax.SyntaxPair)
						arg := cdr.Car().(syntax.SyntaxValue)
						appendArgs = append(appendArgs, arg)
					}
				} else {
					// Regular element - wrap in (list ...)
					appendArgs = append(appendArgs,
						p.buildQuasiquoteSyntaxList(srcCtx,
							syntax.NewSyntaxSymbol("list", srcCtx),
							p.expandQuasisyntax(carSyntax, depth),
						),
					)
				}
			} else {
				appendArgs = append(appendArgs,
					p.buildQuasiquoteSyntaxList(srcCtx,
						syntax.NewSyntaxSymbol("list", srcCtx),
						p.expandQuasisyntax(carSyntax, depth),
					),
				)
			}
		}
		cdr := current.Cdr()
		if values.IsEmptyList(cdr) {
			break
		}
		if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
			current = nextPair
		} else {
			break
		}
	}

	return p.buildQuasiquoteSyntaxList(srcCtx, appendArgs...)
}

// CompileUnsyntax errors - unsyntax outside of quasisyntax
func (p *CompileTimeContinuation) CompileUnsyntax(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
	return values.NewForeignError("unsyntax: not in quasisyntax context")
}

// CompileUnsyntaxSplicing errors - unsyntax-splicing outside of quasisyntax
func (p *CompileTimeContinuation) CompileUnsyntaxSplicing(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
	return values.NewForeignError("unsyntax-splicing: not in quasisyntax context")
}
