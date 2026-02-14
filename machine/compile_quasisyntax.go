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

package machine

import (
	"context"

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
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
func (p *CompileTimeContinuation) CompileQuasisyntax(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// expr is the CDR of the form (already has keyword stripped).
	// So expr = (template)
	argsPair, ok := expr.(*syntax.SyntaxPair)
	if !ok || argsPair.IsEmptyList() {
		return values.WrapForeignErrorf(values.ErrInvalidSyntax, "quasisyntax: expected exactly one argument")
	}

	// Get the template (CAR of the args list)
	template := argsPair.SyntaxCar()

	// Check no extra arguments
	rest, ok := argsPair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok || !rest.IsEmptyList() {
		return values.WrapForeignErrorf(values.ErrInvalidSyntax, "quasisyntax: expected exactly one argument")
	}

	// Compile the quasisyntax template at depth 1
	return p.compileQuasisyntaxTemplate(ctctx, template, 1)
}

// compileQuasisyntaxTemplate compiles a quasisyntax template at the given depth.
// At depth 1, unsyntax expressions are evaluated. At depth > 1, they become literals.
func (p *CompileTimeContinuation) compileQuasisyntaxTemplate(ctctx CompileTimeCallContext, stx syntax.SyntaxValue, depth int) error {
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
	expanded := p.expandQuasisyntax(ctctx.ctx, stx, depth)

	// Wrap: (datum->syntax #f expanded)
	wrapped := p.buildQuasiquoteSyntaxList(srcCtx,
		syntax.NewSyntaxSymbol("datum->syntax", srcCtx),
		syntax.NewSyntaxObject(values.FalseValue, srcCtx),
		expanded,
	)
	return p.CompileExpression(ctctx, wrapped)
}

// quasisyntaxNeedsRuntime checks if a quasisyntax template contains unsyntax at the given depth.
func (p *CompileTimeContinuation) quasisyntaxNeedsRuntime(stx syntax.SyntaxValue, depth int) bool {
	switch v := stx.(type) {
	case *syntax.SyntaxPair:
		if values.IsEmptyList(v) {
			return false
		}

		// Check for unsyntax/unsyntax-splicing/quasisyntax keywords
		carSymName, ok := p.getSymbolName(v.SyntaxCar())
		if ok {
			switch carSymName {
			case "unsyntax", "unsyntax-splicing":
				if depth == 1 {
					return true
				}
				// At depth > 1, check the argument at depth-1
				if v.Length() == 2 {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					arg := cdr.SyntaxCar()
					return p.quasisyntaxNeedsRuntime(arg, depth-1)
				}
				return false
			case "quasisyntax":
				// Nested quasisyntax at depth 1 always needs runtime construction
				if depth == 1 {
					return true
				}
				// At depth > 1, check body at depth+1
				if v.Length() == 2 {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					body := cdr.SyntaxCar()
					return p.quasisyntaxNeedsRuntime(body, depth+1)
				}
				return false
			}
		}

		// Check list elements
		current := v
		for !syntax.IsSyntaxEmptyList(current) {
			car := current.SyntaxCar()
			carSyntax := car
			if p.quasisyntaxNeedsRuntime(carSyntax, depth) {
				return true
			}
			cdr := current.SyntaxCdr()
			if syntax.IsSyntaxEmptyList(cdr) {
				break
			}
			nextPair, ok := cdr.(*syntax.SyntaxPair)
			if ok {
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
func (p *CompileTimeContinuation) expandQuasisyntax(ctx context.Context, stx syntax.SyntaxValue, depth int) syntax.SyntaxValue {
	srcCtx := stx.SourceContext()

	switch v := stx.(type) {
	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(v) {
			// Empty list - wrap in syntax
			return p.buildQuasiquoteSyntaxList(srcCtx,
				syntax.NewSyntaxSymbol("syntax", srcCtx),
				v,
			)
		}

		carSymName, ok := p.getSymbolName(v.SyntaxCar())
		if ok {
			switch carSymName {
			case "unsyntax":
				if depth == 1 {
					// At depth 1, evaluate the expression
					if v.Length() == 2 {
						cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
						return cdr.SyntaxCar()
					}
				}
				// At depth > 1, produce literal unsyntax form with processed arg
				if v.Length() == 2 {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					arg := cdr.SyntaxCar()
					processedArg := p.expandQuasisyntax(ctx, arg, depth-1)
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
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					arg := cdr.SyntaxCar()
					processedArg := p.expandQuasisyntax(ctx, arg, depth-1)
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
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					body := cdr.SyntaxCar()
					processedBody := p.expandQuasisyntax(ctx, body, depth+1)
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
		return p.expandQuasisyntaxList(ctx, v, depth)

	case *syntax.SyntaxSymbol:
		// Sym - wrap in syntax
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
func (p *CompileTimeContinuation) expandQuasisyntaxList(ctx context.Context, pair *syntax.SyntaxPair, depth int) syntax.SyntaxValue {
	srcCtx := pair.SourceContext()

	// Check if any element is unsyntax-splicing at depth 1
	hasSplice := false
	current := pair
	_, err := current.SyntaxForEach(ctx, func(_ context.Context, i int, hasNext bool, carSyntax syntax.SyntaxValue) error {
		carPair, ok := carSyntax.(*syntax.SyntaxPair)
		if !ok {
			return nil
		}
		carSymName, ok := p.getSymbolName(carPair.SyntaxCar())
		if !ok {
			return nil
		}
		if carSymName == "unsyntax-splicing" && depth == 1 {
			hasSplice = true
		}
		return nil
	})
	if err != nil {
		panic(values.WrapForeignErrorf(err, "quasisyntax: error scanning list at %s", srcCtx.SchemeString()))
	}
	// Note: For improper lists, the tail won't be an empty list.
	// That's fine - improper lists can't have splices in their tail anyway.

	if !hasSplice {
		// Simple case: (list elem1 elem2 ...)
		var elems []syntax.SyntaxValue
		elems = append(elems, syntax.NewSyntaxSymbol("list", srcCtx))

		current = pair
		for !syntax.IsSyntaxEmptyList(current) {
			car := current.SyntaxCar()
			carSyntax := car
			elems = append(elems, p.expandQuasisyntax(ctx, carSyntax, depth))
			cdr := current.SyntaxCdr()
			if values.IsEmptyList(cdr) {
				break
			}
			nextPair, ok := cdr.(*syntax.SyntaxPair)
			if ok {
				current = nextPair
			} else {
				// Improper list - handle dotted pair
				// (list* elem1 elem2 ... cdr)
				elems[0] = syntax.NewSyntaxSymbol("list*", srcCtx)
				cdrSyntax := cdr
				elems = append(elems, p.expandQuasisyntax(ctx, cdrSyntax, depth))
				break
			}
		}
		return p.buildQuasiquoteSyntaxList(srcCtx, elems...)
	}

	// Has splicing - use append
	var appendArgs []syntax.SyntaxValue
	appendArgs = append(appendArgs, syntax.NewSyntaxSymbol("append", srcCtx))

	current = pair
	for !syntax.IsSyntaxEmptyList(current) {
		car := current.SyntaxCar()
		carSyntax := car
		carPair, ok := carSyntax.(*syntax.SyntaxPair)
		if ok {
			carSymName, ok := p.getSymbolName(carPair.SyntaxCar())
			if ok && carSymName == "unsyntax-splicing" && depth == 1 {
				// unsyntax-splicing at depth 1 - splice the value directly
				if carPair.Length() == 2 {
					cdr := carPair.SyntaxCdr().(*syntax.SyntaxPair)
					arg := cdr.SyntaxCar()
					appendArgs = append(appendArgs, arg)
				}
			} else {
				// Regular element - wrap in (list ...)
				appendArgs = append(appendArgs,
					p.buildQuasiquoteSyntaxList(srcCtx,
						syntax.NewSyntaxSymbol("list", srcCtx),
						p.expandQuasisyntax(ctx, carSyntax, depth),
					),
				)
			}
		} else {
			appendArgs = append(appendArgs,
				p.buildQuasiquoteSyntaxList(srcCtx,
					syntax.NewSyntaxSymbol("list", srcCtx),
					p.expandQuasisyntax(ctx, carSyntax, depth),
				),
			)
		}
		cdr := current.SyntaxCdr()
		if syntax.IsSyntaxEmptyList(cdr) {
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
func (p *CompileTimeContinuation) CompileUnsyntax(_ CompileTimeCallContext, _ syntax.SyntaxValue) error {
	return values.WrapForeignErrorf(values.ErrInvalidSyntax, "unsyntax: not in quasisyntax context")
}

// CompileUnsyntaxSplicing errors - unsyntax-splicing outside of quasisyntax
func (p *CompileTimeContinuation) CompileUnsyntaxSplicing(_ CompileTimeCallContext, _ syntax.SyntaxValue) error {
	return values.WrapForeignErrorf(values.ErrInvalidSyntax, "unsyntax-splicing: not in quasisyntax context")
}
