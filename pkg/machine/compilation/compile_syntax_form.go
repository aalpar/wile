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
	"context"

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
func (p *CompileTimeContinuation) CompileSyntax(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// expr is the CDR of the form (keyword stripped by syntaxCompiler in register.go).
	// So expr = (template)
	template, err := formSingleArg(expr, "syntax")
	if err != nil {
		return p.wrapCompilationError(err)
	}

	// Check if template contains ellipsis - if so, use runtime expansion
	if templateContainsEllipsis(template) {
		// Compute hygiene data at compile time, mirroring CompileSyntaxRules:
		// which template identifiers are free (resolve at the definition site)
		// vs. pattern variables (substituted from the match). This is what makes
		// the runtime ellipsis expansion hygienic. p.patternVarSyntax and
		// p.libraryScope are set on the body compiler by
		// compileSyntaxCaseClause; outside syntax-case they are nil (and a
		// captureless (syntax ...) errors at runtime regardless).
		freeIds := make(map[string]*FreeIdResolution)
		collectFreeIdentifiersWithEllipsis(p.env, template, p.patternVarSyntax, freeIds, match.DefaultEllipsis, p.libraryScope)
		litIdx := p.template.MaybeAppendLiteral(template)
		p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
		p.AppendOperations(NewOperationSyntaxTemplateExpand(freeIds, p.patternVarSyntax))
		return nil
	}

	// No ellipsis - compile the template to bytecode that constructs the syntax object
	return p.compileSyntaxTemplateToOps(ctctx.Context(), template)
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
func (p *CompileTimeContinuation) compileSyntaxTemplateToOps(ctx context.Context, stx syntax.SyntaxValue) error {
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
		// Resolution decides substitution, and there is nothing else to ask.
		//
		// createPatternVarEnvironment binds each pattern variable at its PATTERN
		// identifier's scopes, so this scoped query IS the question "does this
		// occurrence denote that pattern variable?" — the pattern variable is the
		// binder, the occurrence is the reference, and maximal resolution answers
		// it by Flatt's relation. An identifier an outer expansion introduced
		// merely SPELLS one of this clause's names: it lacks the use-site scope
		// the pattern variable carries, fails the subset test, resolves to
		// nothing here, and falls through to the literal emit below, which is
		// what lets it resolve at its own definition site instead.
		//
		// That fall-through is why the explicit gate this replaced could go. It
		// bailed to exactly the same literal emit on exactly the same verdict;
		// the only difference is that the verdict now comes from the resolver
		// every other identifier already uses. A name ABSENT from this clause's
		// pattern variables is unaffected either way — it may be an ENCLOSING
		// clause's pattern variable, which the frame-chain walk finds.
		//
		// This used to be an AllScopes wildcard, load-bearing for as long as
		// pattern variables were bound scopeless: cardinality 0 loses every
		// argmax, so an enclosing scoped lexical of the same name would have
		// outranked them. Scoping the binding and scoping the query are one
		// change; splitting them in either order is a regression.
		//
		// A scoped query can raise ErrAmbiguousBinding on an incomparable
		// equal-cardinality tie, which the wildcard's first-match early return
		// structurally could not. Never observed across the matrix, the Scheme
		// corpora or the Go suite; loud (a compile-time panic) if it ever is.
		li := p.env.GetLocalIndex(symVal, syntax.ScopesOf(v.Scopes()))
		if li != nil {
			templateLocalEmits.Add(1)
			// This is a pattern variable - load its value, through the shared
			// read emitter. It is not enough to translate the index for let-slot
			// merging and emit OpLoadLocal: the slot may hold a BOX (the binder
			// is captured and assigned) or live in this body's free vector, and
			// a raw load answers with the cell or with an unrelated slot.
			// Measured on the raw emit: (let ((x 5)) (lambda () (set! x 1))
			// (syntax->datum (syntax x))) rendered #&5 where the same source
			// without the set! rendered 5.
			p.emitLocalLoad(li)
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
				return p.compileSyntaxTemplateToOps(ctx, escapedTemplate)
			}
		}

		// Compile list elements and build a syntax list
		return p.compileSyntaxTemplateListToOps(ctx, v)

	default:
		// Other values - load as literal
		litIdx := p.template.MaybeAppendLiteral(stx)
		p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
		return nil
	}
}

// compileSyntaxTemplateListToOps compiles a list template to bytecode.
// Each element is compiled and pushed to the stack, then BuildSyntaxList is called.
//
// SyntaxForEach rather than a hand-rolled cdr descent, and the improper tail is
// the reason rather than the tidiness: walkSyntaxSpine RETURNS the tail instead
// of yielding it, so the two cannot be conflated. The loop this replaces
// appended the tail to the element slice, and BuildSyntaxList then consed it as
// an ordinary element — (syntax->datum (syntax (a b . c))) answered (a b c),
// where Chez answers (a b . c). The tail is still pushed like an element,
// because the operation takes it off the stack; what changed is that the
// operation is told it IS one.
func (p *CompileTimeContinuation) compileSyntaxTemplateListToOps(ctx context.Context, pair *syntax.SyntaxPair) error {
	var elements []syntax.SyntaxValue
	tail, err := pair.SyntaxForEach(ctx, func(_ context.Context, _ int, _ bool, v syntax.SyntaxValue) error {
		elements = append(elements, v)
		return nil
	})
	if err != nil {
		return p.wrapCompilationError(err)
	}

	dotted := !syntax.IsSyntaxEmptyList(tail)
	if dotted {
		elements = append(elements, tail)
	}

	// Compile each element and push to stack (in order)
	for _, elem := range elements {
		err := p.compileSyntaxTemplateToOps(ctx, elem)
		if err != nil {
			return err
		}
		p.AppendOperations(machine.NewOperationPush())
	}

	// Build the list
	if dotted {
		p.AppendOperations(NewOperationBuildDottedSyntaxList(len(elements)))
		return nil
	}
	p.AppendOperations(NewOperationBuildSyntaxList(len(elements)))
	return nil
}
