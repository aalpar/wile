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
	"strings"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// CompileDefineSyntax handles (define-syntax keyword transformer-expr).
//
// This is the compile-time handler for R7RS define-syntax. Unlike most
// definitions, define-syntax is processed entirely at compile time:
//
//  1. Parse the form: (define-syntax keyword (syntax-rules ...))
//  2. Compile the syntax-rules transformer to a machine.MachineClosure
//  3. Store the closure in the environment with BindingTypeSyntax
//  4. Emit NO runtime operations (the binding is already established)
//
// The BindingTypeSyntax marker is crucial: when the expander encounters
// a symbol, it checks if that symbol is bound to a syntax transformer.
// If so, it invokes the transformer closure to expand the macro.
//
// This is how derived expressions like 'let' work: they're defined as
// macros using define-syntax, and expand to lambda expressions:
//
//	(define-syntax let
//	  (syntax-rules ()
//	    ((let ((name val) ...) body)
//	     ((lambda (name ...) body) val ...))))
//
// Reference: R7RS Section 5.4 (Syntax definitions)
func (p *CompileTimeContinuation) CompileDefineSyntax(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	err := p.ensureState("define-syntax")
	if err != nil {
		return err
	}
	// expr is (keyword [docstring] transformer-expr) — the define-syntax keyword
	// was stripped by registerSyntaxCompiler in register.go.
	parts, err := syntax.FormParts(expr, "define-syntax", 2, 3)
	if err != nil {
		return p.wrapCompilationError(err)
	}
	keywordSym, ok := parts[0].(*syntax.SyntaxSymbol)
	if !ok {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "define-syntax: keyword must be a symbol"))
	}
	keyword := keywordSym.Unwrap().(*values.Symbol)
	transformerExpr, docstring, err := splitDefineSyntaxRest(parts[1:])
	if err != nil {
		return p.wrapCompilationError(err)
	}

	// Compile the transformer (supports syntax-rules and lambda)
	closure, err := compileTransformerToMachineClosure(ctctx.ctx, p.env, transformerExpr, p.libraryScope, p.evaluator)
	if err != nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "could not compile transformer"))
	}

	// Store the transformer one phase up from the defining frame (relative, not the
	// absolute expand phase). R7RS requires syntax bindings to live one phase above
	// the code that uses them; NextPhase() keeps this relative so a define-syntax
	// inside a transformer body climbs rather than collapsing into phase 1. Lookup
	// (expander_time_continuation.go) consults the same NextPhase() so storage and
	// lookup stay symmetric. At phaseLevel 0 this equals Expand() (level-0 identity).
	expandEnv := p.env.NextPhase()
	globalIndex, created := expandEnv.MaybeCreateOwnGlobalBinding(keyword, environment.BindingTypeSyntax)
	if !created {
		// Update existing binding
		globalIndex = expandEnv.GetGlobalIndex(keyword)
	}
	if globalIndex == nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrUnexpectedNil, "define-syntax: failed to create or find binding for %s", keyword.Key))
	}

	// Set scopes from the keyword symbol for hygiene, plus any docstring.
	// The scopes ensure local define-syntax bindings resolve correctly; the
	// docstring (when present) is surfaced by ,doc and the doc tooling.
	symbolScopes := keywordSym.Scopes()
	binding := expandEnv.GetGlobalBinding(globalIndex)
	if binding != nil {
		binding.UpdateMeta(func(m *environment.BindingMeta) bool {
			changed := false
			if symbolScopes != nil {
				m.Scopes = symbolScopes
				changed = true
			}
			if docstring != "" {
				m.Doc = docstring
				changed = true
			}
			return changed
		})
	}

	err = expandEnv.SetOwnGlobalValue(globalIndex, closure)
	if err != nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "define-syntax: failed to store transformer for %s", keyword.Key))
	}

	// define-syntax is compile-time only, emit no runtime operations
	return nil
}

// splitDefineSyntaxRest separates an optional leading docstring from the
// transformer expression in a define-syntax form. rest is the operand slice
// that follows the macro keyword name — either [transformer] or
// [docstring, transformer]. When a docstring is present it must be a string
// literal, mirroring the Guile-style leading-string docstring that define
// accepts on procedure bodies. FormParts has already bounded len(rest) to
// [1, 2], so a length other than 1 is treated as the two-operand case.
func splitDefineSyntaxRest(rest []syntax.SyntaxValue) (syntax.SyntaxValue, string, error) {
	if len(rest) < 2 {
		return rest[0], "", nil
	}
	str, ok := rest[0].UnwrapAll().(*values.String)
	if !ok {
		return nil, "", werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "define-syntax: docstring must be a string literal")
	}
	return rest[1], strings.TrimSpace(str.Value), nil
}
