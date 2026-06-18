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
	// expr is (keyword transformer-expr) — the define-syntax keyword was stripped
	// by registerSyntaxCompiler in register.go.
	parts, err := syntax.FormParts(expr, "define-syntax", 2, 2)
	if err != nil {
		return p.wrapCompilationError(err)
	}
	keywordSym, ok := parts[0].(*syntax.SyntaxSymbol)
	if !ok {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "define-syntax: keyword must be a symbol"))
	}
	keyword := keywordSym.Unwrap().(*values.Symbol)
	transformerExpr := parts[1]

	// Compile the transformer (supports syntax-rules and lambda)
	closure, err := compileTransformerToMachineClosure(ctctx.ctx, p.env, transformerExpr, p.libraryScope, p.evaluator)
	if err != nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "could not compile transformer"))
	}

	// Store the transformer in the expand phase environment with BindingTypeSyntax
	// R7RS requires syntax bindings to live in the expand phase, separate from runtime bindings
	expandEnv := p.env.Expand()
	globalIndex, created := expandEnv.MaybeCreateOwnGlobalBinding(keyword, environment.BindingTypeSyntax)
	if !created {
		// Update existing binding
		globalIndex = expandEnv.GetGlobalIndex(keyword)
	}
	if globalIndex == nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrUnexpectedNil, "define-syntax: failed to create or find binding for %s", keyword.Key))
	}

	// Set scopes from the keyword symbol for hygiene
	// This ensures local define-syntax bindings have correct scopes for lookup
	symbolScopes := keywordSym.Scopes()
	binding := expandEnv.GetGlobalBinding(globalIndex)
	if binding != nil && symbolScopes != nil {
		binding.EnsureMeta().Scopes = symbolScopes
	}

	err = expandEnv.SetOwnGlobalValue(globalIndex, closure)
	if err != nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "define-syntax: failed to store transformer for %s", keyword.Key))
	}

	// define-syntax is compile-time only, emit no runtime operations
	return nil
}
