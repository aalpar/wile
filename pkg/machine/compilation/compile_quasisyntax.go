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

	"github.com/aalpar/wile/pkg/machine"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// CompileQuasisyntax compiles the (quasisyntax template) form.
//
// quasisyntax is like quasiquote but for syntax objects. It supports:
//   - (unsyntax expr) - evaluate expr and insert the result at depth 1
//   - (unsyntax-splicing expr) - evaluate and splice list at depth 1
//   - nested quasisyntax increases depth
//
// Like quasiquote, unsyntax only evaluates when depth reaches 0.
// The result is a syntax object, not a raw datum.
func (p *CompileTimeContinuation) CompileQuasisyntax(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// expr is the CDR of the form (keyword stripped by syntaxCompiler in
	// register.go). So expr = (template) — exactly one element.
	template, err := formSingleArg(expr, "quasisyntax")
	if err != nil {
		return p.wrapCompilationError(err)
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
		p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
		return nil
	}

	// Transform to equivalent code and compile
	// The expansion produces regular Scheme values (list, etc), so we wrap with datum->syntax
	// to convert the result back to syntax objects.
	expanded, err := p.expandQuasisyntax(ctctx.ctx, stx, depth)
	if err != nil {
		return p.wrapCompilationError(err)
	}

	// Wrap: (datum->syntax #f expanded)
	wrapped := buildQuasiSyntaxList(srcCtx,
		syntax.NewSyntaxSymbol("datum->syntax", srcCtx),
		syntax.NewSyntaxObject(values.FalseValue, srcCtx),
		expanded,
	)
	return p.CompileExpression(ctctx, wrapped)
}

// quasisyntaxNeedsRuntime checks if a quasisyntax template contains unsyntax at
// the given depth. It creates a fresh recursion guard; the walk itself is the
// shared quasiNeedsRuntime, keyed by quasisyntaxKW.
func (p *CompileTimeContinuation) quasisyntaxNeedsRuntime(stx syntax.SyntaxValue, depth int) bool {
	return quasiNeedsRuntime(stx, depth, quasisyntaxKW, p.newQuasiDepthGuard())
}

// expandQuasisyntax transforms quasisyntax template into equivalent Scheme code.
// Delegates to the unified expandQuasi with quasisyntax keywords.
func (p *CompileTimeContinuation) expandQuasisyntax(ctx context.Context, stx syntax.SyntaxValue, depth int) (syntax.SyntaxValue, error) {
	return p.expandQuasi(ctx, stx, depth, quasisyntaxKW, p.newQuasiDepthGuard())
}

// CompileUnsyntax errors - unsyntax outside of quasisyntax
func (p *CompileTimeContinuation) CompileUnsyntax(_ CompileTimeCallContext, _ syntax.SyntaxValue) error {
	return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "unsyntax: not in quasisyntax context"))
}

// CompileUnsyntaxSplicing errors - unsyntax-splicing outside of quasisyntax
func (p *CompileTimeContinuation) CompileUnsyntaxSplicing(_ CompileTimeCallContext, _ syntax.SyntaxValue) error {
	return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "unsyntax-splicing: not in quasisyntax context"))
}
