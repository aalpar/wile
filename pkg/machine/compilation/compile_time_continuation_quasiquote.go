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
	"github.com/aalpar/wile/pkg/machine"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
)

// hasSyntaxArity reports whether p is a proper list of exactly n elements. It
// never panics, where (*syntax.SyntaxPair).Length panics on an improper list. The
// quasiquote and quasisyntax walks meet malformed templates like `(a unquote x . y);
// a malformed template must reach a clean compile path, not an internal-error crash.
// After hasSyntaxArity(p, k) is true, the first k SyntaxCdr().(*SyntaxPair) hops on
// p are guaranteed safe.
func hasSyntaxArity(p *syntax.SyntaxPair, n int) bool {
	if n <= 0 {
		return false
	}
	var cur syntax.SyntaxValue = p
	for range n {
		pair, ok := cur.(*syntax.SyntaxPair)
		if !ok {
			return false
		}
		cur = pair.SyntaxCdr()
	}
	return syntax.IsSyntaxEmptyList(cur)
}

// compileQuasiquoteDatum compiles a quasiquoted datum at the given nesting depth.
//
// depth=1 means we're inside one level of quasiquote (the common case).
// depth=2 means nested quasiquote `(a `(b ,x)), etc.
// depth=0 would mean we should evaluate (but we start at 1, so this is the trigger).
func (p *CompileTimeContinuation) compileQuasiquoteDatum(ctctx CompileTimeCallContext, datum syntax.SyntaxValue, depth int) error {
	// A single guard bounds both the needs-runtime analysis and the expansion;
	// each is reset (enter/leave is symmetric) before the next phase runs.
	g := p.newQuasiDepthGuard()

	// Optimization: if no runtime evaluation needed, emit as literal
	if !p.quasiNeedsRuntime(datum, depth, quasiquoteKW, g) {
		// Validate quoted literal for circular datum labels
		val, err := p.validateQuotedLiteral(datum.UnwrapAll())
		if err != nil {
			return err
		}
		// No unquote anywhere: the quasiquote is a constant and gets the same
		// R7RS §4.1.2 treatment (and the same pre-append mark) as a quote.
		li := p.appendConstantLiteral(val)
		p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(li))
		return nil
	}

	// Transform to equivalent Scheme code and compile. No vector pre-dispatch:
	// expandQuasi dispatches vectors itself, and the pre-dispatch that used to
	// sit here called the SAME function with the same arguments — a branch that
	// was reachable, redundant, and an asymmetry with the quasisyntax entry,
	// which never had one.
	expanded, err := p.expandQuasi(ctctx.ctx, datum, depth, quasiquoteKW, g)
	if err != nil {
		return p.wrapCompilationError(err)
	}
	return p.CompileExpression(ctctx, expanded)
}

// CompileUnquote errors - unquote outside of quasiquote
func (p *CompileTimeContinuation) CompileUnquote(_ CompileTimeCallContext, _ syntax.SyntaxValue) error {
	return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "unquote: not in quasiquote context"))
}

// CompileUnquoteSplicing errors - unquote-splicing outside of quasiquote
func (p *CompileTimeContinuation) CompileUnquoteSplicing(_ CompileTimeCallContext, _ syntax.SyntaxValue) error {
	return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "unquote-splicing: not in quasiquote context"))
}
