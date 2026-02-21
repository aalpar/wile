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

// CompileBeginForSyntax handles (begin-for-syntax expr ...).
//
// This form evaluates a sequence of expressions at compile time in the
// expand phase environment. It is useful for setting up compile-time
// state like hash tables or registries that macros can access.
//
// Each expression is compiled and executed at compile time. The expressions
// can use define-for-syntax bindings and runtime primitives. The result
// of the last expression is discarded (begin-for-syntax is used for side effects).
func (p *CompileTimeContinuation) CompileBeginForSyntax(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	err := p.ensureState("begin-for-syntax")
	if err != nil {
		return err
	}

	// expr is (expr ...) - the expressions after 'begin-for-syntax'
	if syntax.IsSyntaxEmptyList(expr) {
		// No expressions - nothing to do
		return nil
	}
	exprPair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxPair, "begin-for-syntax: expected expressions")
	}

	// Get expand phase environment for execution
	expandEnv := p.env.Expand()

	// Create expander for macro expansion
	expander := NewExpanderTimeContinuation(ctctx.ctx, p.env)

	// Process each expression
	current := exprPair
	v, err := current.SyntaxForEach(ctctx.ctx, func(_ context.Context, _ int, _ bool, stxVal syntax.SyntaxValue) error {
		_, err := p.expandCompileExecute(ctctx.ctx, ctctx, stxVal, expandEnv, expander, "begin-for-syntax")
		return err
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "begin-for-syntax: error processing expressions")
	}
	if !syntax.IsSyntaxEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "begin-for-syntax: expected a proper list of expressions")
	}

	// begin-for-syntax has no runtime effect - don't emit any operations
	return nil
}
