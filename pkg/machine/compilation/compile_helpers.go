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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// expandCompileExecute expands, compiles, and executes an expression at
// compile time in the expand-phase environment.
//
// Used by begin-for-syntax, define-for-syntax, and eval-when to evaluate
// expressions during compilation. The expression is expanded (to handle
// macros), compiled to a temporary template, and executed immediately.
//
// Returns the result value from execution.
func (p *CompileTimeContinuation) expandCompileExecute(
	ctx context.Context,
	ctctx CompileTimeCallContext,
	expr syntax.SyntaxValue,
	expandEnv *environment.EnvironmentFrame,
	expander *ExpanderTimeContinuation,
	errPrefix string,
) (values.Value, error) {
	expandedExpr, err := expander.ExpandExpression(expr)
	if err != nil {
		return nil, p.wrapCompilationError(werr.WrapForeignErrorf(err, "%s: expansion failed", errPrefix))
	}

	tmpTpl := machine.NewNativeTemplate(0, 0, false)
	tmpCcnt := NewCompileTimeContinuation(tmpTpl, expandEnv, p.evaluator)
	tmpCcnt.SetInlineThreshold(p.inlineThreshold)

	err = tmpCcnt.CompileExpression(ctctx, expandedExpr)
	if err != nil {
		return nil, p.wrapCompilationError(werr.WrapForeignErrorf(err, "%s: compilation failed", errPrefix))
	}

	result, err := p.evaluator.EvalTemplate(ctx, tmpTpl, expandEnv)
	if err != nil {
		return nil, p.wrapCompilationError(werr.WrapForeignErrorf(err, "%s: evaluation failed", errPrefix))
	}

	return result, nil
}

// executeFormsAtCompileTime expands, compiles, and executes each expression
// in bodyPair at compile time. Used by eval-when and begin-for-syntax.
//
// The body forms run one phase up from the defining frame (env.NextPhase()), so a
// begin-for-syntax at phaseLevel N runs its body at phase N+1 and a nested one
// climbs to N+2. At phaseLevel 0 NextPhase() == Expand(), so top-level
// begin-for-syntax/eval-when are unchanged (level-0 identity). The expander stays
// rooted at p.env because its own macro lookup already applies NextPhase()
// (expander_time_continuation.go), so expander.env.NextPhase() == expandEnv at
// every phase — expansion and compile/eval agree on the target frame.
func (p *CompileTimeContinuation) executeFormsAtCompileTime(
	ctctx CompileTimeCallContext,
	formName string,
	bodyPair *syntax.SyntaxPair,
) error {
	expandEnv := p.env.NextPhase()
	expander := NewExpanderTimeContinuation(ctctx.ctx, p.env, p.evaluator)

	v, err := bodyPair.SyntaxForEach(ctctx.ctx, func(_ context.Context, _ int, _ bool, stxVal syntax.SyntaxValue) error {
		_, err := p.expandCompileExecute(ctctx.ctx, ctctx, stxVal, expandEnv, expander, formName)
		return err
	})
	if err != nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "%s: error processing body expressions", formName))
	}
	if !syntax.IsSyntaxEmptyList(v) {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotAList, "%s: improper body expressions list", formName))
	}
	return nil
}

// ensureState checks that the compiler has a valid environment and template.
// Every compile-time form (define-syntax, begin-for-syntax, define-for-syntax,
// eval-when) must call this before accessing p.env or p.template.
func (p *CompileTimeContinuation) ensureState(formName string) error {
	if p.env == nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrUnexpectedNil, "%s: nil environment", formName))
	}
	if p.template == nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrUnexpectedNil, "%s: nil template", formName))
	}
	return nil
}
