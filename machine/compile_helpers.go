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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
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
		return nil, values.WrapForeignErrorf(err, "%s: expansion failed", errPrefix)
	}

	tmpTpl := NewNativeTemplate(0, 0, false)
	tmpCcnt := NewCompiletimeContinuation(tmpTpl, expandEnv)

	err = tmpCcnt.CompileExpression(ctctx, expandedExpr)
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "%s: compilation failed", errPrefix)
	}

	cont := NewMachineContinuation(nil, tmpTpl, expandEnv)
	mc := NewMachineContext(ctx, cont)
	err = mc.Run()
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "%s: evaluation failed", errPrefix)
	}

	return mc.GetValue(), nil
}

// ensureState checks that the compiler has a valid environment and template.
// Every compile-time form (define-syntax, begin-for-syntax, define-for-syntax,
// eval-when) must call this before accessing p.env or p.template.
func (p *CompileTimeContinuation) ensureState(formName string) error {
	if p.env == nil {
		return values.WrapForeignErrorf(values.ErrUnexpectedNil, "%s: nil environment", formName)
	}
	if p.template == nil {
		return values.WrapForeignErrorf(values.ErrUnexpectedNil, "%s: nil template", formName)
	}
	return nil
}
