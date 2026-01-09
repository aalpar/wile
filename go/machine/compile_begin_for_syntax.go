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
	"context"
	"errors"

	"wile/syntax"
	"wile/values"
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
	if p.env == nil {
		return values.WrapForeignErrorf(values.ErrUnexpectedNil, "begin-for-syntax: nil environment")
	}
	if p.template == nil {
		return values.WrapForeignErrorf(values.ErrUnexpectedNil, "begin-for-syntax: nil template")
	}

	// expr is (expr ...) - the expressions after 'begin-for-syntax'
	exprPair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxPair, "begin-for-syntax: expected expressions")
	}

	// Handle empty begin-for-syntax
	if syntax.IsSyntaxEmptyList(exprPair) {
		// No expressions - nothing to do
		return nil
	}

	// Get expand phase environment for execution
	expandEnv := p.env.Expand()

	// Create expander for macro expansion
	ectx := NewExpandTimeCallContext()
	expander := NewExpanderTimeContinuation(p.env)

	// Process each expression
	current := exprPair
	v, err := current.SyntaxForEach(context.TODO(), func(_ context.Context, i int, hasNext bool, stxVal syntax.SyntaxValue) error {
		// Expand the expression (it may contain macros)
		expandedExpr, err := expander.ExpandExpression(ectx, stxVal)
		if err != nil {
			return values.WrapForeignErrorf(err, "begin-for-syntax: expansion failed")
		}

		// Compile the expression to a temporary template
		tmpTpl := NewNativeTemplate(0, 0, false)
		tmpCcnt := NewCompiletimeContinuation(tmpTpl, expandEnv)

		err = tmpCcnt.CompileExpression(ctctx, expandedExpr)
		if err != nil {
			return values.WrapForeignErrorf(err, "begin-for-syntax: compilation failed")
		}

		// Execute the compiled code at compile time
		cont := NewMachineContinuation(nil, tmpTpl, expandEnv)
		mc := NewMachineContext(cont)
		err = mc.Run(context.Background())
		if err != nil {
			if !errors.Is(err, ErrMachineHalt) {
				return values.WrapForeignErrorf(err, "begin-for-syntax: evaluation failed")
			}
		}
		return nil
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
