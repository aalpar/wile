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

package primitives

import (
	"context"

	"wile/machine"
	"wile/syntax"
	"wile/utils"
	"wile/values"
)

// PrimCompile implements the (compile) primitive.
// Compiles an expression and returns a zero-argument procedure (thunk)
// that, when called, executes the compiled code.
//
// (compile expr) -> procedure
//
// The expr can be either a syntax object or a datum (which will be
// converted to a syntax object automatically).
//
// This is the final phase hook, completing the pipeline:
//
//	expand -> compile -> (execute via calling the returned thunk)
func PrimCompile(_ context.Context, mc *machine.MachineContext) error {
	expr := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

	// Accept either syntax object or datum
	var syntaxVal syntax.SyntaxValue
	if sv, ok := expr.(syntax.SyntaxValue); ok {
		syntaxVal = sv
	} else {
		// Convert datum to syntax value
		sctx := syntax.NewZeroValueSourceContext()
		syntaxVal = utils.DatumToSyntaxValue(sctx, expr)
	}

	// Get the environment for expansion and compilation
	env := mc.EnvironmentFrame()

	// Step 1: Expand the syntax object
	ectx := machine.NewExpandTimeCallContext()
	expanded, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ectx, syntaxVal)
	if err != nil {
		return values.WrapForeignErrorf(err, "compile: expansion failed")
	}

	// Step 2: Compile to bytecode template
	// Create a thunk template (0 params, 0 locals, not variadic)
	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	if err != nil {
		return values.WrapForeignErrorf(err, "compile: compilation failed")
	}

	// Add return operation so the thunk properly returns its value
	// through the continuation chain when called
	tpl.AppendOperations(machine.NewOperationRestoreContinuation())

	// Step 3: Wrap in a closure (thunk)
	// The closure captures the current environment
	closure := machine.NewClosureWithTemplate(tpl, env)

	mc.SetValue(closure)
	return nil
}
