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
	"errors"

	"wile/environment"
	"wile/machine"
	"wile/syntax"
	"wile/utils"
	"wile/values"
)

// PrimEval implements the (eval) primitive.
// Evaluates an expression in a given environment.
func PrimEval(ctx context.Context, mc *machine.MachineContext) error {
	expr := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	envSpec := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	// Get the environment frame from the SchemeEnvironment
	schemeEnv, ok := envSpec.(*values.SchemeEnvironment)
	if !ok {
		return values.NewForeignErrorf("eval: expected an environment specifier but got %T", envSpec)
	}

	env, ok := schemeEnv.Frame.(*environment.EnvironmentFrame)
	if !ok {
		return values.NewForeignError("eval: environment frame is invalid")
	}

	// Convert datum to syntax value
	sctx := syntax.NewZeroValueSourceContext()
	stx := utils.DatumToSyntaxValue(sctx, expr)

	// Expand the expression
	ectx := machine.NewExpandTimeCallContext()
	expanded, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	if err != nil {
		return values.WrapForeignErrorf(err, "eval: expansion error")
	}

	// Compile the expression
	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	if err != nil {
		return values.WrapForeignErrorf(err, "eval: compilation error")
	}

	// Run the compiled code in a sub-context
	cont := machine.NewMachineContinuation(nil, tpl, env)
	sub := machine.NewMachineContext(cont)
	if err := sub.Run(ctx); err != nil {
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(err, &escapeErr) {
			return err
		}
		if !errors.Is(err, machine.ErrMachineHalt) {
			return err
		}
	}

	mc.SetValue(sub.GetValue())
	return nil
}
