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

package primitives_test

import (
	"context"
	"strings"
	"testing"

	"wile/environment"
	"wile/machine"
	"wile/parser"
	"wile/runtime"
	"wile/syntax"
	"wile/utils"
	"wile/values"
)

// runProgram is a helper to compile and run a Scheme program from a values.Value AST.
func runProgram(t *testing.T, prog values.Value) (values.Value, error) {
	t.Helper()
	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	if err != nil {
		return nil, err
	}
	return runProgramWithEnv(t, env, prog)
}

// runProgramWithEnv runs a program with the given environment.
func runProgramWithEnv(t *testing.T, env *environment.EnvironmentFrame, prog values.Value) (values.Value, error) {
	t.Helper()
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()
	err := ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	if err != nil {
		return nil, err
	}
	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	if err != nil {
		return nil, err
	}
	return mc.GetValue(), nil
}

// runSchemeCode parses and runs Scheme source code string.
func runSchemeCode(t *testing.T, code string) (values.Value, error) {
	t.Helper()
	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	if err != nil {
		return nil, err
	}
	return runSchemeCodeWithEnv(t, env, code)
}

// runSchemeCodeWithEnv parses and runs Scheme source code with the given environment.
func runSchemeCodeWithEnv(t *testing.T, env *environment.EnvironmentFrame, code string) (values.Value, error) {
	t.Helper()
	p := parser.NewParser(env, strings.NewReader(code))
	stx, err := p.ReadSyntax(nil)
	if err != nil {
		return nil, err
	}

	ectx := machine.NewExpandTimeCallContext()
	expanded, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	if err != nil {
		return nil, err
	}

	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	if err != nil {
		return nil, err
	}

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	if err != nil {
		return nil, err
	}
	return mc.GetValue(), nil
}
