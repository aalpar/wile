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

// runProgram parses and runs a Scheme source code string.
// This is the primary test helper - it takes a string of Scheme code,
// parses it, expands macros, compiles it, and runs it.
func runProgram(t *testing.T, code string) (values.Value, error) {
	t.Helper()
	return runSchemeCode(t, code)
}

// runProgramAST is a helper to compile and run a Scheme program from a values.Value AST.
// This is the legacy version that accepts a pre-built AST.
func runProgramAST(t *testing.T, prog values.Value) (values.Value, error) {
	t.Helper()
	env, err := runtime.NewTopLevelEnvironmentFrameTiny(context.TODO())
	if err != nil {
		return nil, err
	}
	return runProgramASTWithEnv(t, env, prog)
}

// runProgramASTWithEnv runs a program AST with the given environment.
func runProgramASTWithEnv(t *testing.T, env *environment.EnvironmentFrame, prog values.Value) (values.Value, error) {
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
	env, err := runtime.NewTopLevelEnvironmentFrameTiny(context.TODO())
	if err != nil {
		return nil, err
	}
	return runSchemeCodeWithEnv(t, env, code)
}

// runSchemeCodeWithEnv parses and runs Scheme source code with the given environment.
func runSchemeCodeWithEnv(t *testing.T, env *environment.EnvironmentFrame, code string) (values.Value, error) {
	t.Helper()
	p := parser.NewParser(env, true, strings.NewReader(code))
	stx, err := p.ReadSyntax(context.TODO())
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

// schemeCodeTestCase is the common struct for table-driven tests that run Scheme code
// and compare against an expected value.
type schemeCodeTestCase struct {
	name     string
	code     string
	expected values.Value
}

// schemeCodeErrorTestCase is the common struct for table-driven tests that run Scheme code
// and expect an error (or just verify execution without checking the result).
type schemeCodeErrorTestCase struct {
	name string
	code string
}

// runSchemeCodeExpectError runs code and expects an error (including panics).
func runSchemeCodeExpectError(t *testing.T, code string) (err error) {
	t.Helper()
	defer func() {
		if r := recover(); r != nil {
			// Panic was expected, convert to error
			if e, ok := r.(error); ok {
				err = e
			}
		}
	}()
	_, err = runSchemeCode(t, code)
	if err == nil {
		t.Errorf("expected error but got none for: %s", code)
	}
	return err
}

// runSchemeCodeExpectTrue is a shorthand for boolean true result.
func runSchemeCodeExpectTrue(t *testing.T, code string) {
	t.Helper()
	result, err := runSchemeCode(t, code)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result != values.TrueValue {
		t.Errorf("expected #t but got %v for: %s", result, code)
	}
}

// runSchemeCodeExpectFalse is a shorthand for boolean false result.
func runSchemeCodeExpectFalse(t *testing.T, code string) {
	t.Helper()
	result, err := runSchemeCode(t, code)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result != values.FalseValue {
		t.Errorf("expected #f but got %v for: %s", result, code)
	}
}
