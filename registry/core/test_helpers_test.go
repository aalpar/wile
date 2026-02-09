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

package core_test

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/bootstrap"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
)

// runProgramAST is a helper to compile and run a Scheme program from a values.Value AST.
// This is the legacy version that accepts a pre-built AST.
func runProgramAST(t *testing.T, prog values.Value) (values.Value, error) {
	t.Helper()
	env, err := bootstrap.NewTopLevelEnvironmentFrameTiny(context.TODO())
	if err != nil {
		return nil, err
	}
	return runProgramASTWithEnv(t, env, prog)
}

// runProgramASTWithEnv runs a program AST with the given environment.
func runProgramASTWithEnv(t *testing.T, env *environment.EnvironmentFrame, prog values.Value) (values.Value, error) {
	t.Helper()
	cctx := machine.NewCompileTimeCallContext(context.Background(), false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()
	err := ccnt.CompileExpression(cctx, schemeutil.DatumToSyntaxValue(sctx, prog))
	if err != nil {
		return nil, err
	}
	ctx := context.Background()
	mc := machine.NewMachineContext(ctx, machine.NewMachineContinuation(nil, tpl, env))
	err = mc.RunWithEscapeHandling()
	if err != nil {
		return nil, err
	}
	return mc.GetValue(), nil
}

// runSchemeCode parses and runs Scheme source code string.
func runSchemeCode(t *testing.T, code string) (values.Value, error) {
	t.Helper()
	env, err := bootstrap.NewTopLevelEnvironmentFrameTiny(context.TODO())
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

	ectx := machine.NewExpandTimeCallContext(context.Background())
	expanded, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	if err != nil {
		return nil, err
	}

	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := machine.NewCompileTimeCallContext(context.Background(), false, true, env)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	if err != nil {
		return nil, err
	}

	ctx := context.Background()
	mc := machine.NewMachineContext(ctx, machine.NewMachineContinuation(nil, tpl, env))
	err = mc.RunWithEscapeHandling()
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
		r := recover()
		if r != nil {
			// Panic was expected, convert to error
			e, ok := r.(error)
			if ok {
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

// runSchemeCodeWithTimeout runs code with a timeout to prevent infinite loops.
// Uses context.WithTimeout for proper cooperative cancellation - the VM loop
// checks ctx.Done() on each iteration and exits cleanly when cancelled.
func runSchemeCodeWithTimeout(t *testing.T, code string, timeout time.Duration) (values.Value, error) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	return runSchemeCodeWithContext(ctx, t, code)
}

// runSchemeCodeWithContext parses and runs Scheme source code with the given context.
// The context enables cancellation/timeout - the VM loop checks ctx.Done() on each iteration.
func runSchemeCodeWithContext(ctx context.Context, t *testing.T, code string) (values.Value, error) {
	t.Helper()
	env, err := bootstrap.NewTopLevelEnvironmentFrameTiny(ctx)
	if err != nil {
		return nil, err
	}
	return runSchemeCodeWithEnvAndContext(ctx, t, env, code)
}

// runSchemeCodeWithEnvAndContext parses and runs Scheme source code with the given context and environment.
func runSchemeCodeWithEnvAndContext(ctx context.Context, t *testing.T, env *environment.EnvironmentFrame, code string) (values.Value, error) {
	t.Helper()
	p := parser.NewParser(env, true, strings.NewReader(code))
	stx, err := p.ReadSyntax(ctx)
	if err != nil {
		return nil, err
	}

	ectx := machine.NewExpandTimeCallContext(ctx)
	expanded, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	if err != nil {
		return nil, err
	}

	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := machine.NewCompileTimeCallContext(ctx, false, true, env)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	if err != nil {
		return nil, err
	}

	mc := machine.NewMachineContext(ctx, machine.NewMachineContinuation(nil, tpl, env))
	err = mc.RunWithEscapeHandling()
	if err != nil {
		return nil, err
	}
	return mc.GetValue(), nil
}
