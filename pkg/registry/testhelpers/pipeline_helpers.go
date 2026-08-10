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

package testhelpers

import (
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/bootstrap"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/schemeutil"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// RunProgramAST compiles and runs a Scheme program from a pre-built values.Value AST.
// Uses a fully-bootstrapped top-level environment (bootstrap.NewNamespaceFrame),
// which loads the full kitchen-sink extension set.
func RunProgramAST(t *testing.T, prog values.Value) (values.Value, error) {
	t.Helper()
	env, err := bootstrap.NewNamespaceFrame(context.Background())
	if err != nil {
		return nil, err
	}
	return RunProgramASTWithEnv(t, env, prog)
}

// RunProgramASTWithEnv compiles and runs a Scheme program AST with the given environment.
func RunProgramASTWithEnv(t *testing.T, env *environment.EnvironmentFrame, prog values.Value) (values.Value, error) {
	t.Helper()
	cctx := compilation.NewCompileTimeCallContext(context.Background(), false)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	sctx := syntax.NewZeroValueSourceContext()
	stx, err := schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog)
	if err != nil {
		return nil, err
	}
	err = ccnt.CompileExpression(cctx, stx)
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

// RunSchemeCodeWithEnv parses, expands, compiles, and runs Scheme source code
// with the given environment. Useful for tests that need custom bindings.
func RunSchemeCodeWithEnv(t *testing.T, env *environment.EnvironmentFrame, code string) (values.Value, error) {
	t.Helper()
	return RunSchemeCodeWithEnvAndContext(context.Background(), t, env, code)
}

// RunSchemeCodeWithEnvAndContext parses, expands, compiles, and runs Scheme source code
// with the given context and environment.
func RunSchemeCodeWithEnvAndContext(ctx context.Context, t *testing.T, env *environment.EnvironmentFrame, code string) (values.Value, error) {
	t.Helper()
	p := parser.NewParser(env, true, strings.NewReader(code))
	stx, err := p.ReadSyntax(ctx)
	if err != nil {
		return nil, err
	}

	expanded, err := compilation.NewExpanderTimeContinuation(ctx, env, machine.NewVMMacroEvaluator()).ExpandTopLevelExpression(stx)
	if err != nil {
		return nil, err
	}

	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := compilation.NewCompileTimeCallContext(ctx, false)
	err = compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator()).CompileExpression(cctx, expanded)
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
