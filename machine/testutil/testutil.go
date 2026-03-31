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

// Package testutil provides shared test helpers for both the machine/
// and machine/compilation/ test suites. It exists to break the import
// cycle between machine (which cannot import compilation) and tests
// that need both packages.
package testutil

import (
	"bufio"
	"context"
	"io"
	"strings"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/bootstrap"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/machine/compilation"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// NewFullRuntimeEnv creates a full runtime environment with all
// primitives and bootstrap macros registered.
func NewFullRuntimeEnv(t *testing.T) *environment.EnvironmentFrame {
	t.Helper()
	env, err := bootstrap.NewNamespaceFrameTiny(context.TODO())
	qt.Assert(t, err, qt.IsNil)
	return env
}

// ParseSchemeExpr parses a single Scheme expression from code.
func ParseSchemeExpr(t *testing.T, env *environment.EnvironmentFrame, code string) syntax.SyntaxValue {
	t.Helper()
	reader := bufio.NewReader(strings.NewReader(code))
	p := parser.NewParser(env, true, reader)
	sv, err := p.ReadSyntax(context.TODO())
	qt.Assert(t, err, qt.IsNil)
	return sv
}

// NewTopLevelThunk expands, compiles, and wraps a syntax value into
// a continuation ready for MachineContext.Run().
func NewTopLevelThunk(
	sv syntax.SyntaxValue,
	env *environment.EnvironmentFrame,
) (*machine.MachineContinuation, error) {
	ctx := context.Background()
	eval := machine.NewVMMacroEvaluator()
	expanded, err := compilation.NewExpanderTimeContinuation(ctx, env, eval).ExpandExpression(sv)
	if err != nil {
		return nil, err
	}
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctctx := compilation.NewCompileTimeCallContext(ctx, false)
	err = compilation.NewCompileTimeContinuation(tpl, env, eval).CompileExpression(ctctx, expanded)
	if err != nil {
		return nil, err
	}
	return machine.NewMachineContinuation(nil, tpl, env), nil
}

// EvalScheme evaluates one or more Scheme expressions and returns the
// last value. Fails the test on any error.
func EvalScheme(t *testing.T, code string) values.Value {
	t.Helper()
	env := NewFullRuntimeEnv(t)
	return EvalSchemeInEnv(t, env, code)
}

// EvalSchemeInEnv evaluates one or more Scheme expressions in the given
// environment and returns the last value.
func EvalSchemeInEnv(t *testing.T, env *environment.EnvironmentFrame, code string) values.Value {
	t.Helper()
	ctx := context.Background()
	rdr := strings.NewReader(code)
	p := parser.NewParser(env, true, rdr)

	var lastValue = values.Void

	for {
		stx, err := p.ReadSyntax(context.TODO())
		if err == io.EOF {
			break
		}
		qt.Assert(t, err, qt.IsNil)

		cont, err := NewTopLevelThunk(stx, env)
		qt.Assert(t, err, qt.IsNil)

		mc := machine.NewMachineContext(ctx, cont)
		err = mc.Run()
		qt.Assert(t, err, qt.IsNil)

		lastValue = mc.GetValue()
	}

	return lastValue
}

// SetupLibraryTest creates a test environment with library loading
// capability, using the testdata/ directory at the given path as the
// library search path.
func SetupLibraryTest(t *testing.T, testdataPath string) *environment.EnvironmentFrame {
	t.Helper()
	env, err := bootstrap.NewNamespaceFrameTiny(context.TODO())
	if err != nil {
		t.Fatalf("failed to create environment: %v", err)
	}
	env.Namespace().SetLibraryEnvFactory(bootstrap.NewLibraryEnvironmentFrame)
	registry := compilation.NewLibraryRegistry()
	registry.SetSearchPaths([]string{testdataPath})
	env.SetLibraryRegistry(registry)
	return env
}

// NewMinimalNamespace creates a minimal namespace with core special form
// bindings registered (if, lambda, quote, etc.). Useful for unit tests
// that need compilation without a full bootstrap.
func NewMinimalNamespace(env *environment.EnvironmentFrame) *environment.EnvironmentFrame {
	for _, name := range []string{
		"if", "lambda", "quote", "quasiquote", "define",
		"set!", "begin", "meta", "include", "include-ci",
	} {
		env.MaybeCreateOwnGlobalBinding(
			values.NewSymbol(name),
			environment.BindingTypePrimitive,
		)
	}
	compilation.RegisterSyntaxCompilers(env)    //nolint:errcheck
	compilation.RegisterPrimitiveExpanders(env) //nolint:errcheck
	return env
}
