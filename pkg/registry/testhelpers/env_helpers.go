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
	"bufio"
	"context"
	"errors"
	"io"
	"io/fs"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/bootstrap"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// NewFullRuntimeEnv creates a full runtime environment with all
// primitives and bootstrap macros registered.
func NewFullRuntimeEnv(t *testing.T) *environment.EnvironmentFrame {
	t.Helper()
	env, err := bootstrap.NewNamespaceFrame(context.Background())
	qt.Assert(t, err, qt.IsNil)
	return env
}

// ParseSchemeExpr parses a single Scheme expression from code.
func ParseSchemeExpr(t *testing.T, env *environment.EnvironmentFrame, code string) syntax.SyntaxValue {
	t.Helper()
	reader := bufio.NewReader(strings.NewReader(code))
	p := parser.NewParser(env, true, reader)
	sv, err := p.ReadSyntax(context.Background())
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
	expanded, err := compilation.NewExpanderTimeContinuation(ctx, env, eval).ExpandTopLevelExpression(sv)
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
// environment and returns the last value. Calls t.Fatal on any error.
func EvalSchemeInEnv(t *testing.T, env *environment.EnvironmentFrame, code string) values.Value {
	t.Helper()
	result, err := evalSchemeInEnvCore(env, code)
	qt.Assert(t, err, qt.IsNil)
	return result
}

// SetupLibraryTest creates a test environment with library loading
// capability, using the testdata/ directory at the given path as the
// library search path.
func SetupLibraryTest(t *testing.T, testdataPath string) *environment.EnvironmentFrame {
	t.Helper()
	env, err := bootstrap.NewNamespaceFrame(context.Background())
	if err != nil {
		t.Fatalf("failed to create environment: %v", err)
	}
	env.Namespace().SetLibraryEnvFactory(bootstrap.NewLibraryEnvironmentFrame)
	registry := compilation.NewLibraryRegistry()
	registry.SetSearchPaths([]string{testdataPath})
	env.SetLibraryRegistry(registry)
	return env
}

// SetupEngineTest creates a test environment with full library loading support,
// mirroring what wile.NewEngine(WithSourceFS(...)) does internally. The fsys
// parameter provides the virtual filesystem for library resolution (use
// testing/fstest.MapFS for inline .sld content). Pass nil to skip FS setup.
//
// This avoids importing the root wile package (which would create a circular
// dependency) while providing the same library infrastructure.
func SetupEngineTest(t *testing.T, fsys fs.FS) *environment.EnvironmentFrame {
	t.Helper()
	env, err := bootstrap.NewNamespaceFrame(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Wire up library infrastructure (same as Engine internals).
	reg := compilation.NewLibraryRegistry()
	env.SetLibraryRegistry(reg)

	if fsys != nil {
		userResolver := compilation.NewFSFileResolver(fsys, env)
		stdlibResolver := compilation.NewFSFileResolver(stdlib.FS, env)
		resolver := compilation.NewChainFileResolver([]compilation.FileResolver{
			userResolver,
			stdlibResolver,
		})
		env.SetFileResolver(resolver)
	}

	env.Namespace().SetLibraryEnvFactory(bootstrap.NewLibraryEnvironmentFrame)

	return env
}

// evalSchemeInEnvCore is the shared eval loop used by both EvalSchemeInEnv
// and EvalSchemeInEnvMayFail. Supports multi-expression input.
func evalSchemeInEnvCore(env *environment.EnvironmentFrame, code string) (values.Value, error) {
	ctx := context.Background()
	rdr := strings.NewReader(code)
	p := parser.NewParser(env, true, rdr)

	var lastValue = values.Void

	for {
		stx, err := p.ReadSyntax(context.Background())
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			return nil, err
		}

		cont, err := NewTopLevelThunk(stx, env)
		if err != nil {
			return nil, err
		}

		mc := machine.NewMachineContext(ctx, cont)
		err = mc.RunWithEscapeHandling()
		if err != nil {
			return nil, err
		}

		lastValue = mc.GetValue()
	}

	return lastValue, nil
}

// EvalSchemeInEnvMayFail evaluates Scheme expressions in the given environment,
// returning the last value and any error. VM panics are recovered and returned
// as errors.
func EvalSchemeInEnvMayFail(t *testing.T, env *environment.EnvironmentFrame, code string) (result values.Value, err error) {
	t.Helper()
	defer func() {
		r := recover()
		if r == nil {
			return
		}
		e, ok := r.(error)
		if ok {
			err = e
		} else {
			err = panicNonError{v: r}
		}
	}()
	return evalSchemeInEnvCore(env, code)
}

// NewMinimalNamespace registers core special-form bindings (if, lambda, quote,
// etc.) into env and returns env itself: it mutates its argument rather than
// constructing a new frame. Panics if phase-handler registration fails.
// Useful for unit tests that need compilation without a full bootstrap.
func NewMinimalNamespace(env *environment.EnvironmentFrame) *environment.EnvironmentFrame {
	for _, name := range []string{
		"if", "lambda", "quote", "quasiquote", "define",
		"set!", "begin", "meta", "include", "include-ci",
	} {
		env.MaybeCreateOwnGlobalBinding(
			values.NewSymbol(name),
			environment.BindingTypePrimitive,
			nil,
		)
	}
	err := compilation.RegisterAllPhaseHandlers(env)
	if err != nil {
		panic(werr.WrapForeignErrorf(err, "NewMinimalNamespace"))
	}
	return env
}
