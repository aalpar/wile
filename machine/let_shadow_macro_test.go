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

package machine_test

import (
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/bootstrap"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// runSchemeCodeForShadowTest parses and runs Scheme code using a full environment.
func runSchemeCodeForShadowTest(t *testing.T, code string) (values.Value, error) {
	t.Helper()
	env, err := bootstrap.NewTopLevelEnvironmentFrameTiny(context.TODO())
	if err != nil {
		return nil, err
	}
	return runSchemeCodeWithEnvForShadowTest(t, env, code)
}

// runSchemeCodeWithEnvForShadowTest parses and runs Scheme code with the given environment.
func runSchemeCodeWithEnvForShadowTest(t *testing.T, env *environment.EnvironmentFrame, code string) (values.Value, error) {
	t.Helper()
	p := parser.NewParser(env, true, strings.NewReader(code))
	stx, err := p.ReadSyntax(context.TODO())
	if err != nil {
		return nil, err
	}

	ectx := context.Background()
	expanded, err := machine.NewExpanderTimeContinuation(ectx, env).ExpandExpression(stx)
	if err != nil {
		return nil, err
	}

	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := machine.NewCompileTimeCallContext(context.Background(), false, true)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	if err != nil {
		return nil, err
	}

	ctx := context.Background()
	mc := machine.NewMachineContext(ctx, machine.NewMachineContinuation(nil, tpl, env))
	err = mc.Run()
	if err != nil {
		return nil, err
	}
	return mc.GetValue(), nil
}

// TestLetShadowsMacro tests that let bindings can shadow macro definitions.
// This is a fix for GitHub issue #19: let does not allow hiding of macros.
// R7RS §4.2.2 requires let bindings to shadow outer bindings including macros.
//
// Note: These tests shadow bootstrap macros (and, or, when, unless) which are
// already defined in the environment, rather than defining new macros in the
// same expression (which wouldn't work because macro bindings are created
// during compilation, not expansion).
func TestLetShadowsMacro(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{
			name: "let shadows and macro - use as value",
			code: `(let ((and 100)) and)`,
			want: values.NewInteger(100),
		},
		{
			name: "let shadows or macro - use as value",
			code: `(let ((or 200)) or)`,
			want: values.NewInteger(200),
		},
		{
			name: "nested let shadows macro",
			code: `(let ((and 2))
				  (let ((x and)) x))`,
			want: values.NewInteger(2),
		},
		{
			name: "lambda parameter shadows and macro",
			code: `((lambda (and) and) 123)`,
			want: values.NewInteger(123),
		},
		{
			name: "lambda parameter shadows or macro",
			code: `((lambda (or) (+ or 1)) 50)`,
			want: values.NewInteger(51),
		},
		{
			name: "shadowed macro name can be used in arithmetic",
			code: `(let ((and 5))
				  (+ and 3))`,
			want: values.NewInteger(8),
		},
		{
			name: "macro still works when not shadowed - and",
			code: `(let ((x 1)) (and #t #t))`,
			want: values.TrueValue,
		},
		{
			name: "macro still works when not shadowed - or",
			code: `(let ((x 1)) (or #f #t))`,
			want: values.TrueValue,
		},
		{
			name: "shadow in inner let only",
			code: `(let ((x (and #t 10)))
				  (let ((and 1))
				    (+ x and)))`,
			want: values.NewInteger(11),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCodeForShadowTest(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.want)
		})
	}
}

// TestHygienePreservedWithShadowing ensures that hygiene is preserved
// when let shadowing is enabled. Macro-introduced variables should not
// be captured by user-defined variables with the same name.
//
// These tests verify that the shadowing feature doesn't break hygiene.
func TestHygienePreservedWithShadowing(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{
			name: "and macro still works correctly",
			code: `(and #t #t 42)`,
			want: values.NewInteger(42),
		},
		{
			name: "or macro still works correctly",
			code: `(or #f #f 99)`,
			want: values.NewInteger(99),
		},
		{
			name: "shadowed and doesnt affect outer scope",
			code: `(let ((result (and #t 10)))
				  (let ((and 1))
				    result))`,
			want: values.NewInteger(10),
		},
		{
			name: "let macro expands correctly with shadowed variable",
			// This tests that the 'let' macro itself still works when
			// we shadow 'and' inside the let body
			code: `(let ((x 5))
				  (let ((and x))
				    and))`,
			want: values.NewInteger(5),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCodeForShadowTest(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.want)
		})
	}
}
