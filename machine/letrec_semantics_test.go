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
	"io"
	"strings"
	"testing"

	"github.com/aalpar/wile/internal/bootstrap"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// evalLetrecScheme evaluates a Scheme expression through the full pipeline
// (parse -> expand -> compile -> run) using a tiny bootstrap environment.
func evalLetrecScheme(t *testing.T, code string) values.Value {
	t.Helper()
	env, err := bootstrap.NewTopLevelEnvironmentFrameTiny(context.TODO())
	qt.Assert(t, err, qt.IsNil)

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

		expanded, err := machine.NewExpanderTimeContinuation(ctx, env).ExpandExpression(stx)
		qt.Assert(t, err, qt.IsNil)

		tpl := machine.NewNativeTemplate(0, 0, false)
		cctx := machine.NewCompileTimeCallContext(ctx, false)
		err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
		qt.Assert(t, err, qt.IsNil)

		cont := machine.NewMachineContinuation(nil, tpl, env)
		mc := machine.NewMachineContext(ctx, cont)
		err = mc.Run()
		qt.Assert(t, err, qt.IsNil)

		lastValue = mc.GetValue()
	}

	return lastValue
}

func TestLetrecSemantics(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "letrec basic bindings",
			code:     "(letrec ((x 1) (y 2)) (+ x y))",
			expected: values.NewInteger(3),
		},
		{
			name:     "letrec* sequential bindings",
			code:     "(letrec* ((x 1) (y (+ x 1))) y)",
			expected: values.NewInteger(2),
		},
		{
			name:     "letrec forward reference via lambdas",
			code:     "(letrec ((f (lambda () (g))) (g (lambda () 42))) (f))",
			expected: values.NewInteger(42),
		},
		{
			name:     "letrec mutual recursion",
			code:     "(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 10))",
			expected: values.TrueValue,
		},
		{
			name:     "letrec* body with multiple expressions",
			code:     "(letrec* ((x 10)) (+ x 1) (+ x 2))",
			expected: values.NewInteger(12),
		},
		{
			name:     "letrec single binding",
			code:     "(letrec ((x 99)) x)",
			expected: values.NewInteger(99),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := evalLetrecScheme(t, tc.code)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}
