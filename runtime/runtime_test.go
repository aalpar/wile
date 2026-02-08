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

package runtime_test

import (
	"context"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/bootstrap"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/runtime"
	"github.com/aalpar/wile/values"
)

func newEnv(t *testing.T) *environment.EnvironmentFrame {
	t.Helper()
	env, err := bootstrap.NewTopLevelEnvironmentFrameTiny(context.Background())
	qt.New(t).Assert(err, qt.IsNil)
	return env
}

func parseSyntax(t *testing.T, env *environment.EnvironmentFrame, code string) syntax.SyntaxValue {
	t.Helper()
	p := parser.NewParser(env, true, strings.NewReader(code))
	stx, err := p.ReadSyntax(context.Background())
	qt.New(t).Assert(err, qt.IsNil)
	return stx
}

func TestCompile(t *testing.T) {
	c := qt.New(t)
	env := newEnv(t)

	tcs := []struct {
		name string
		code string
	}{
		{"integer literal", `42`},
		{"string literal", `"hello"`},
		{"addition", `(+ 1 2)`},
		{"nested expression", `(* (+ 1 2) (- 5 3))`},
		{"boolean", `#t`},
		{"lambda", `(lambda (x) x)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			stx := parseSyntax(t, env, tc.code)
			tpl, err := runtime.Compile(context.Background(), env, stx)
			c.Assert(err, qt.IsNil)
			c.Assert(tpl, qt.IsNotNil)
		})
	}
}

func TestCompileError(t *testing.T) {
	c := qt.New(t)
	env := newEnv(t)

	// Use an unbound variable to trigger a compilation error
	stx := parseSyntax(t, env, `(no-such-binding-xyz 1 2)`)
	_, err := runtime.Compile(context.Background(), env, stx)
	c.Assert(err, qt.IsNotNil)
}

func TestRun(t *testing.T) {
	c := qt.New(t)
	env := newEnv(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"integer", `42`, values.NewInteger(42)},
		{"addition", `(+ 1 2)`, values.NewInteger(3)},
		{"boolean true", `(= 1 1)`, values.TrueValue},
		{"boolean false", `(= 1 2)`, values.FalseValue},
		{"string", `"hello"`, values.NewString("hello")},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			stx := parseSyntax(t, env, tc.code)
			tpl, err := runtime.Compile(context.Background(), env, stx)
			c.Assert(err, qt.IsNil)

			result, err := runtime.Run(context.Background(), tpl, env)
			c.Assert(err, qt.IsNil)
			c.Assert(len(result) > 0, qt.IsTrue)
			c.Assert(result[0], values.SchemeEquals, tc.want)
		})
	}
}

func TestLoad(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		code string
	}{
		{"empty input", ``},
		{"single expression", `(define x 42)`},
		{"multiple expressions", "(define x 1)\n(define y 2)"},
		{"define and use", "(define x 10)\n(define y (* x 2))"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := newEnv(t)
			err := runtime.Load(context.Background(), env, strings.NewReader(tc.code), "test.scm")
			c.Assert(err, qt.IsNil)
		})
	}
}

func TestLoadCompileError(t *testing.T) {
	c := qt.New(t)
	env := newEnv(t)

	// Referencing an unbound variable triggers a compile error
	err := runtime.Load(context.Background(), env, strings.NewReader(`(no-such-binding-xyz 1 2)`), "bad.scm")
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "bad.scm")
}

func TestLoadRuntimeError(t *testing.T) {
	c := qt.New(t)
	env := newEnv(t)

	// Division by zero triggers a runtime error
	err := runtime.Load(context.Background(), env, strings.NewReader(`(/ 1 0)`), "runtime-err.scm")
	c.Assert(err, qt.IsNotNil)
}
