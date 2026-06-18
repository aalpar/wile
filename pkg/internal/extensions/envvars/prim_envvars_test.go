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

package envvars_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/internal/extensions/envvars"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// newEngine creates a Wile engine with core + envvars extensions.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(envvars.Extension),
	)
	qt.Assert(t, err, qt.IsNil)
	return engine
}

// newSandboxedEngine creates a Wile engine with envvars extension and a
// virtual env map. Used to exercise the namespaceEnvMap branch in the
// primitives instead of the os.LookupEnv branch.
func newSandboxedEngine(t *testing.T, env map[string]string) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(envvars.Extension),
		wile.WithEnvMap(env),
	)
	qt.Assert(t, err, qt.IsNil)
	return engine
}

// runScheme runs Scheme code and returns the result.
func runScheme(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.EvalMultiple(context.Background(), code)
	qt.Assert(t, err, qt.IsNil)
	return result
}

// runSchemeExpectError runs Scheme code and expects an error.
func runSchemeExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	expr, err := engine.Parse(context.Background(), code)
	if err != nil {
		return // parse error counts as expected error
	}
	_, err = engine.Eval(context.Background(), expr)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestGetEnvironmentVariable(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("existing variable", func(t *testing.T) {
		t.Setenv("WILE_TEST_VAR", "hello")
		result := runScheme(t, engine, `(get-environment-variable "WILE_TEST_VAR")`)
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewString("hello"))
	})

	t.Run("nonexistent variable returns false", func(t *testing.T) {
		result := runScheme(t, engine, `(get-environment-variable "WILE_NONEXISTENT_VAR_12345")`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})

	t.Run("empty string value", func(t *testing.T) {
		t.Setenv("WILE_TEST_EMPTY", "")
		result := runScheme(t, engine, `(get-environment-variable "WILE_TEST_EMPTY")`)
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewString(""))
	})

	t.Run("value with equals sign", func(t *testing.T) {
		t.Setenv("WILE_TEST_EQUALS", "a=b=c")
		result := runScheme(t, engine, `(get-environment-variable "WILE_TEST_EQUALS")`)
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewString("a=b=c"))
	})

	t.Run("wrong argument type", func(t *testing.T) {
		runSchemeExpectError(t, engine, `(get-environment-variable 42)`)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		runSchemeExpectError(t, engine, `(get-environment-variable)`)
	})
}

func TestGetEnvironmentVariables(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("returns a list", func(t *testing.T) {
		result := runScheme(t, engine, `(list? (get-environment-variables))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("contains known variable", func(t *testing.T) {
		t.Setenv("WILE_TEST_ENVLIST", "found")
		result := runScheme(t, engine, `
			(let loop ((vars (get-environment-variables)))
			  (cond
			    ((null? vars) #f)
			    ((string=? (car (car vars)) "WILE_TEST_ENVLIST")
			     (cdr (car vars)))
			    (else (loop (cdr vars)))))
		`)
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewString("found"))
	})

	t.Run("entries are pairs", func(t *testing.T) {
		result := runScheme(t, engine, `
			(let ((vars (get-environment-variables)))
			  (if (null? vars)
			      #t
			      (pair? (car vars))))
		`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		runSchemeExpectError(t, engine, `(get-environment-variables 42)`)
	})
}

// TestGetEnvironmentVariable_VirtualMap exercises the namespaceEnvMap branch
// of PrimGetEnvironmentVariable. When the Namespace has a non-nil EnvMap,
// lookups read from the virtual map and bypass os.LookupEnv (and the
// authorizer gate that wraps it).
func TestGetEnvironmentVariable_VirtualMap(t *testing.T) {
	c := qt.New(t)

	t.Run("hit returns mapped value", func(t *testing.T) {
		engine := newSandboxedEngine(t, map[string]string{
			"VIRT_KEY": "virt-value",
		})
		result := runScheme(t, engine, `(get-environment-variable "VIRT_KEY")`)
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewString("virt-value"))
	})

	t.Run("miss returns false even if process has it", func(t *testing.T) {
		t.Setenv("VIRT_SHADOWED", "from-os")
		engine := newSandboxedEngine(t, map[string]string{
			"OTHER_KEY": "x",
		})
		result := runScheme(t, engine, `(get-environment-variable "VIRT_SHADOWED")`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})

	t.Run("empty map yields no entries", func(t *testing.T) {
		engine := newSandboxedEngine(t, map[string]string{})
		result := runScheme(t, engine, `(get-environment-variable "ANYTHING")`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})
}

// TestGetEnvironmentVariables_VirtualMap exercises the namespaceEnvMap branch
// of PrimGetEnvironmentVariables. The returned alist should reflect exactly
// the virtual map's contents, ignoring the process environment.
func TestGetEnvironmentVariables_VirtualMap(t *testing.T) {
	c := qt.New(t)

	t.Run("returns mapped entries", func(t *testing.T) {
		engine := newSandboxedEngine(t, map[string]string{
			"K1": "v1",
			"K2": "v2",
		})
		// Build an alist length to verify both entries are present.
		// Map iteration order is unspecified, so check membership via lookup.
		result := runScheme(t, engine, `(length (get-environment-variables))`)
		c.Assert(result.SchemeString(), qt.Equals, "2")

		result = runScheme(t, engine, `
			(let ((vars (get-environment-variables)))
			  (cdr (assoc "K1" vars)))
		`)
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewString("v1"))

		result = runScheme(t, engine, `
			(let ((vars (get-environment-variables)))
			  (cdr (assoc "K2" vars)))
		`)
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewString("v2"))
	})

	t.Run("empty map returns empty list", func(t *testing.T) {
		engine := newSandboxedEngine(t, map[string]string{})
		result := runScheme(t, engine, `(get-environment-variables)`)
		c.Assert(result.Internal(), qt.Equals, values.EmptyList)
	})

	t.Run("ignores process environment", func(t *testing.T) {
		t.Setenv("VIRT_NOT_INCLUDED", "from-os")
		engine := newSandboxedEngine(t, map[string]string{
			"ONLY_VIRT": "yes",
		})
		// VIRT_NOT_INCLUDED must not appear in the returned alist.
		result := runScheme(t, engine, `
			(let loop ((vars (get-environment-variables)))
			  (cond
			    ((null? vars) #f)
			    ((string=? (car (car vars)) "VIRT_NOT_INCLUDED") #t)
			    (else (loop (cdr vars)))))
		`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})
}
