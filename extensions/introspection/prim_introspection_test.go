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

package introspection_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile"
	extintrospection "github.com/aalpar/wile/extensions/introspection"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// newEngine creates a Wile engine with core + introspection extensions.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extintrospection.Extension),
	)
	qt.Assert(t, err, qt.IsNil)
	return engine
}

// schemeEval runs Scheme code and returns the result.
func schemeEval(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.Eval(context.Background(), code)
	qt.Assert(t, err, qt.IsNil)
	return result
}

// evalExpectError runs Scheme code and expects an error.
func evalExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	_, err := engine.Eval(context.Background(), code)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestInteractionEnvironment(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("returns environment object", func(t *testing.T) {
		result := schemeEval(t, engine, `(interaction-environment)`)
		c.Assert(result.Internal(), qt.IsNotNil)
	})

	t.Run("environment? true for interaction-environment", func(t *testing.T) {
		result := schemeEval(t, engine, `(environment? (interaction-environment))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(interaction-environment 42)`)
	})
}

func TestEnvironmentIntrospection(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("environment? true for environment", func(t *testing.T) {
		result := schemeEval(t, engine, `(environment? (interaction-environment))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("environment? false for non-environment", func(t *testing.T) {
		result := schemeEval(t, engine, `(environment? 42)`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})

	t.Run("environment-bound-names returns list", func(t *testing.T) {
		result := schemeEval(t, engine, `(pair? (environment-bound-names (interaction-environment)))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("environment-bound-names elements are symbols", func(t *testing.T) {
		result := schemeEval(t, engine, `(symbol? (car (environment-bound-names (interaction-environment))))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("environment-bound-names wrong type", func(t *testing.T) {
		evalExpectError(t, engine, `(environment-bound-names 42)`)
	})

	t.Run("environment-ref looks up procedure", func(t *testing.T) {
		result := schemeEval(t, engine, `(procedure? (environment-ref (interaction-environment) '+))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("environment-ref unbound symbol", func(t *testing.T) {
		evalExpectError(t, engine, `(environment-ref (interaction-environment) 'nonexistent-xyz)`)
	})

	t.Run("environment-ref wrong env type", func(t *testing.T) {
		evalExpectError(t, engine, `(environment-ref 42 '+)`)
	})

	t.Run("environment-ref wrong symbol type", func(t *testing.T) {
		evalExpectError(t, engine, `(environment-ref (interaction-environment) 42)`)
	})

	t.Run("environment-bound? true for bound", func(t *testing.T) {
		result := schemeEval(t, engine, `(environment-bound? (interaction-environment) '+)`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("environment-bound? false for unbound", func(t *testing.T) {
		result := schemeEval(t, engine, `(environment-bound? (interaction-environment) 'nonexistent-xyz)`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})

	t.Run("environment-bound? wrong env type", func(t *testing.T) {
		evalExpectError(t, engine, `(environment-bound? 42 '+)`)
	})

	t.Run("environment-bound? wrong symbol type", func(t *testing.T) {
		evalExpectError(t, engine, `(environment-bound? (interaction-environment) 42)`)
	})
}

func TestEnvironmentBoundNamesResult(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("includes known core primitive", func(t *testing.T) {
		// + is always available in the core registry
		result := schemeEval(t, engine, `
			(let loop ((names (environment-bound-names (interaction-environment))))
			  (cond
			    ((null? names) #f)
			    ((equal? (car names) '+) #t)
			    (else (loop (cdr names)))))
		`)
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.TrueValue)
	})
}
