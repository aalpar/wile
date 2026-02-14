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

package eval_test

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"github.com/aalpar/wile"
	exteval "github.com/aalpar/wile/internal/extensions/eval"
	extexceptions "github.com/aalpar/wile/internal/extensions/exceptions"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// newEngine creates a Wile engine with core + eval extensions.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(exteval.Extension),
	)
	qt.Assert(t, err, qt.IsNil)
	return engine
}

// eval runs Scheme code and returns the result.
func eval(t *testing.T, engine *wile.Engine, code string) wile.Value {
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

// writeTestFile creates a file with the given contents in the temp directory.
func writeTestFile(t *testing.T, dir, name, contents string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	err := os.WriteFile(path, []byte(contents), 0o644)
	qt.Assert(t, err, qt.IsNil)
	return path
}

func TestEval(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("evaluate simple expression", func(t *testing.T) {
		result := eval(t, engine, `(eval '(+ 1 2) (interaction-environment))`)
		c.Assert(result.Internal(), values.SchemeEquals, values.NewInteger(3))
	})

	t.Run("evaluate in null environment", func(t *testing.T) {
		evalExpectError(t, engine, `(eval '(+ 1 2) (null-environment 7))`)
	})

	t.Run("evaluate variable reference", func(t *testing.T) {
		eval(t, engine, `(define x 42)`)
		result := eval(t, engine, `(eval 'x (interaction-environment))`)
		c.Assert(result.Internal(), values.SchemeEquals, values.NewInteger(42))
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		evalExpectError(t, engine, `(eval '(+ 1 2))`)
	})

	t.Run("wrong environment type", func(t *testing.T) {
		evalExpectError(t, engine, `(eval '(+ 1 2) 42)`)
	})
}

func TestLoad(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	t.Run("load defines variable", func(t *testing.T) {
		path := writeTestFile(t, dir, "def.scm", "(define loaded-value 123)")
		eval(t, engine, fmt.Sprintf(`(load %q)`, path))
		result := eval(t, engine, `loaded-value`)
		c.Assert(result.Internal(), values.SchemeEquals, values.NewInteger(123))
	})

	t.Run("load multiple expressions", func(t *testing.T) {
		path := writeTestFile(t, dir, "multi.scm", "(define x 10)\n(define y 20)\n(+ x y)")
		result := eval(t, engine, fmt.Sprintf(`(load %q)`, path))
		// load returns the value of the last expression
		c.Assert(result.Internal(), values.SchemeEquals, values.NewInteger(30))
	})

	t.Run("load nonexistent file", func(t *testing.T) {
		evalExpectError(t, engine, fmt.Sprintf(`(load %q)`, filepath.Join(dir, "nonexistent.scm")))
	})

	t.Run("wrong argument type", func(t *testing.T) {
		evalExpectError(t, engine, `(load 42)`)
	})
}

func TestInteractionEnvironment(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("returns environment object", func(t *testing.T) {
		// interaction-environment returns a SchemeEnvironment
		result := eval(t, engine, `(interaction-environment)`)
		c.Assert(result.Internal(), qt.IsNotNil)
	})

	t.Run("environment has standard bindings", func(t *testing.T) {
		result := eval(t, engine, `(eval '(+ 1 2) (interaction-environment))`)
		c.Assert(result.Internal(), values.SchemeEquals, values.NewInteger(3))
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(interaction-environment 42)`)
	})
}

func TestSchemeReportEnvironment(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("R5RS environment", func(t *testing.T) {
		result := eval(t, engine, `(scheme-report-environment 5)`)
		c.Assert(result.Internal(), qt.IsNotNil)
	})

	t.Run("R7RS environment", func(t *testing.T) {
		result := eval(t, engine, `(scheme-report-environment 7)`)
		c.Assert(result.Internal(), qt.IsNotNil)
	})

	t.Run("unsupported version", func(t *testing.T) {
		evalExpectError(t, engine, `(scheme-report-environment 4)`)
	})

	t.Run("wrong argument type", func(t *testing.T) {
		evalExpectError(t, engine, `(scheme-report-environment "5")`)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(scheme-report-environment)`)
	})
}

func TestNullEnvironment(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("null environment has no bindings", func(t *testing.T) {
		result := eval(t, engine, `(null-environment 7)`)
		c.Assert(result.Internal(), qt.IsNotNil)
	})

	t.Run("eval in null environment fails for unbound", func(t *testing.T) {
		evalExpectError(t, engine, `(eval '(+ 1 2) (null-environment 7))`)
	})

	t.Run("unsupported version", func(t *testing.T) {
		evalExpectError(t, engine, `(null-environment 4)`)
	})

	t.Run("wrong argument type", func(t *testing.T) {
		evalExpectError(t, engine, `(null-environment "7")`)
	})
}

func TestEnvironment(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("create empty environment", func(t *testing.T) {
		// environment with no args creates an empty environment
		result := eval(t, engine, `(environment)`)
		c.Assert(result.Internal(), qt.IsNotNil)
	})

	t.Run("eval in empty environment fails", func(t *testing.T) {
		// Empty environment has no bindings
		evalExpectError(t, engine, `(eval '(+ 1 2) (environment))`)
	})

	t.Run("wrong argument type", func(t *testing.T) {
		evalExpectError(t, engine, `(environment "foo")`)
	})
}

func TestExpand(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("expand macro application", func(t *testing.T) {
		eval(t, engine, `
			(define-syntax my-when
			  (syntax-rules ()
			    ((my-when test body ...)
			     (if test (begin body ...)))))
		`)
		result := eval(t, engine, `(expand (syntax (my-when #t (+ 1 2))))`)
		// Should expand to (if #t (begin (+ 1 2)))
		c.Assert(result.Internal(), qt.IsNotNil)
	})

	t.Run("expand non-macro returns same", func(t *testing.T) {
		result := eval(t, engine, `(expand (syntax (+ 1 2)))`)
		c.Assert(result.Internal(), qt.IsNotNil)
	})

	t.Run("wrong argument type", func(t *testing.T) {
		evalExpectError(t, engine, `(expand '(+ 1 2))`)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(expand)`)
	})
}

func TestExpandOnce(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("expand once macro application", func(t *testing.T) {
		eval(t, engine, `
			(define-syntax my-when
			  (syntax-rules ()
			    ((my-when test body ...)
			     (if test (begin body ...)))))
		`)
		result := eval(t, engine, `(expand-once (syntax (my-when #t (+ 1 2))))`)
		c.Assert(result.Internal(), qt.IsNotNil)
	})

	t.Run("expand once non-macro returns same", func(t *testing.T) {
		result := eval(t, engine, `(expand-once (syntax 42))`)
		c.Assert(result.Internal(), qt.IsNotNil)
	})

	t.Run("wrong argument type", func(t *testing.T) {
		evalExpectError(t, engine, `(expand-once '42)`)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(expand-once)`)
	})
}

func TestCompile(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("compile returns compiled code", func(t *testing.T) {
		result := eval(t, engine, `(procedure? (compile '(+ 1 2)))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("compiled code can be called", func(t *testing.T) {
		// compile creates a 0-arg thunk that evaluates the expression
		eval(t, engine, `(define compiled-expr (compile '(+ 3 4)))`)
		result := eval(t, engine, `(compiled-expr)`)
		c.Assert(result.Internal(), values.SchemeEquals, values.NewInteger(7))
	})

	t.Run("compile with variable reference", func(t *testing.T) {
		eval(t, engine, `(define x 10)`)
		eval(t, engine, `(define compiled-ref (compile 'x))`)
		result := eval(t, engine, `(compiled-ref)`)
		c.Assert(result.Internal(), values.SchemeEquals, values.NewInteger(10))
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(compile)`)
	})
}

func TestSyntaxLocalValue(t *testing.T) {
	engine := newEngine(t)

	t.Run("not in expansion context", func(t *testing.T) {
		eval(t, engine, `
			(define-syntax test-macro
			  (syntax-rules ()
			    ((test-macro x) x)))
		`)
		// syntax-local-value requires being called during macro expansion
		evalExpectError(t, engine, `(syntax-local-value (syntax test-macro))`)
	})

	t.Run("wrong argument type - plain symbol", func(t *testing.T) {
		evalExpectError(t, engine, `(syntax-local-value 'foo)`)
	})

	t.Run("wrong argument type - number", func(t *testing.T) {
		evalExpectError(t, engine, `(syntax-local-value 42)`)
	})
}

func TestMakeCompileTimeValue(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("create compile time value", func(t *testing.T) {
		result := eval(t, engine, `(make-compile-time-value 42)`)
		c.Assert(result.Internal(), qt.IsNotNil)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(make-compile-time-value)`)
	})
}

func TestSyntaxLocalIntroduce(t *testing.T) {
	engine := newEngine(t)

	t.Run("not in expansion context", func(t *testing.T) {
		// syntax-local-introduce requires being called during macro expansion
		evalExpectError(t, engine, `(syntax-local-introduce (syntax foo))`)
	})

	t.Run("wrong argument type", func(t *testing.T) {
		evalExpectError(t, engine, `(syntax-local-introduce 'foo)`)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(syntax-local-introduce)`)
	})
}

func TestSyntaxLocalIdentifierAsBinding(t *testing.T) {
	engine := newEngine(t)

	t.Run("not in expansion context", func(t *testing.T) {
		// syntax-local-identifier-as-binding requires being called during macro expansion
		evalExpectError(t, engine, `(syntax-local-identifier-as-binding (syntax test-id))`)
	})

	t.Run("wrong argument type", func(t *testing.T) {
		evalExpectError(t, engine, `(syntax-local-identifier-as-binding 42)`)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(syntax-local-identifier-as-binding)`)
	})
}

func TestEvalDynamicContextInheritance(t *testing.T) {
	c := qt.New(t)

	// Create engine with both the standard extension and exceptions extension
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(exteval.Extension),
		wile.WithExtension(extexceptions.Extension),
	)
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"inherits exception handler via raise-continuable",
			`(with-exception-handler
			   (lambda (e) 42)
			   (lambda ()
			     (eval '(raise-continuable "boom") (interaction-environment))))`,
			values.NewInteger(42)},
		{"handler sees raised condition",
			`(with-exception-handler
			   (lambda (e) (equal? e "oops"))
			   (lambda ()
			     (eval '(raise-continuable "oops") (interaction-environment))))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := engine.Eval(context.Background(), tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}
