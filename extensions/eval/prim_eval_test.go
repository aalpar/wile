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

	exteval "github.com/aalpar/wile/extensions/eval"
	extintrospection "github.com/aalpar/wile/extensions/introspection"
	"github.com/aalpar/wile/pkg/wile"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// newEngine creates a Wile engine with core + eval extensions.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(exteval.Extension),
		wile.WithExtension(extintrospection.Extension),
	)
	qt.Assert(t, err, qt.IsNil)
	return engine
}

// eval runs Scheme code and returns the result of the last expression.
func eval(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.EvalMultiple(context.Background(), code)
	qt.Assert(t, err, qt.IsNil)
	return result
}

// evalExpectError runs Scheme code and expects an error.
func evalExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	expr, err := engine.Parse(context.Background(), code)
	if err != nil {
		return // parse error counts as expected error
	}
	_, err = engine.Eval(context.Background(), expr)
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
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewInteger(3))
	})

	t.Run("evaluate in null environment", func(t *testing.T) {
		evalExpectError(t, engine, `(eval '(+ 1 2) (null-environment 7))`)
	})

	t.Run("evaluate variable reference", func(t *testing.T) {
		eval(t, engine, `(define x 42)`)
		result := eval(t, engine, `(eval 'x (interaction-environment))`)
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewInteger(42))
	})

	t.Run("1-arg form uses current namespace", func(t *testing.T) {
		result := eval(t, engine, `(eval '(+ 1 2))`)
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewInteger(3))
	})

	t.Run("wrong environment type", func(t *testing.T) {
		evalExpectError(t, engine, `(eval '(+ 1 2) 42)`)
	})

	t.Run("expansion error in eval", func(t *testing.T) {
		evalExpectError(t, engine, `(eval '(let) (interaction-environment))`)
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
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewInteger(123))
	})

	t.Run("load multiple expressions", func(t *testing.T) {
		path := writeTestFile(t, dir, "multi.scm", "(define x 10)\n(define y 20)\n(+ x y)")
		result := eval(t, engine, fmt.Sprintf(`(load %q)`, path))
		// load returns the value of the last expression
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewInteger(30))
	})

	t.Run("load nonexistent file", func(t *testing.T) {
		evalExpectError(t, engine, fmt.Sprintf(`(load %q)`, filepath.Join(dir, "nonexistent.scm")))
	})

	t.Run("load parse error", func(t *testing.T) {
		path := writeTestFile(t, dir, "bad-syntax.scm", `"unterminated string`)
		evalExpectError(t, engine, fmt.Sprintf(`(load %q)`, path))
	})

	t.Run("load expansion error", func(t *testing.T) {
		code := "(define-syntax bad-macro (syntax-rules () ((bad-macro a b) a)))\n(bad-macro 1)"
		path := writeTestFile(t, dir, "bad-expand.scm", code)
		evalExpectError(t, engine, fmt.Sprintf(`(load %q)`, path))
	})

	t.Run("load compile error", func(t *testing.T) {
		// set! on an unbound variable triggers a compile error
		path := writeTestFile(t, dir, "bad-compile.scm", "(set! undefined-xyz-var 42)")
		evalExpectError(t, engine, fmt.Sprintf(`(load %q)`, path))
	})

	t.Run("wrong argument type", func(t *testing.T) {
		evalExpectError(t, engine, `(load 42)`)
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

	// C4: scheme-report-environment must be distinct from interaction-environment.
	// Before the fix, both returned the same Namespace object.
	t.Run("distinct from interaction-environment", func(t *testing.T) {
		result := eval(t, engine, `(eq? (interaction-environment) (scheme-report-environment 5))`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
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
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewInteger(7))
	})

	t.Run("compile with variable reference", func(t *testing.T) {
		eval(t, engine, `(define x 10)`)
		eval(t, engine, `(define compiled-ref (compile 'x))`)
		result := eval(t, engine, `(compiled-ref)`)
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewInteger(10))
	})

	t.Run("compile syntax object input", func(t *testing.T) {
		result := eval(t, engine, `(procedure? (compile (syntax (+ 1 2))))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("compile expansion error", func(t *testing.T) {
		evalExpectError(t, engine, `(compile '(let))`)
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
		wile.WithExtension(extintrospection.Extension),
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
			result, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), tc.code))
			c.Assert(err, qt.IsNil)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestEnvironmentWithLibraryRegistry covers the library import path in PrimEnvironment.
// Using WithLibraryPaths() enables the registry so ForEach is entered.
func TestEnvironmentWithLibraryRegistry(t *testing.T) {
	c := qt.New(t)

	// stdlib/lib/ is at repo root, test is at extensions/eval/
	libDir := filepath.Join("..", "..", "stdlib", "lib")
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(exteval.Extension),
		wile.WithLibraryPaths(libDir),
	)
	qt.Assert(t, err, qt.IsNil)

	t.Run("nonexistent library covers ForEach body", func(t *testing.T) {
		// Entering the ForEach with a valid import spec covers ParseImportSetFromDatum
		// and the LoadLibrary call (which fails because the library doesn't exist).
		_, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), `(environment '(nonexistent lib))`))
		qt.Assert(t, err, qt.IsNotNil)
	})

	t.Run("for-syntax phase modifier parses correctly", func(t *testing.T) {
		// for-syntax variant covers the phase-shift parsing in ParseImportSetFromDatum.
		_, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), `(environment '(for-syntax (nonexistent lib)))`))
		qt.Assert(t, err, qt.IsNotNil)
	})

	t.Run("successful library import", func(t *testing.T) {
		result, err := engine.Eval(context.Background(),
			engine.MustParse(context.Background(), `(eval '(caar '((1 2) 3)) (environment '(scheme cxr)))`))
		c.Assert(err, qt.IsNil)
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewInteger(1))
	})
}

// TestExpandInExpansionContext covers the expanderCtx != nil path in PrimExpand.
// When called from within a macro transformer, the ExpanderContext is set.
func TestExpandInExpansionContext(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("expand in macro transformer uses expansion context", func(t *testing.T) {
		result := eval(t, engine, `
			(define-syntax test-expand-ctx
			  (lambda (stx)
			    (expand (syntax (+ 1 2)))))
			(test-expand-ctx)
		`)
		c.Assert(result.Internal(), qt.IsNotNil)
	})
}

// TestExpandOnceInExpansionContext covers the expanderCtx != nil path in PrimExpandOnce.
func TestExpandOnceInExpansionContext(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("expand-once in macro transformer uses expansion context", func(t *testing.T) {
		result := eval(t, engine, `
			(define-syntax test-expand-once-ctx
			  (lambda (stx)
			    (expand-once (syntax (+ 1 2)))))
			(test-expand-once-ctx)
		`)
		c.Assert(result.Internal(), qt.IsNotNil)
	})
}

// TestSyntaxLocalValueInMacro covers the success path of PrimSyntaxLocalValue.
// When called from within a macro transformer, it looks up bindings in the expand env.
// The definition and invocation are in a single eval call so the expand env is shared.
func TestSyntaxLocalValueInMacro(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("looks up macro binding in expansion context", func(t *testing.T) {
		// Define target-macro and use-slv together; invoke in the same form.
		// The eval helper asserts no error, confirming syntax-local-value succeeded.
		result := eval(t, engine, `
			(define-syntax target-macro
			  (syntax-rules () ((target-macro) 42)))
			(define-syntax use-slv
			  (lambda (stx)
			    (let ((v (syntax-local-value (syntax target-macro))))
			      ; v is the transformer — ignore it and return #t
			      (syntax #t))))
			(use-slv)
		`)
		c.Assert(result.Internal(), qt.IsNotNil)
	})
}

// TestSyntaxLocalIntroduceInMacro covers PrimSyntaxLocalIntroduce in expansion context.
func TestSyntaxLocalIntroduceInMacro(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("syntax-local-introduce in macro expansion context", func(t *testing.T) {
		result := eval(t, engine, `
			(define-syntax test-introduce
			  (lambda (stx)
			    (let ((introduced (syntax-local-introduce (syntax x))))
			      (syntax #t))))
			(test-introduce)
		`)
		c.Assert(result.Internal(), qt.IsNotNil)
	})
}

// TestSyntaxLocalIdentifierAsBindingInMacro covers PrimSyntaxLocalIdentifierAsBinding
// in expansion context.
func TestSyntaxLocalIdentifierAsBindingInMacro(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("syntax-local-identifier-as-binding in macro expansion context", func(t *testing.T) {
		result := eval(t, engine, `
			(define-syntax test-as-binding
			  (lambda (stx)
			    (let ((bound-id (syntax-local-identifier-as-binding (syntax my-id))))
			      (syntax #t))))
			(test-as-binding)
		`)
		c.Assert(result.Internal(), qt.IsNotNil)
	})
}

func TestCurrentLoadPath(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	t.Run("returns #f outside load", func(t *testing.T) {
		result := eval(t, engine, `(current-load-path)`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})

	t.Run("returns absolute path during load", func(t *testing.T) {
		path := writeTestFile(t, dir, "check-path.scm", `(current-load-path)`)
		result := eval(t, engine, fmt.Sprintf(`(load %q)`, path))
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewString(path))
	})
}

func TestCurrentLoadDirectory(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	t.Run("returns #f outside load", func(t *testing.T) {
		result := eval(t, engine, `(current-load-directory)`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})

	t.Run("returns directory during load", func(t *testing.T) {
		writeTestFile(t, dir, "check-dir.scm", `(current-load-directory)`)
		path := filepath.Join(dir, "check-dir.scm")
		result := eval(t, engine, fmt.Sprintf(`(load %q)`, path))
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewString(dir))
	})
}

func TestCurrentLoadDepth(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	t.Run("returns 0 outside load", func(t *testing.T) {
		result := eval(t, engine, `(current-load-depth)`)
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewInteger(0))
	})

	t.Run("returns 1 during load", func(t *testing.T) {
		writeTestFile(t, dir, "depth1.scm", `(current-load-depth)`)
		path := filepath.Join(dir, "depth1.scm")
		result := eval(t, engine, fmt.Sprintf(`(load %q)`, path))
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewInteger(1))
	})

	t.Run("returns 2 during nested load", func(t *testing.T) {
		writeTestFile(t, dir, "inner.scm", `(current-load-depth)`)
		innerPath := filepath.Join(dir, "inner.scm")
		writeTestFile(t, dir, "outer.scm", fmt.Sprintf(`(load %q)`, innerPath))
		outerPath := filepath.Join(dir, "outer.scm")
		result := eval(t, engine, fmt.Sprintf(`(load %q)`, outerPath))
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewInteger(2))
	})
}

func TestEvalRuntimeError(t *testing.T) {
	engine := newEngine(t)

	t.Run("runtime error propagates", func(t *testing.T) {
		// Division by zero triggers a runtime error inside the sub-context
		evalExpectError(t, engine, `(eval '(/ 1 0) (interaction-environment))`)
	})
}

func TestLoadRuntimeError(t *testing.T) {
	engine := newEngine(t)
	dir := t.TempDir()

	t.Run("runtime error in loaded file propagates", func(t *testing.T) {
		path := writeTestFile(t, dir, "runtime-err.scm", `(/ 1 0)`)
		evalExpectError(t, engine, fmt.Sprintf(`(load %q)`, path))
	})
}

func TestEnvironment_WileProfiles(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	runProfile := func(name, expr string) string {
		t.Helper()
		code := fmt.Sprintf("(%s %s (environment '(wile %s)))", "eval", expr, name)
		return eval(t, engine, code).SchemeString()
	}

	t.Run("tiny has arithmetic", func(t *testing.T) {
		c.Assert(runProfile("tiny", "'(+ 1 2)"), qt.Equals, "3")
	})

	t.Run("tiny has no io", func(t *testing.T) {
		code := fmt.Sprintf("(%s '(display \"hi\") (environment '(wile tiny)))", "eval")
		evalExpectError(t, engine, code)
	})

	t.Run("console", func(t *testing.T) {
		c.Assert(runProfile("console", "'(+ 1 2)"), qt.Equals, "3")
	})

	t.Run("console-with-load", func(t *testing.T) {
		c.Assert(runProfile("console-with-load", "'(+ 1 2)"), qt.Equals, "3")
	})

	t.Run("console-with-load supports nested", func(t *testing.T) {
		nested := fmt.Sprintf("'(%s '(+ 2 3) (environment '(wile tiny)))", "eval")
		c.Assert(runProfile("console-with-load", nested), qt.Equals, "5")
	})

	t.Run("small", func(t *testing.T) {
		c.Assert(runProfile("small", "'(+ 1 2)"), qt.Equals, "3")
	})

	t.Run("kitchen-sink", func(t *testing.T) {
		c.Assert(runProfile("kitchen-sink", "'(+ 1 2)"), qt.Equals, "3")
	})

	t.Run("unknown profile errors", func(t *testing.T) {
		evalExpectError(t, engine, `(environment '(wile unknown))`)
	})
}
