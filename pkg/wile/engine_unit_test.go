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

package wile_test

import (
	"context"
	"errors"
	"testing"

	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

func TestEngineEval(t *testing.T) {
	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	tcs := []struct {
		name    string
		code    string
		wantStr string
	}{
		{"integer arithmetic", "(+ 1 2)", "3"},
		{"string literal", `"hello"`, `"hello"`},
		{"boolean true", "#t", "#t"},
		{"empty list", "'()", "()"},
		{"if expression", "(if #t 'yes 'no)", "yes"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := engine.Eval(ctx, engine.MustParse(ctx, tc.code))
			qt.Assert(t, err, qt.IsNil, qt.Commentf("code: %s", tc.code))
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.wantStr)
		})
	}
}

func TestEngineEvalMultiple(t *testing.T) {
	ctx := context.Background()

	tcs := []struct {
		name    string
		code    string
		wantStr string
	}{
		{
			"define and use variables",
			"(define x 1) (define y 2) (+ x y)",
			"3",
		},
		{
			"define and call lambda",
			"(define f (lambda (x) (* x x))) (f 5)",
			"25",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			engine, err := wile.NewEngine(ctx)
			qt.Assert(t, err, qt.IsNil)

			result, err := engine.EvalMultiple(ctx, tc.code)
			qt.Assert(t, err, qt.IsNil, qt.Commentf("code: %s", tc.code))
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.wantStr)
		})
	}
}

func TestEngineCompileAndRun(t *testing.T) {
	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	t.Run("compile and run", func(t *testing.T) {
		compiled, err := engine.Compile(ctx, engine.MustParse(ctx, "(+ 1 2)"))
		qt.Assert(t, err, qt.IsNil)

		result, err := engine.Run(ctx, compiled)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result.SchemeString(), qt.Equals, "3")
	})

	t.Run("compiled code is reusable", func(t *testing.T) {
		compiled, err := engine.Compile(ctx, engine.MustParse(ctx, "(+ 1 2)"))
		qt.Assert(t, err, qt.IsNil)

		result1, err := engine.Run(ctx, compiled)
		qt.Assert(t, err, qt.IsNil)

		result2, err := engine.Run(ctx, compiled)
		qt.Assert(t, err, qt.IsNil)

		qt.Assert(t, result1.SchemeString(), qt.Equals, result2.SchemeString())
	})
}

func TestEngineDefineAndGet(t *testing.T) {
	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	t.Run("define and retrieve", func(t *testing.T) {
		err := engine.Define("x", wile.NewInteger(42))
		qt.Assert(t, err, qt.IsNil)

		val, ok := engine.Get("x")
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, val.SchemeString(), qt.Equals, "42")
	})

	t.Run("get nonexistent returns not found", func(t *testing.T) {
		_, ok := engine.Get("nonexistent")
		qt.Assert(t, ok, qt.IsFalse)
	})
}

func TestEngineCall(t *testing.T) {
	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	_, err = engine.EvalMultiple(ctx, `(define (add a b) (+ a b))`)
	qt.Assert(t, err, qt.IsNil)

	proc, ok := engine.Get("add")
	qt.Assert(t, ok, qt.IsTrue)

	result, err := engine.Call(ctx, proc, wile.NewInteger(1), wile.NewInteger(2))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "3")
}

func TestEngineClose(t *testing.T) {
	ctx := context.Background()

	t.Run("close does not panic", func(t *testing.T) {
		engine, err := wile.NewEngine(ctx)
		qt.Assert(t, err, qt.IsNil)

		err = engine.Close()
		qt.Assert(t, err, qt.IsNil)
	})

	t.Run("double close does not panic", func(t *testing.T) {
		engine, err := wile.NewEngine(ctx)
		qt.Assert(t, err, qt.IsNil)

		err = engine.Close()
		qt.Assert(t, err, qt.IsNil)

		err = engine.Close()
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, errors.Is(err, wile.ErrEngineClosed), qt.IsTrue)
	})
}

func TestEngineErrorWrapping(t *testing.T) {
	ctx := context.Background()

	tcs := []struct {
		name    string
		code    string
		errType string // "runtime" or "compilation"
	}{
		{"division by zero", "(/ 1 0)", "runtime"},
		{"invalid syntax", "(if)", "compilation"},
		{"undefined variable", "undefined-var", "compilation"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			engine, err := wile.NewEngine(ctx)
			qt.Assert(t, err, qt.IsNil)

			expr, parseErr := engine.Parse(ctx, tc.code)
			if parseErr != nil {
				// Parse error — must be a compilation error
				var compErr *wile.CompilationError
				qt.Assert(t, errors.As(parseErr, &compErr), qt.IsTrue,
					qt.Commentf("expected CompilationError for %q, got %T: %v", tc.code, parseErr, parseErr))
				return
			}
			_, err = engine.Eval(ctx, expr)
			qt.Assert(t, err, qt.IsNotNil)

			switch tc.errType {
			case "runtime":
				var rtErr *wile.RuntimeError
				qt.Assert(t, errors.As(err, &rtErr), qt.IsTrue,
					qt.Commentf("expected RuntimeError for %q, got %T: %v", tc.code, err, err))
			case "compilation":
				var compErr *wile.CompilationError
				qt.Assert(t, errors.As(err, &compErr), qt.IsTrue,
					qt.Commentf("expected CompilationError for %q, got %T: %v", tc.code, err, err))
			}
		})
	}
}

func TestEngineParse(t *testing.T) {
	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	t.Run("single expression succeeds", func(t *testing.T) {
		expr, err := engine.Parse(ctx, "(+ 1 2)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, expr, qt.IsNotNil)
		qt.Assert(t, expr.String(), qt.Equals, "#<expression>")
	})

	t.Run("trailing input errors with CompilationError", func(t *testing.T) {
		_, err := engine.Parse(ctx, "(+ 1 2) (+ 3 4)")
		qt.Assert(t, err, qt.IsNotNil)

		var compErr *wile.CompilationError
		qt.Assert(t, errors.As(err, &compErr), qt.IsTrue)
		qt.Assert(t, compErr.Message, qt.Matches, "trailing input.*")
	})

	t.Run("empty input errors with CompilationError", func(t *testing.T) {
		_, err := engine.Parse(ctx, "")
		qt.Assert(t, err, qt.IsNotNil)

		var compErr *wile.CompilationError
		qt.Assert(t, errors.As(err, &compErr), qt.IsTrue)
	})

	t.Run("parse error from malformed input", func(t *testing.T) {
		_, err := engine.Parse(ctx, "(")
		qt.Assert(t, err, qt.IsNotNil)

		var compErr *wile.CompilationError
		qt.Assert(t, errors.As(err, &compErr), qt.IsTrue)
	})
}

func TestEngineParseWithSource(t *testing.T) {
	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	expr, err := engine.ParseWithSource(ctx, "(+ 1 2)", "test-file.scm")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, expr, qt.IsNotNil)
	qt.Assert(t, expr.Source(), qt.Equals, "test-file.scm")
}

func TestEngineMustParse(t *testing.T) {
	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	t.Run("valid input succeeds", func(t *testing.T) {
		expr := engine.MustParse(ctx, "(+ 1 2)")
		qt.Assert(t, expr, qt.IsNotNil)
		qt.Assert(t, expr.String(), qt.Equals, "#<expression>")
	})

	t.Run("invalid input panics", func(t *testing.T) {
		qt.Assert(t, func() {
			engine.MustParse(ctx, "(")
		}, qt.PanicMatches, ".*")
	})

	t.Run("trailing input panics", func(t *testing.T) {
		qt.Assert(t, func() {
			engine.MustParse(ctx, "(+ 1 2) (+ 3 4)")
		}, qt.PanicMatches, ".*")
	})
}

func TestEngineOptions(t *testing.T) {
	ctx := context.Background()

	t.Run("WithMaxCallDepth limits recursion", func(t *testing.T) {
		engine, err := wile.NewEngine(ctx, wile.WithMaxCallDepth(5))
		qt.Assert(t, err, qt.IsNil)

		// Non-tail recursion to force continuation stack growth.
		_, err = engine.EvalMultiple(ctx, "(define (f n) (+ 1 (f (+ n 1))))")
		qt.Assert(t, err, qt.IsNil)

		_, err = engine.Eval(ctx, engine.MustParse(ctx, "(f 0)"))
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, errors.Is(err, werr.ErrCallDepthExceeded), qt.IsTrue)
	})
}
