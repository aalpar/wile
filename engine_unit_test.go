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

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/werr"

	qt "github.com/frankban/quicktest"
)

func TestEngineEval(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	c.Assert(err, qt.IsNil)

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
			result, err := engine.Eval(ctx, tc.code)
			c.Assert(err, qt.IsNil, qt.Commentf("code: %s", tc.code))
			c.Assert(result.SchemeString(), qt.Equals, tc.wantStr)
		})
	}
}

func TestEngineEvalMultiple(t *testing.T) {
	c := qt.New(t)
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
			c.Assert(err, qt.IsNil)

			result, err := engine.EvalMultiple(ctx, tc.code)
			c.Assert(err, qt.IsNil, qt.Commentf("code: %s", tc.code))
			c.Assert(result.SchemeString(), qt.Equals, tc.wantStr)
		})
	}
}

func TestEngineCompileAndRun(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	c.Assert(err, qt.IsNil)

	t.Run("compile and run", func(t *testing.T) {
		compiled, err := engine.Compile(ctx, "(+ 1 2)")
		c.Assert(err, qt.IsNil)

		result, err := engine.Run(ctx, compiled)
		c.Assert(err, qt.IsNil)
		c.Assert(result.SchemeString(), qt.Equals, "3")
	})

	t.Run("compiled code is reusable", func(t *testing.T) {
		compiled, err := engine.Compile(ctx, "(+ 1 2)")
		c.Assert(err, qt.IsNil)

		result1, err := engine.Run(ctx, compiled)
		c.Assert(err, qt.IsNil)

		result2, err := engine.Run(ctx, compiled)
		c.Assert(err, qt.IsNil)

		c.Assert(result1.SchemeString(), qt.Equals, result2.SchemeString())
	})

	t.Run("compile invalid syntax returns CompilationError", func(t *testing.T) {
		_, err := engine.Compile(ctx, "(")
		c.Assert(err, qt.IsNotNil)

		var compErr *wile.CompilationError
		c.Assert(errors.As(err, &compErr), qt.IsTrue)
	})
}

func TestEngineDefineAndGet(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	c.Assert(err, qt.IsNil)

	t.Run("define and retrieve", func(t *testing.T) {
		err := engine.Define("x", wile.NewInteger(42))
		c.Assert(err, qt.IsNil)

		val, ok := engine.Get("x")
		c.Assert(ok, qt.IsTrue)
		c.Assert(val.SchemeString(), qt.Equals, "42")
	})

	t.Run("get nonexistent returns not found", func(t *testing.T) {
		_, ok := engine.Get("nonexistent")
		c.Assert(ok, qt.IsFalse)
	})
}

func TestEngineCall(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	c.Assert(err, qt.IsNil)

	_, err = engine.EvalMultiple(ctx, `(define (add a b) (+ a b))`)
	c.Assert(err, qt.IsNil)

	proc, ok := engine.Get("add")
	c.Assert(ok, qt.IsTrue)

	result, err := engine.Call(ctx, proc, wile.NewInteger(1), wile.NewInteger(2))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "3")
}

func TestEngineClose(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	t.Run("close does not panic", func(t *testing.T) {
		engine, err := wile.NewEngine(ctx)
		c.Assert(err, qt.IsNil)

		err = engine.Close()
		c.Assert(err, qt.IsNil)
	})

	t.Run("double close does not panic", func(t *testing.T) {
		engine, err := wile.NewEngine(ctx)
		c.Assert(err, qt.IsNil)

		err = engine.Close()
		c.Assert(err, qt.IsNil)

		err = engine.Close()
		c.Assert(err, qt.IsNotNil)
		c.Assert(errors.Is(err, wile.ErrEngineClosed), qt.IsTrue)
	})
}

func TestEngineErrorWrapping(t *testing.T) {
	c := qt.New(t)
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
			c.Assert(err, qt.IsNil)

			_, err = engine.Eval(ctx, tc.code)
			c.Assert(err, qt.IsNotNil)

			switch tc.errType {
			case "runtime":
				var rtErr *wile.RuntimeError
				c.Assert(errors.As(err, &rtErr), qt.IsTrue,
					qt.Commentf("expected RuntimeError for %q, got %T: %v", tc.code, err, err))
			case "compilation":
				var compErr *wile.CompilationError
				c.Assert(errors.As(err, &compErr), qt.IsTrue,
					qt.Commentf("expected CompilationError for %q, got %T: %v", tc.code, err, err))
			}
		})
	}
}

func TestEngineOptions(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	t.Run("WithMaxCallDepth limits recursion", func(t *testing.T) {
		engine, err := wile.NewEngine(ctx, wile.WithMaxCallDepth(5))
		c.Assert(err, qt.IsNil)

		// Non-tail recursion to force continuation stack growth.
		_, err = engine.Eval(ctx, "(define (f n) (+ 1 (f (+ n 1))))")
		c.Assert(err, qt.IsNil)

		_, err = engine.Eval(ctx, "(f 0)")
		c.Assert(err, qt.IsNotNil)
		c.Assert(errors.Is(err, werr.ErrCallDepthExceeded), qt.IsTrue)
	})
}
