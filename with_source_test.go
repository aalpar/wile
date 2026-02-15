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

package wile

import (
	"context"
	"errors"
	"strings"
	"testing"

	"github.com/aalpar/wile/extensions/exceptions"

	qt "github.com/frankban/quicktest"
)

// TestEvalWithSource_RuntimeError verifies that EvalWithSource populates
// RuntimeError.Source with the filename, line, and column when a runtime
// error occurs.
func TestEvalWithSource_RuntimeError(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name          string
		source        string
		code          string
		wantSourcePfx string // Source field must start with this prefix
		wantCondition string // Condition.SchemeString(), empty to skip check
		wantHasSource bool
		wantHasTrace  bool
	}{
		{
			name:          "raise with source",
			source:        "config.scm",
			code:          `(raise "boom")`,
			wantSourcePfx: "config.scm:",
			wantCondition: `"boom"`,
			wantHasSource: true,
			wantHasTrace:  true,
		},
		{
			name:          "error with source",
			source:        "app.scm",
			code:          `(error "something went wrong" 42)`,
			wantSourcePfx: "app.scm:",
			wantCondition: "",
			wantHasSource: true,
			wantHasTrace:  true,
		},
		{
			name:          "raise without source",
			source:        "",
			code:          `(raise "boom")`,
			wantSourcePfx: "",
			wantCondition: `"boom"`,
			wantHasSource: false,
			wantHasTrace:  false,
		},
		{
			name:          "nested call error with source",
			source:        "nested.scm",
			code:          `(define (f) (raise "inner"))  (f)`,
			wantSourcePfx: "nested.scm:",
			wantCondition: `"inner"`,
			wantHasSource: true,
			wantHasTrace:  true,
		},
		{
			name:          "path-like source",
			source:        "/etc/config/rules.scm",
			code:          `(raise "fail")`,
			wantSourcePfx: "/etc/config/rules.scm:",
			wantCondition: `"fail"`,
			wantHasSource: true,
			wantHasTrace:  true,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			engine, err := NewEngine(context.Background(), WithExtension(exceptions.Extension))
			c.Assert(err, qt.IsNil)

			ctx := context.Background()
			var evalErr error
			if tc.source != "" {
				_, evalErr = engine.EvalMultipleWithSource(ctx, tc.code, tc.source)
			} else {
				_, evalErr = engine.EvalMultiple(ctx, tc.code)
			}

			c.Assert(evalErr, qt.IsNotNil)

			var rtErr *RuntimeError
			c.Assert(errors.As(evalErr, &rtErr), qt.IsTrue)

			if tc.wantHasSource {
				c.Assert(rtErr.Source, qt.Not(qt.Equals), "")
				c.Assert(strings.HasPrefix(rtErr.Source, tc.wantSourcePfx), qt.IsTrue,
					qt.Commentf("Source=%q want prefix=%q", rtErr.Source, tc.wantSourcePfx))
				// Source format is "file:line:col" — verify it has at least two colons
				parts := strings.Split(rtErr.Source, ":")
				c.Assert(len(parts) >= 3, qt.IsTrue,
					qt.Commentf("Source=%q doesn't match file:line:col format", rtErr.Source))
			} else {
				c.Assert(rtErr.Source, qt.Equals, "")
			}

			if tc.wantHasTrace {
				c.Assert(rtErr.StackTrace, qt.Not(qt.Equals), "")
			}

			if tc.wantCondition != "" {
				c.Assert(rtErr.Condition, qt.IsNotNil)
				c.Assert(rtErr.Condition.SchemeString(), qt.Equals, tc.wantCondition)
			}

			// Source should appear in Error() output when present
			if tc.wantHasSource {
				c.Assert(strings.Contains(rtErr.Error(), tc.wantSourcePfx), qt.IsTrue,
					qt.Commentf("Error()=%q should contain %q", rtErr.Error(), tc.wantSourcePfx))
			}
		})
	}
}

// TestEvalWithSource_Success verifies that EvalWithSource returns correct
// results for code that does not error.
func TestEvalWithSource_Success(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name   string
		source string
		code   string
		want   string
	}{
		{
			name:   "simple expression",
			source: "math.scm",
			code:   "(+ 1 2)",
			want:   "3",
		},
		{
			name:   "string result",
			source: "greeting.scm",
			code:   `(string-append "hello" " " "world")`,
			want:   `"hello world"`,
		},
		{
			name:   "empty source string",
			source: "",
			code:   "(* 6 7)",
			want:   "42",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			engine, err := NewEngine(context.Background())
			c.Assert(err, qt.IsNil)

			ctx := context.Background()
			result, err := engine.EvalWithSource(ctx, tc.code, tc.source)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestEvalMultipleWithSource_RuntimeError verifies that source tracking
// works across multiple expressions.
func TestEvalMultipleWithSource_RuntimeError(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background(), WithExtension(exceptions.Extension))
	c.Assert(err, qt.IsNil)

	ctx := context.Background()
	// The error occurs in the second expression on line 2.
	code := "(define x 10)\n(error \"failure\" x)"
	_, err = engine.EvalMultipleWithSource(ctx, code, "multi.scm")
	c.Assert(err, qt.IsNotNil)

	var rtErr *RuntimeError
	c.Assert(errors.As(err, &rtErr), qt.IsTrue)
	c.Assert(strings.HasPrefix(rtErr.Source, "multi.scm:"), qt.IsTrue,
		qt.Commentf("Source=%q", rtErr.Source))
}

// TestEvalMultipleWithSource_Success verifies that EvalMultipleWithSource
// returns the result of the last expression.
func TestEvalMultipleWithSource_Success(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	ctx := context.Background()
	result, err := engine.EvalMultipleWithSource(ctx,
		"(define x 10) (define y 20) (+ x y)",
		"calc.scm")
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "30")
}

// TestCompileWithSource_RuntimeError verifies that source information
// survives the compile → run boundary. Code compiled with CompileWithSource
// and later executed with Run should still carry source info in errors.
func TestCompileWithSource_RuntimeError(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background(), WithExtension(exceptions.Extension))
	c.Assert(err, qt.IsNil)

	ctx := context.Background()
	compiled, err := engine.CompileWithSource(ctx, `(raise "compiled-boom")`, "compiled.scm")
	c.Assert(err, qt.IsNil)

	_, err = engine.Run(ctx, compiled)
	c.Assert(err, qt.IsNotNil)

	var rtErr *RuntimeError
	c.Assert(errors.As(err, &rtErr), qt.IsTrue)
	c.Assert(strings.HasPrefix(rtErr.Source, "compiled.scm:"), qt.IsTrue,
		qt.Commentf("Source=%q", rtErr.Source))
	c.Assert(rtErr.Condition.SchemeString(), qt.Equals, `"compiled-boom"`)
}

// TestCompileWithSource_ParseError verifies that CompileWithSource returns
// a CompilationError for malformed input.
func TestCompileWithSource_ParseError(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	ctx := context.Background()
	_, err = engine.CompileWithSource(ctx, "(", "broken.scm")
	c.Assert(err, qt.IsNotNil)

	var compErr *CompilationError
	c.Assert(errors.As(err, &compErr), qt.IsTrue)
}

// TestWithSource_DistinctSources verifies that different source strings
// produce distinct Source fields in errors — the source string is not
// cached or shared between calls.
func TestWithSource_DistinctSources(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name   string
		source string
	}{
		{"alpha", "alpha.scm"},
		{"beta", "beta.scm"},
		{"gamma", "scripts/gamma.scm"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			engine, err := NewEngine(context.Background(), WithExtension(exceptions.Extension))
			c.Assert(err, qt.IsNil)

			ctx := context.Background()
			_, err = engine.EvalWithSource(ctx, `(raise "x")`, tc.source)

			var rtErr *RuntimeError
			c.Assert(errors.As(err, &rtErr), qt.IsTrue)
			c.Assert(strings.HasPrefix(rtErr.Source, tc.source+":"), qt.IsTrue,
				qt.Commentf("Source=%q want prefix=%q:", rtErr.Source, tc.source))
		})
	}
}

// TestWithSource_SourcelessEvalHasEmptySource is a negative test: errors
// from the sourceless Eval/EvalMultiple/Compile should have empty Source.
func TestWithSource_SourcelessEvalHasEmptySource(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		eval func(*Engine, context.Context) error
	}{
		{
			name: "Eval",
			eval: func(e *Engine, ctx context.Context) error {
				_, err := e.Eval(ctx, `(raise "x")`)
				return err
			},
		},
		{
			name: "EvalMultiple",
			eval: func(e *Engine, ctx context.Context) error {
				_, err := e.EvalMultiple(ctx, `(raise "x")`)
				return err
			},
		},
		{
			name: "Compile+Run",
			eval: func(e *Engine, ctx context.Context) error {
				cc, err := e.Compile(ctx, `(raise "x")`)
				if err != nil {
					return err
				}
				_, err = e.Run(ctx, cc)
				return err
			},
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			engine, err := NewEngine(context.Background(), WithExtension(exceptions.Extension))
			c.Assert(err, qt.IsNil)

			err = tc.eval(engine, context.Background())
			c.Assert(err, qt.IsNotNil)

			var rtErr *RuntimeError
			c.Assert(errors.As(err, &rtErr), qt.IsTrue)
			c.Assert(rtErr.Source, qt.Equals, "")
		})
	}
}

// TestCompileWithSource_ReusedCompiledCode verifies that a single
// CompileWithSource result can be Run multiple times, each retaining
// the source information in errors.
func TestCompileWithSource_ReusedCompiledCode(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background(), WithExtension(exceptions.Extension))
	c.Assert(err, qt.IsNil)

	ctx := context.Background()
	compiled, err := engine.CompileWithSource(ctx, `(raise "again")`, "reuse.scm")
	c.Assert(err, qt.IsNil)

	for i := range 3 {
		_, err = engine.Run(ctx, compiled)
		c.Assert(err, qt.IsNotNil)

		var rtErr *RuntimeError
		c.Assert(errors.As(err, &rtErr), qt.IsTrue)
		c.Assert(strings.HasPrefix(rtErr.Source, "reuse.scm:"), qt.IsTrue,
			qt.Commentf("run %d: Source=%q", i, rtErr.Source))
	}
}

// TestEvalWithSource_ErrorFormat verifies the full Error() string format
// includes the source prefix.
func TestEvalWithSource_ErrorFormat(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background(), WithExtension(exceptions.Extension))
	c.Assert(err, qt.IsNil)

	ctx := context.Background()
	_, err = engine.EvalWithSource(ctx, `(raise "fmt-test")`, "format.scm")

	var rtErr *RuntimeError
	c.Assert(errors.As(err, &rtErr), qt.IsTrue)

	// Error() should start with "format.scm:line:col: runtime error: ..."
	c.Assert(strings.HasPrefix(rtErr.Error(), "format.scm:"), qt.IsTrue,
		qt.Commentf("Error()=%q", rtErr.Error()))
	c.Assert(strings.Contains(rtErr.Error(), "runtime error"), qt.IsTrue)
}
