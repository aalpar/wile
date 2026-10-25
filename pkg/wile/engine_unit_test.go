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
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

func TestEngineEval(t *testing.T) {
	t.Parallel()

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
	t.Parallel()

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
	t.Parallel()

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
	t.Parallel()

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

// A host Define must not land on a macro-introduced binding that happens to
// share the name.
//
// Define CREATES under the exact empty scope set (a distinct slot, since a
// macro-introduced binder carries its intro scope) but WRITES through
// NewGlobalIndex, a deferred index that resolves MATCH ANY — the name's first
// live slot. When a macro got there first, that slot is the macro's, so the
// host's value lands on the macro's variable and the host's own binding is
// left #!void. Create means exact-empty-set; write means any.
func TestEngineDefine_DoesNotClobberMacroIntroducedBinding(t *testing.T) {
	t.Parallel()

	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	// The macro's template introduces a top-level `zz`, keyed under its intro
	// scope, and exposes a reader so we can observe it from Scheme afterwards.
	_, err = engine.EvalMultiple(ctx, `
		(define-syntax mk
		  (syntax-rules ()
		    ((_ reader)
		     (begin (define zz 'macro-value)
		            (define (reader) zz)))))
		(mk read-zz)`)
	qt.Assert(t, err, qt.IsNil)

	err = engine.Define("zz", wile.NewInteger(42))
	qt.Assert(t, err, qt.IsNil)

	// The macro's own variable must be untouched by the host's Define.
	got, err := engine.EvalMultiple(ctx, `(read-zz)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.SchemeString(), qt.Equals, "macro-value",
		qt.Commentf("host Define wrote through the macro's slot"))

	// And the host must be able to read back what it defined.
	val, ok := engine.Get("zz")
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, val.SchemeString(), qt.Equals, "42",
		qt.Commentf("host's own binding was left unwritten"))
}

// The other half of Get's scoped resolution: a name bound ONLY under a macro's
// intro scope is reported absent, not handed to the host.
//
// The binding is real and the macro can use it, but no source-written reference
// can reach it either, so there is no name the host could legitimately use to
// ask for it. A wildcard Get would hand back a binding whose hygiene says it is
// not addressable.
func TestEngineGet_DoesNotSeeMacroOnlyBinding(t *testing.T) {
	t.Parallel()

	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	_, err = engine.EvalMultiple(ctx, `
		(define-syntax mk
		  (syntax-rules ()
		    ((_ reader)
		     (begin (define hidden 'macro-only)
		            (define (reader) hidden)))))
		(mk read-hidden)`)
	qt.Assert(t, err, qt.IsNil)

	// The macro itself can still reach its own binder.
	got, err := engine.EvalMultiple(ctx, `(read-hidden)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.SchemeString(), qt.Equals, "macro-only")

	_, ok := engine.Get("hidden")
	qt.Assert(t, ok, qt.IsFalse,
		qt.Commentf("macro-introduced binder must not be host-addressable by name"))
}

func TestEngineCall(t *testing.T) {
	t.Parallel()

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

// newHostCallEngine binds host-call to a RegisterPrimitive host that runs its
// argument through Engine.Call and forwards the error, and host-call-func to the
// same host written as a func-typed RegisterFunc callback, the control: both
// apply their procedure to 0.
func newHostCallEngine(t *testing.T) *wile.Engine {
	t.Helper()
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	err = engine.RegisterPrimitive(wile.PrimitiveSpec{
		Name:       "host-call",
		ParamCount: 1,
		Impl: func(mc wile.CallContext) error {
			v, callErr := engine.Call(mc.Context(), wile.WrapValue(mc.Arg(0)), wile.NewInteger(0))
			if callErr != nil {
				return callErr
			}
			mc.SetValue(v.Internal())
			return nil
		},
	})
	qt.Assert(t, err, qt.IsNil)
	err = engine.RegisterFunc("host-call-func", func(f func(int64) (int64, error)) (int64, error) {
		return f(0)
	})
	qt.Assert(t, err, qt.IsNil)
	return engine
}

// TestEngineCallIsNotAPrompt pins that a procedure a host runs through
// Engine.Call is not a prompt: a continuation the calling program captured,
// invoked inside it, is passed up to the program's driver when the host returns
// the error, so the rest of the program runs once and the callback's extents and
// the program's are both left. Before, the Engine.Call ran the rest of the
// program itself, the host returned, and it ran again. The func-typed callback
// answered this way already. Racket agrees (host-call as
// call-with-continuation-barrier: an escape out of a barrier is allowed).
func TestEngineCallIsNotAPrompt(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name  string
		code  string
		want  string
		hosts []string // nil: both hosts
	}{
		{
			name: "the rest of the program runs once",
			code: `(let ((log '()) (n 0) (k #f) (ends 0))
			         (set! log (cons (call/cc (lambda (c) (set! k c) 'first)) log))
			         (set! n (+ n 1))
			         (if (< n 2) (HOST (lambda (_) (k 'again))) #f)
			         (set! ends (+ ends 1))
			         (list (reverse log) ends))`,
			want: "((first again) 1)",
		},
		{
			name: "the callback's extents and the program's are left",
			code: `(let ((log '()) (n 0) (k #f))
			         (define (note x) (set! log (cons x log)))
			         (note (call/cc (lambda (c) (set! k c) 'first)))
			         (set! n (+ n 1))
			         (if (< n 2)
			             (dynamic-wind
			               (lambda () (note 'in))
			               (lambda ()
			                 (HOST (lambda (_)
			                         (dynamic-wind (lambda () (note 'cb-in))
			                                       (lambda () (k 'again))
			                                       (lambda () (note 'cb-out))))))
			               (lambda () (note 'out)))
			             #f)
			         (reverse log))`,
			want: "(first in cb-in cb-out out again)",
		},
		{
			name: "a guard around the host does not catch the jump",
			code: `(let ((log '()) (n 0) (k #f) (ends 0))
			         (set! log (cons (call/cc (lambda (c) (set! k c) 'first)) log))
			         (set! n (+ n 1))
			         (if (< n 2)
			             (guard (e (#t (set! log (cons 'caught log))))
			               (HOST (lambda (_) (k 'again))))
			             #f)
			         (set! ends (+ ends 1))
			         (list (reverse log) ends))`,
			want: "((first again) 1)",
		},
		{
			name: "an enclosing host call owns its own continuation",
			code: `(HOST (lambda (_)
			         (let ((n 0) (k #f) (ends 0))
			           (call/cc (lambda (c) (set! k c)))
			           (set! n (+ n 1))
			           (if (< n 2) (HOST (lambda (_) (k 'again))) #f)
			           (set! ends (+ ends 1))
			           ends)))`,
			want: "1",
		},
		{
			// The continuation's own boundary (eval's) is gone, but its root is
			// this call, so it is resumed here rather than passed up. The
			// func-typed host passes it up and the program's root drops the rest
			// of the program (the sub-context truncation class), so it is left out.
			name: "a continuation rooted in the call stays in it",
			code: `(define kbox (vector #f))
			       (list 'p (HOST (lambda (_)
			         (let ((v (eval '(call/cc (lambda (c) (vector-set! kbox 0 c) 0))
			                        (interaction-environment))))
			           (if (= v 0) ((vector-ref kbox 0) 5) v)))))`,
			want:  "(p 5)",
			hosts: []string{"host-call"},
		},
	}
	for _, tc := range tcs {
		hosts := tc.hosts
		if hosts == nil {
			hosts = []string{"host-call", "host-call-func"}
		}
		for _, host := range hosts {
			t.Run(tc.name+"/"+host, func(t *testing.T) {
				t.Parallel()
				ctx := context.Background()
				engine := newHostCallEngine(t)
				code := strings.ReplaceAll(tc.code, "HOST", host)
				got, err := engine.EvalMultiple(ctx, code)
				qt.Assert(t, err, qt.IsNil)
				qt.Assert(t, got.SchemeString(), qt.Equals, tc.want)
			})
		}
	}
}

// TestEngineCallDrivesAHostGenerator pins the host-driven generator shape: a
// continuation captured inside one Engine.Call and invoked from the next, whose
// owner's run is over, is resumed by the Engine.Call it reaches, from top-level
// Go and from a host nested in a running program alike. Racket refuses this
// shape (a continuation barrier per callback); Wile keeps it on purpose.
func TestEngineCallDrivesAHostGenerator(t *testing.T) {
	t.Parallel()

	ctx := context.Background()

	t.Run("from top-level Go", func(t *testing.T) {
		t.Parallel()
		engine := newHostCallEngine(t)
		_, err := engine.EvalMultiple(ctx, `
			(define saved (vector #f))
			(define (start _) (+ 1 (call/cc (lambda (c) (vector-set! saved 0 c) 1))))
			(define (resume _) ((vector-ref saved 0) 10))`)
		qt.Assert(t, err, qt.IsNil)
		start, ok := engine.Get("start")
		qt.Assert(t, ok, qt.IsTrue)
		resume, ok := engine.Get("resume")
		qt.Assert(t, ok, qt.IsTrue)

		got, err := engine.Call(ctx, start, wile.NewInteger(0))
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, got.SchemeString(), qt.Equals, "2")
		got, err = engine.Call(ctx, resume, wile.NewInteger(0))
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, got.SchemeString(), qt.Equals, "11")
	})

	t.Run("from a host inside a running program", func(t *testing.T) {
		t.Parallel()
		engine := newHostCallEngine(t)
		got, err := engine.EvalMultiple(ctx, `
			(let* ((saved #f)
			       (a (host-call (lambda (_) (+ 1 (call/cc (lambda (c) (set! saved c) 1))))))
			       (b (host-call (lambda (_) (saved 10)))))
			  (list a b))`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, got.SchemeString(), qt.Equals, "(2 11)")
	})
}

func TestEngineClose(t *testing.T) {
	t.Parallel()

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
	t.Parallel()

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
	t.Parallel()

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
	t.Parallel()

	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	expr, err := engine.ParseWithSource(ctx, "(+ 1 2)", "test-file.scm")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, expr, qt.IsNotNil)
	qt.Assert(t, expr.Source(), qt.Equals, "test-file.scm")
}

func TestEngineMustParse(t *testing.T) {
	t.Parallel()

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
	t.Parallel()

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
