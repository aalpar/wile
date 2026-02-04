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

package exceptions_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile"
	extexceptions "github.com/aalpar/wile/internal/extensions/exceptions"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// newEngine creates a Wile engine with core + exceptions extensions.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(
		wile.WithExtension(extexceptions.Extension),
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

func TestWithExceptionHandler(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("handler called on exception", func(t *testing.T) {
		// With continuable exceptions, handler can return
		result := eval(t, engine, `
			(let ((caught #f))
			  (with-exception-handler
			    (lambda (e) (set! caught e) 'default)
			    (lambda () (raise-continuable 'my-error)))
			  caught)
		`)
		c.Assert(result.Internal(), values.SchemeEquals, values.NewSymbol("my-error"))
	})

	t.Run("thunk completes without exception", func(t *testing.T) {
		result := eval(t, engine, `
			(with-exception-handler
			  (lambda (e) 'not-called)
			  (lambda () 42))
		`)
		c.Assert(result.Internal(), values.SchemeEquals, values.NewInteger(42))
	})

	t.Run("nested handlers with continuable", func(t *testing.T) {
		result := eval(t, engine, `
			(define log '())
			(with-exception-handler
			  (lambda (e) (set! log (cons 'outer log)) 'outer-result)
			  (lambda ()
			    (with-exception-handler
			      (lambda (e) (set! log (cons 'inner log)) 'inner-result)
			      (lambda () (raise-continuable 'error)))))
			log
		`)
		c.Assert(result.Internal(), qt.IsNotNil)
	})

	t.Run("wrong thunk type", func(t *testing.T) {
		evalExpectError(t, engine, `
			(with-exception-handler (lambda (e) e) 42)
		`)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(with-exception-handler (lambda (e) e))`)
	})
}

func TestRaise(t *testing.T) {
	engine := newEngine(t)

	t.Run("raise without handler propagates", func(t *testing.T) {
		evalExpectError(t, engine, `(raise 'error)`)
	})

	t.Run("raise with error propagates", func(t *testing.T) {
		evalExpectError(t, engine, `(error "test error")`)
	})

	t.Run("raise is non-continuable", func(t *testing.T) {
		// Handler that tries to return from non-continuable exception should error
		evalExpectError(t, engine, `
			(with-exception-handler
			  (lambda (e) 'handled)
			  (lambda () (raise 'boom)))
		`)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(raise)`)
	})
}

func TestRaiseContinuable(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("raise continuable allows handler to return", func(t *testing.T) {
		result := eval(t, engine, `
			(with-exception-handler
			  (lambda (e) 100)
			  (lambda () (+ 1 (raise-continuable 'ignored))))
		`)
		c.Assert(result.Internal(), values.SchemeEquals, values.NewInteger(101))
	})

	t.Run("raise continuable with symbol", func(t *testing.T) {
		result := eval(t, engine, `
			(with-exception-handler
			  (lambda (e) 42)
			  (lambda () (raise-continuable 'warning)))
		`)
		c.Assert(result.Internal(), values.SchemeEquals, values.NewInteger(42))
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(raise-continuable)`)
	})
}

func TestError(t *testing.T) {
	engine := newEngine(t)

	t.Run("error raises exception", func(t *testing.T) {
		evalExpectError(t, engine, `(error "test")`)
	})

	t.Run("error with irritants raises exception", func(t *testing.T) {
		evalExpectError(t, engine, `(error "failed" 1 2 3)`)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(error)`)
	})

	t.Run("wrong message type", func(t *testing.T) {
		evalExpectError(t, engine, `(error 42)`)
	})
}

func TestErrorObjectQ(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("non-error returns false", func(t *testing.T) {
		result := eval(t, engine, `(error-object? 42)`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})

	t.Run("string returns false", func(t *testing.T) {
		result := eval(t, engine, `(error-object? "not an error")`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})

	t.Run("symbol returns false", func(t *testing.T) {
		result := eval(t, engine, `(error-object? 'err)`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(error-object?)`)
	})
}

func TestErrorObjectMessage(t *testing.T) {
	engine := newEngine(t)

	t.Run("wrong argument type", func(t *testing.T) {
		evalExpectError(t, engine, `(error-object-message 42)`)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(error-object-message)`)
	})
}

func TestErrorObjectIrritants(t *testing.T) {
	engine := newEngine(t)

	t.Run("wrong argument type", func(t *testing.T) {
		evalExpectError(t, engine, `(error-object-irritants 42)`)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(error-object-irritants)`)
	})
}

func TestReadErrorQ(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("symbol is not read-error", func(t *testing.T) {
		result := eval(t, engine, `
			(let ((caught-error #f))
			  (with-exception-handler
			    (lambda (e) (set! caught-error e) 'handled)
			    (lambda () (raise-continuable 'my-symbol)))
			  (read-error? caught-error))
		`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})

	t.Run("non-error returns false", func(t *testing.T) {
		result := eval(t, engine, `(read-error? 42)`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(read-error?)`)
	})
}

func TestFileErrorQ(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("symbol is not file-error", func(t *testing.T) {
		result := eval(t, engine, `
			(let ((caught-error #f))
			  (with-exception-handler
			    (lambda (e) (set! caught-error e) 'handled)
			    (lambda () (raise-continuable 'my-symbol)))
			  (file-error? caught-error))
		`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})

	t.Run("non-error returns false", func(t *testing.T) {
		result := eval(t, engine, `(file-error? 42)`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(file-error?)`)
	})
}

func TestExceptionHandlerWithDynamicWind(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("dynamic-wind before/after with exception", func(t *testing.T) {
		result := eval(t, engine, `
			(define log '())
			(define result
			  (with-exception-handler
			    (lambda (e) 'caught)
			    (lambda ()
			      (dynamic-wind
			        (lambda () (set! log (cons 'before log)))
			        (lambda () (raise 'error))
			        (lambda () (set! log (cons 'after log)))))))
			(list result log)
		`)
		c.Assert(result.Internal(), qt.IsNotNil)
	})
}
