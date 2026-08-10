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

package gointerop_test

import (
	"context"
	"testing"

	extgointerop "github.com/aalpar/wile/extensions/gointerop"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// newEngine creates a Wile engine with the gointerop extension loaded.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extgointerop.Extension),
	)
	qt.New(t).Assert(err, qt.IsNil)
	return engine
}

// eval runs Scheme code and returns the result.
func eval(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.EvalMultiple(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNil)
	return result
}

// evalExpectError runs Scheme code and asserts that it produces an error.
func evalExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	expr, err := engine.Parse(context.Background(), code)
	if err != nil {
		return // parse error counts as expected error
	}
	_, err = engine.Eval(context.Background(), expr)
	qt.New(t).Assert(err, qt.IsNotNil)
}

func TestAtomic(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// make-atomic and atomic?
		{"make-atomic", `(atomic? (make-atomic 0))`, values.TrueValue},
		{"atomic? false", `(atomic? 42)`, values.FalseValue},
		{"atomic? string", `(atomic? "hello")`, values.FalseValue},

		// atomic-load returns initial value
		{"load initial integer",
			`(= (atomic-load (make-atomic 42)) 42)`,
			values.TrueValue},
		{"load initial string",
			`(equal? (atomic-load (make-atomic "hello")) "hello")`,
			values.TrueValue},
		{"load initial boolean",
			`(atomic-load (make-atomic #t))`,
			values.TrueValue},

		// atomic-store! then load
		{"store then load",
			`(let ((a (make-atomic 0)))
			   (atomic-store! a 99)
			   (= (atomic-load a) 99))`,
			values.TrueValue},
		{"store string then load",
			`(let ((a (make-atomic "old")))
			   (atomic-store! a "new")
			   (equal? (atomic-load a) "new"))`,
			values.TrueValue},

		// atomic-swap! returns old value and stores new
		{"swap returns old",
			`(let ((a (make-atomic 42)))
			   (= (atomic-swap! a 99) 42))`,
			values.TrueValue},
		{"swap stores new",
			`(let ((a (make-atomic 42)))
			   (atomic-swap! a 99)
			   (= (atomic-load a) 99))`,
			values.TrueValue},

		// atomic-compare-and-swap! with pointer identity
		// CAS uses Go's atomic.Value.CompareAndSwap (pointer comparison),
		// so we must load the value first to get the same pointer.
		{"cas succeeds with loaded ref",
			`(let ((a (make-atomic 42)))
			   (let ((old (atomic-load a)))
			     (atomic-compare-and-swap! a old 99)))`,
			values.TrueValue},
		{"cas updates value",
			`(let ((a (make-atomic 42)))
			   (let ((old (atomic-load a)))
			     (atomic-compare-and-swap! a old 99)
			     (= (atomic-load a) 99)))`,
			values.TrueValue},

		// multiple store/load cycles
		{"multiple stores",
			`(let ((a (make-atomic 0)))
			   (atomic-store! a 1)
			   (atomic-store! a 2)
			   (atomic-store! a 3)
			   (= (atomic-load a) 3))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestGoInteropErrors(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
	}{
		// atomic type errors
		{"atomic-load not atomic", `(atomic-load 42)`},
		{"atomic-store! not atomic", `(atomic-store! 42 99)`},
		{"atomic-swap! not atomic", `(atomic-swap! 42 99)`},
		{"atomic-compare-and-swap! not atomic", `(atomic-compare-and-swap! 42 0 99)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}
