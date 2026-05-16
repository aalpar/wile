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

// Package math_test contains shared test helpers and the test suite for the
// math extension. Tests are split across per-module files:
//
//   - prim_transcendental_test.go: exp, log, sin, cos, tan, asin, acos, atan, sqrt, expt, square
//   - prim_conversion_test.go:     number->string, string->number
//   - prim_complex_test.go:        make-rectangular, make-polar, real-part, imag-part, magnitude, angle
//   - prim_rational_test.go:       numerator, denominator, rationalize, exact-integer-sqrt
//   - prim_rounding_test.go:       floor, ceiling, truncate, round, integer division, numeric predicates
package math_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile"
	extmath "github.com/aalpar/wile/extensions/math"

	qt "github.com/frankban/quicktest"
)

// newEngine creates a Wile engine with the math extension loaded.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extmath.Extension),
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

// evalExpectError runs Scheme code, asserts that it produces an error,
// and returns the error. Callers that need to match the error against a
// sentinel (errors.Is) consume the return value; callers that only need
// "did it error" can ignore it. Parse errors are returned unwrapped
// (and count as expected errors for the assertion side).
func evalExpectError(t *testing.T, engine *wile.Engine, code string) error {
	t.Helper()
	expr, err := engine.Parse(context.Background(), code)
	if err != nil {
		return err // parse error counts as expected error
	}
	_, err = engine.Eval(context.Background(), expr)
	qt.New(t).Assert(err, qt.IsNotNil)
	return err
}
