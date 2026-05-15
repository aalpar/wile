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
	extmath "github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/werr"

	qt "github.com/frankban/quicktest"
)

// --- PR 2 — numeric loss signals: FFI Float64 + Complex128 tests ---

// newEngineWithMath constructs a default engine with the math extension
// enabled; the loss-signal tests need expt and make-rectangular.
func newEngineWithMath(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extmath.Extension))
	if err != nil {
		t.Fatal(err)
	}
	return engine
}

// newEngineLossy constructs an engine with WithLossyConversionsAllowed
// and the math extension enabled.
func newEngineLossy(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithLossyConversionsAllowed(),
		wile.WithExtension(extmath.Extension))
	if err != nil {
		t.Fatal(err)
	}
	return engine
}

// runProgram parses and runs the given Scheme expression and returns the
// resulting value (or fatal if it errors). For tests that expect an error,
// see runProgramExpectError.
func runProgram(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	ctx := context.Background()
	result, err := engine.Eval(ctx, engine.MustParse(ctx, code))
	if err != nil {
		t.Fatalf("Eval %q: %v", code, err)
	}
	return result
}

// runProgramExpectError parses and runs the given Scheme expression and
// returns the error (or fatal if it succeeds).
func runProgramExpectError(t *testing.T, engine *wile.Engine, code string) error {
	t.Helper()
	ctx := context.Background()
	_, err := engine.Eval(ctx, engine.MustParse(ctx, code))
	if err == nil {
		t.Fatalf("Eval %q: expected error, got nil", code)
	}
	return err
}

// TestFFIFloat64StrictModeLossless verifies that Float64 FFI parameters
// accept inputs that fit float64 exactly under the default strict mode.
// All inputs here are exactly representable; none should error.
func TestFFIFloat64StrictModeLossless(t *testing.T) {
	c := qt.New(t)
	engine := newEngineWithMath(t)

	err := engine.RegisterFunc("take-float", func(f float64) float64 {
		return f
	})
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name string
		code string
		want string
	}{
		// All inputs are exactly representable in float64.
		{"integer 42", `(take-float 42)`, "42.0"},
		{"integer negative", `(take-float -7)`, "-7.0"},
		{"float literal 0.5", `(take-float 0.5)`, "0.5"},
		{"float 3.0", `(take-float 3.0)`, "3.0"},
		{"rational 1/2", `(take-float 1/2)`, "0.5"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := runProgram(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestFFIFloat64StrictModeLossy verifies that Float64 FFI parameters reject
// inputs that cannot fit float64 exactly under strict mode (the default).
// Pre-PR-2 these all succeeded silently; now they return ErrLossyConversion.
func TestFFIFloat64StrictModeLossy(t *testing.T) {
	c := qt.New(t)
	engine := newEngineWithMath(t)

	err := engine.RegisterFunc("take-float", func(f float64) float64 {
		return f
	})
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name string
		code string
	}{
		// 1/3 cannot be expressed in float64 (Below).
		{"rational 1/3", `(take-float 1/3)`},
		// (+ 1.0 (expt 10 60)) is a *BigFloat (mixing an inexact unit
		// with a 60-digit exact integer forces arbitrary-precision
		// arithmetic). Float64 can't hold all 60 decimal digits;
		// conversion rounds with non-Exact accuracy.
		{"big float lossy mantissa", `(take-float (+ 1.0 (expt 10 60)))`},
		// 2^100 + 1 is a BigInteger that float64 cannot preserve exactly.
		{"big integer precision loss",
			`(take-float (+ (expt 2 100) 1))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err := runProgramExpectError(t, engine, tc.code)
			c.Assert(errors.Is(err, werr.ErrLossyConversion), qt.IsTrue,
				qt.Commentf("expected ErrLossyConversion, got: %v", err))
		})
	}
}

// TestFFIFloat64LossyAllowedMode verifies that the same lossy inputs succeed
// (with silently-rounded values) when WithLossyConversionsAllowed is set.
func TestFFIFloat64LossyAllowedMode(t *testing.T) {
	c := qt.New(t)
	engine := newEngineLossy(t)

	err := engine.RegisterFunc("take-float", func(f float64) float64 {
		return f
	})
	c.Assert(err, qt.IsNil)

	// The exact result values are float64-rounded approximations of the
	// inputs. Asserting "not nil + numeric" suffices for the behavior
	// contract (silently succeeded vs. errored).
	tcs := []string{
		`(take-float 1/3)`,
		`(take-float (+ 1.0 (expt 10 60)))`,
		`(take-float (+ (expt 2 100) 1))`,
	}
	for _, code := range tcs {
		t.Run(code, func(t *testing.T) {
			result := runProgram(t, engine, code)
			c.Assert(result, qt.IsNotNil)
		})
	}
}

// TestFFIEngineIsolation verifies the flag is per-engine: registering the
// same Go function on a strict engine and a lossy-allowed engine yields
// independent behaviors. The flag is captured at RegisterFunc time, so
// behavior is frozen per engine.
func TestFFIEngineIsolation(t *testing.T) {
	c := qt.New(t)
	strict := newEngineWithMath(t)
	lossy := newEngineLossy(t)

	fn := func(f float64) float64 { return f }
	c.Assert(strict.RegisterFunc("take-float", fn), qt.IsNil)
	c.Assert(lossy.RegisterFunc("take-float", fn), qt.IsNil)

	// Strict: 1/3 errors with ErrLossyConversion.
	errStrict := runProgramExpectError(t, strict, `(take-float 1/3)`)
	c.Assert(errors.Is(errStrict, werr.ErrLossyConversion), qt.IsTrue,
		qt.Commentf("strict engine: expected ErrLossyConversion, got: %v", errStrict))

	// Lossy-allowed: 1/3 succeeds.
	result := runProgram(t, lossy, `(take-float 1/3)`)
	c.Assert(result, qt.IsNotNil)
}

// TestFFIComplex128StrictMode verifies the new Complex128 path: lossless
// inputs succeed, lossy ones error with ErrLossyConversion. The Go callback
// returns bool to sidestep the not-yet-supported complex128 *return* path.
func TestFFIComplex128StrictMode(t *testing.T) {
	c := qt.New(t)
	engine := newEngineWithMath(t)

	err := engine.RegisterFunc("complex-finite?", func(c complex128) bool {
		_ = c
		return true
	})
	c.Assert(err, qt.IsNil)

	// Lossless inputs.
	for _, code := range []string{
		`(complex-finite? 3)`,
		`(complex-finite? 0.5)`,
		`(complex-finite? (make-rectangular 3 4))`,
	} {
		t.Run("ok-"+code, func(t *testing.T) {
			result := runProgram(t, engine, code)
			c.Assert(result.SchemeString(), qt.Equals, "#t")
		})
	}

	// Lossy inputs error in strict mode.
	for _, code := range []string{
		`(complex-finite? 1/3)`,
		`(complex-finite? (make-rectangular 1/3 0))`,
	} {
		t.Run("lossy-"+code, func(t *testing.T) {
			err := runProgramExpectError(t, engine, code)
			c.Assert(errors.Is(err, werr.ErrLossyConversion), qt.IsTrue,
				qt.Commentf("expected ErrLossyConversion, got: %v", err))
		})
	}
}

// TestFFIComplex128LossyAllowed verifies the new Complex128 path silently
// truncates under WithLossyConversionsAllowed.
func TestFFIComplex128LossyAllowed(t *testing.T) {
	c := qt.New(t)
	engine := newEngineLossy(t)

	err := engine.RegisterFunc("complex-finite?", func(c complex128) bool {
		_ = c
		return true
	})
	c.Assert(err, qt.IsNil)

	for _, code := range []string{
		`(complex-finite? 1/3)`,
		`(complex-finite? (make-rectangular 1/3 0))`,
	} {
		t.Run(code, func(t *testing.T) {
			result := runProgram(t, engine, code)
			c.Assert(result.SchemeString(), qt.Equals, "#t")
		})
	}
}
