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
// enabled; the loss-signal tests need expt and make-rectangular to
// construct BigInteger / BigFloat / BigComplex test inputs from Scheme.
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

// TestRegisterFuncFloat64StrictModeLossless verifies that Float64 FFI
// parameters accept inputs that fit float64 exactly under the default
// strict mode. All inputs here are exactly representable; none should error.
func TestRegisterFuncFloat64StrictModeLossless(t *testing.T) {
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
		{"integer 42", `(take-float 42)`, "42.0"},
		{"integer negative", `(take-float -7)`, "-7.0"},
		{"float literal 0.5", `(take-float 0.5)`, "0.5"},
		{"float 3.0", `(take-float 3.0)`, "3.0"},
		{"rational 1/2 (power-of-2 denom)", `(take-float 1/2)`, "0.5"},
		// (expt 2 53) = 2^53, the largest exact integer in float64.
		{"power-of-2 boundary 2^53", `(take-float (expt 2 53))`, "9007199254740992.0"},
		// MinInt64 = -2^63, an exact power of 2 (one sign bit + leading
		// mantissa bit). float64 renders it via printer rounding to
		// nearest representable decimal.
		{"math.MinInt64 (exact power of 2)",
			`(take-float -9223372036854775808)`, "-9223372036854776000.0"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestRegisterFuncFloat64StrictModeLossy verifies that Float64 FFI
// parameters reject inputs that cannot fit float64 exactly under strict
// mode (the default). Pre-PR-2 these all succeeded silently; now they
// return ErrLossyConversion.
func TestRegisterFuncFloat64StrictModeLossy(t *testing.T) {
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
		// 1/3 is rational but not exactly representable in binary
		// float64 (denominator is not a power of 2).
		{"rational 1/3 (non-power-of-2 denom)", `(take-float 1/3)`},
		// (+ 1.0 (expt 10 60)) constructs a *BigFloat (mixing an
		// inexact unit with a 60-digit exact integer forces
		// arbitrary-precision arithmetic). Float64 can't hold all
		// 60 decimal digits; conversion rounds with non-Exact accuracy.
		{"big float lossy mantissa", `(take-float (+ 1.0 (expt 10 60)))`},
		// 2^100 + 1: BigInteger whose mantissa requires more than
		// 53 bits, so float64 can't preserve every digit.
		{"big integer precision loss",
			`(take-float (+ (expt 2 100) 1))`},
		// math.MaxInt64 = 2^63 - 1 is a BigInteger-fits-int64 value
		// that float64 rounds up to 2^63 (accuracy Above).
		{"int64 max value (rounds Above)",
			`(take-float 9223372036854775807)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err := evalExpectError(t, engine, tc.code)
			c.Assert(errors.Is(err, werr.ErrLossyConversion), qt.IsTrue,
				qt.Commentf("expected ErrLossyConversion, got: %v", err))
		})
	}
}

// TestRegisterFuncFloat64LossyAllowedMode verifies that the same lossy
// inputs succeed (with silently-rounded values) when
// WithLossyConversionsAllowed is set. Assertions check the exact
// IEEE-754-rounded result (rounding is deterministic).
func TestRegisterFuncFloat64LossyAllowedMode(t *testing.T) {
	c := qt.New(t)
	engine := newEngineLossy(t)

	err := engine.RegisterFunc("take-float", func(f float64) float64 {
		return f
	})
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name string
		code string
		want string // exact float64 SchemeString
	}{
		{"rational 1/3", `(take-float 1/3)`, "0.3333333333333333"},
		// (+ 1.0 (expt 10 60)) → ~1e60 rounded to nearest float64.
		{"big float lossy", `(take-float (+ 1.0 (expt 10 60)))`,
			"1000000000000000000000000000000000000000000000000000000000000.0"},
		// 2^100 + 1 → 2^100 (the +1 rounds away).
		{"big integer", `(take-float (+ (expt 2 100) 1))`,
			"1267650600228229400000000000000.0"},
		// MaxInt64 → 2^63.
		{"int64 max", `(take-float 9223372036854775807)`, "9223372036854776000.0"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestRegisterFuncFloat64EngineIsolation verifies the flag is per-engine:
// registering the same Go function on a strict engine and a lossy-allowed
// engine yields independent behaviors. The flag is captured at RegisterFunc
// time, so behavior is frozen per engine.
func TestRegisterFuncFloat64EngineIsolation(t *testing.T) {
	c := qt.New(t)
	strict := newEngineWithMath(t)
	lossy := newEngineLossy(t)

	fn := func(f float64) float64 { return f }
	c.Assert(strict.RegisterFunc("take-float", fn), qt.IsNil)
	c.Assert(lossy.RegisterFunc("take-float", fn), qt.IsNil)

	// Strict: 1/3 errors with ErrLossyConversion.
	errStrict := evalExpectError(t, strict, `(take-float 1/3)`)
	c.Assert(errors.Is(errStrict, werr.ErrLossyConversion), qt.IsTrue,
		qt.Commentf("strict engine: expected ErrLossyConversion, got: %v", errStrict))

	// Lossy-allowed: 1/3 succeeds with the IEEE-754-rounded result.
	result := eval(t, lossy, `(take-float 1/3)`)
	c.Assert(result.SchemeString(), qt.Equals, "0.3333333333333333")
}

// TestRegisterFuncFloat64FreezeAtRegistration verifies that the lossy
// flag is captured at RegisterFunc time, not consulted dynamically.
// After registration, additional RegisterFunc calls on the same engine
// continue to share the engine's flag state — each newly-registered
// function gets the same captured value.
func TestRegisterFuncFloat64FreezeAtRegistration(t *testing.T) {
	c := qt.New(t)

	strictEng := newEngineWithMath(t)
	fn := func(f float64) float64 { return f }
	c.Assert(strictEng.RegisterFunc("take-float", fn), qt.IsNil)
	c.Assert(strictEng.RegisterFunc("take-float-2", fn), qt.IsNil)
	c.Assert(strictEng.RegisterFunc("take-float-3", fn), qt.IsNil)

	// All three closures share the captured strict flag.
	for _, code := range []string{
		`(take-float 1/3)`,
		`(take-float-2 1/3)`,
		`(take-float-3 1/3)`,
	} {
		err := evalExpectError(t, strictEng, code)
		c.Assert(errors.Is(err, werr.ErrLossyConversion), qt.IsTrue,
			qt.Commentf("for %s, expected ErrLossyConversion, got: %v", code, err))
	}

	// Sanity: a separate lossy engine permits the same input.
	lossyEng := newEngineLossy(t)
	c.Assert(lossyEng.RegisterFunc("take-float", fn), qt.IsNil)
	result := eval(t, lossyEng, `(take-float 1/3)`)
	c.Assert(result.SchemeString(), qt.Equals, "0.3333333333333333")
}

// TestRegisterFuncFloat64SliceLossyAllowedPropagation verifies the
// lossyAllowed flag threads through composite-type builders. A slice
// of float64 with a lossy element succeeds under lossy-allowed mode
// and errors under strict — proving makeSliceArgConverter forwards
// the flag to its element converter.
func TestRegisterFuncFloat64SliceLossyAllowedPropagation(t *testing.T) {
	c := qt.New(t)

	fn := func(xs []float64) int64 { return int64(len(xs)) }

	strictEng := newEngineWithMath(t)
	c.Assert(strictEng.RegisterFunc("count-floats", fn), qt.IsNil)
	errStrict := evalExpectError(t, strictEng, `(count-floats '(0.5 1/3 1.0))`)
	c.Assert(errors.Is(errStrict, werr.ErrLossyConversion), qt.IsTrue,
		qt.Commentf("strict slice: expected ErrLossyConversion for lossy element, got: %v", errStrict))

	lossyEng := newEngineLossy(t)
	c.Assert(lossyEng.RegisterFunc("count-floats", fn), qt.IsNil)
	result := eval(t, lossyEng, `(count-floats '(0.5 1/3 1.0))`)
	c.Assert(result.SchemeString(), qt.Equals, "3")
}

// TestRegisterFuncComplex128StrictMode verifies the new Complex128 path:
// lossless inputs succeed, lossy ones error with ErrLossyConversion.
// The Go callback returns bool to sidestep the not-yet-supported
// complex128 *return* path.
func TestRegisterFuncComplex128StrictMode(t *testing.T) {
	c := qt.New(t)
	engine := newEngineWithMath(t)

	err := engine.RegisterFunc("complex-finite?", func(z complex128) bool {
		_ = z
		return true
	})
	c.Assert(err, qt.IsNil)

	losslessCases := []struct {
		name string
		code string
	}{
		{"integer 3", `(complex-finite? 3)`},
		{"float 0.5", `(complex-finite? 0.5)`},
		{"complex 3+4i", `(complex-finite? (make-rectangular 3 4))`},
	}
	for _, tc := range losslessCases {
		t.Run("lossless/"+tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, "#t")
		})
	}

	lossyCases := []struct {
		name string
		code string
	}{
		{"rational 1/3", `(complex-finite? 1/3)`},
		{"complex 1/3+0i", `(complex-finite? (make-rectangular 1/3 0))`},
		// BigComplex with a real-part magnitude that float64 cannot
		// preserve. (+ 1.0 (expt 10 60)) is a *BigFloat (~10^60),
		// representable in big.Float but lossy in float64.
		{"big complex (lossy real)",
			`(complex-finite? (make-rectangular (+ 1.0 (expt 10 60)) 0))`},
		// BigComplex with a lossy imaginary part (real-part exact).
		{"big complex (lossy imag)",
			`(complex-finite? (make-rectangular 0 (+ 1.0 (expt 10 60))))`},
	}
	for _, tc := range lossyCases {
		t.Run("lossy/"+tc.name, func(t *testing.T) {
			err := evalExpectError(t, engine, tc.code)
			c.Assert(errors.Is(err, werr.ErrLossyConversion), qt.IsTrue,
				qt.Commentf("expected ErrLossyConversion, got: %v", err))
		})
	}
}

// TestRegisterFuncComplex128LossyAllowed verifies the new Complex128
// path silently truncates under WithLossyConversionsAllowed.
func TestRegisterFuncComplex128LossyAllowed(t *testing.T) {
	c := qt.New(t)
	engine := newEngineLossy(t)

	err := engine.RegisterFunc("complex-finite?", func(z complex128) bool {
		_ = z
		return true
	})
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name string
		code string
	}{
		{"rational 1/3", `(complex-finite? 1/3)`},
		{"complex 1/3+0i", `(complex-finite? (make-rectangular 1/3 0))`},
		{"big complex (lossy real)",
			`(complex-finite? (make-rectangular (+ 1.0 (expt 10 60)) 0))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, "#t")
		})
	}
}
