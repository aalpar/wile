// Copyright 2025 Aaron Alpar
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

// prim_trig_test.go tests transcendental functions: exp, log, sin, cos, tan,
// asin, acos, atan.
//
// IMPLEMENTATION NOTES:
// - All transcendental functions accept complex inputs per R7RS
// - Branch cut conventions follow Go's math/cmplx package (implementation-defined per R7RS)
// - Special value handling (Inf/NaN) follows Go's conventions
// - See CLAUDE.md "Transcendental Functions - Implementation Details" for full documentation

package core_test

import (
	"math"
	"testing"

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func withinTolerance(t *testing.T, result, expected values.Value, tolerance float64) bool {
	t.Helper()
	resultFloat, ok1 := result.(*values.Float)
	expectedFloat, ok2 := expected.(*values.Float)
	if !ok1 || !ok2 {
		return false
	}
	diff := math.Abs(resultFloat.Value - expectedFloat.Value)
	return diff < tolerance
}

func TestSin(t *testing.T) {
	tcs := []struct {
		name      string
		prog      values.Value
		out       values.Value
		tolerance float64
	}{
		{
			name: "sin(0) = 0",
			prog: values.List(values.NewSymbol("sin"), values.NewInteger(0)),
			out:  values.NewFloat(0.0),
		},
		{
			name:      "sin(π/2) ≈ 1",
			prog:      values.List(values.NewSymbol("sin"), values.NewFloat(math.Pi/2)),
			out:       values.NewFloat(1.0),
			tolerance: 0.0001,
		},
		{
			name:      "sin(π) ≈ 0",
			prog:      values.List(values.NewSymbol("sin"), values.NewFloat(math.Pi)),
			out:       values.NewFloat(0.0),
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			if tc.tolerance > 0 {
				qt.Assert(t, withinTolerance(t, result, tc.out, tc.tolerance), qt.IsTrue)
			} else {
				qt.Assert(t, result, values.SchemeEquals, tc.out)
			}
		})
	}
}

func TestCos(t *testing.T) {
	tcs := []struct {
		name      string
		prog      values.Value
		out       values.Value
		tolerance float64
	}{
		{
			name: "cos(0) = 1",
			prog: values.List(values.NewSymbol("cos"), values.NewInteger(0)),
			out:  values.NewFloat(1.0),
		},
		{
			name:      "cos(π) ≈ -1",
			prog:      values.List(values.NewSymbol("cos"), values.NewFloat(math.Pi)),
			out:       values.NewFloat(-1.0),
			tolerance: 0.0001,
		},
		{
			name:      "cos(π/2) ≈ 0",
			prog:      values.List(values.NewSymbol("cos"), values.NewFloat(math.Pi/2)),
			out:       values.NewFloat(0.0),
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			if tc.tolerance > 0 {
				qt.Assert(t, withinTolerance(t, result, tc.out, tc.tolerance), qt.IsTrue)
			} else {
				qt.Assert(t, result, values.SchemeEquals, tc.out)
			}
		})
	}
}

func TestTan(t *testing.T) {
	tcs := []struct {
		name      string
		prog      values.Value
		out       values.Value
		tolerance float64
	}{
		{
			name: "tan(0) = 0",
			prog: values.List(values.NewSymbol("tan"), values.NewInteger(0)),
			out:  values.NewFloat(0.0),
		},
		{
			name:      "tan(π/4) ≈ 1",
			prog:      values.List(values.NewSymbol("tan"), values.NewFloat(math.Pi/4)),
			out:       values.NewFloat(1.0),
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			if tc.tolerance > 0 {
				qt.Assert(t, withinTolerance(t, result, tc.out, tc.tolerance), qt.IsTrue)
			} else {
				qt.Assert(t, result, values.SchemeEquals, tc.out)
			}
		})
	}
}

func TestAsin(t *testing.T) {
	tcs := []struct {
		name      string
		prog      values.Value
		out       values.Value
		tolerance float64
	}{
		{
			name: "asin(0) = 0",
			prog: values.List(values.NewSymbol("asin"), values.NewInteger(0)),
			out:  values.NewFloat(0.0),
		},
		{
			name:      "asin(1) ≈ π/2",
			prog:      values.List(values.NewSymbol("asin"), values.NewInteger(1)),
			out:       values.NewFloat(math.Pi / 2),
			tolerance: 0.0001,
		},
		{
			name:      "asin(-1) ≈ -π/2",
			prog:      values.List(values.NewSymbol("asin"), values.NewInteger(-1)),
			out:       values.NewFloat(-math.Pi / 2),
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			if tc.tolerance > 0 {
				qt.Assert(t, withinTolerance(t, result, tc.out, tc.tolerance), qt.IsTrue)
			} else {
				qt.Assert(t, result, values.SchemeEquals, tc.out)
			}
		})
	}
}

func TestAcos(t *testing.T) {
	tcs := []struct {
		name      string
		prog      values.Value
		out       values.Value
		tolerance float64
	}{
		{
			name: "acos(1) = 0",
			prog: values.List(values.NewSymbol("acos"), values.NewInteger(1)),
			out:  values.NewFloat(0.0),
		},
		{
			name:      "acos(0) ≈ π/2",
			prog:      values.List(values.NewSymbol("acos"), values.NewInteger(0)),
			out:       values.NewFloat(math.Pi / 2),
			tolerance: 0.0001,
		},
		{
			name:      "acos(-1) ≈ π",
			prog:      values.List(values.NewSymbol("acos"), values.NewInteger(-1)),
			out:       values.NewFloat(math.Pi),
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			if tc.tolerance > 0 {
				qt.Assert(t, withinTolerance(t, result, tc.out, tc.tolerance), qt.IsTrue)
			} else {
				qt.Assert(t, result, values.SchemeEquals, tc.out)
			}
		})
	}
}

func TestAtan(t *testing.T) {
	tcs := []struct {
		name      string
		prog      values.Value
		out       values.Value
		tolerance float64
	}{
		{
			name: "atan(0) = 0",
			prog: values.List(values.NewSymbol("atan"), values.NewInteger(0)),
			out:  values.NewFloat(0.0),
		},
		{
			name:      "atan(1) ≈ π/4",
			prog:      values.List(values.NewSymbol("atan"), values.NewInteger(1)),
			out:       values.NewFloat(math.Pi / 4),
			tolerance: 0.0001,
		},
		{
			name:      "atan(1, 1) ≈ π/4",
			prog:      values.List(values.NewSymbol("atan"), values.NewInteger(1), values.NewInteger(1)),
			out:       values.NewFloat(math.Pi / 4),
			tolerance: 0.0001,
		},
		{
			name:      "atan(1, 0) ≈ π/2",
			prog:      values.List(values.NewSymbol("atan"), values.NewInteger(1), values.NewInteger(0)),
			out:       values.NewFloat(math.Pi / 2),
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			if tc.tolerance > 0 {
				qt.Assert(t, withinTolerance(t, result, tc.out, tc.tolerance), qt.IsTrue)
			} else {
				qt.Assert(t, result, values.SchemeEquals, tc.out)
			}
		})
	}
}

func TestExp(t *testing.T) {
	tcs := []struct {
		name      string
		prog      values.Value
		out       values.Value
		tolerance float64
	}{
		{
			name: "exp(0) = 1",
			prog: values.List(values.NewSymbol("exp"), values.NewInteger(0)),
			out:  values.NewFloat(1.0),
		},
		{
			name:      "exp(1) ≈ e",
			prog:      values.List(values.NewSymbol("exp"), values.NewInteger(1)),
			out:       values.NewFloat(math.E),
			tolerance: 0.0001,
		},
		{
			name:      "exp(2) ≈ e^2",
			prog:      values.List(values.NewSymbol("exp"), values.NewInteger(2)),
			out:       values.NewFloat(math.E * math.E),
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			if tc.tolerance > 0 {
				qt.Assert(t, withinTolerance(t, result, tc.out, tc.tolerance), qt.IsTrue)
			} else {
				qt.Assert(t, result, values.SchemeEquals, tc.out)
			}
		})
	}
}

func TestLog(t *testing.T) {
	tcs := []struct {
		name      string
		prog      values.Value
		out       values.Value
		tolerance float64
	}{
		{
			name: "log(1) = 0",
			prog: values.List(values.NewSymbol("log"), values.NewInteger(1)),
			out:  values.NewFloat(0.0),
		},
		{
			name:      "log(e) ≈ 1",
			prog:      values.List(values.NewSymbol("log"), values.NewFloat(math.E)),
			out:       values.NewFloat(1.0),
			tolerance: 0.0001,
		},
		{
			name: "log(8, 2) = 3",
			prog: values.List(values.NewSymbol("log"), values.NewInteger(8), values.NewInteger(2)),
			out:  values.NewFloat(3.0),
		},
		{
			name:      "log(100, 10) = 2",
			prog:      values.List(values.NewSymbol("log"), values.NewInteger(100), values.NewInteger(10)),
			out:       values.NewFloat(2.0),
			tolerance: 0.0001,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			if tc.tolerance > 0 {
				qt.Assert(t, withinTolerance(t, result, tc.out, tc.tolerance), qt.IsTrue)
			} else {
				qt.Assert(t, result, values.SchemeEquals, tc.out)
			}
		})
	}
}

func TestTrigWithRationals(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "sin with rational",
			code: `(sin 1/2)`,
		},
		{
			name: "cos with rational",
			code: `(cos 1/2)`,
		},
		{
			name: "tan with rational",
			code: `(tan 1/4)`,
		},
		{
			name: "asin with rational",
			code: `(asin 1/2)`,
		},
		{
			name: "acos with rational",
			code: `(acos 1/2)`,
		},
		{
			name: "atan with rational",
			code: `(atan 1/2)`,
		},
		{
			name: "atan2 with rationals",
			code: `(atan 1/2 3/4)`,
		},
		{
			name: "log with rational",
			code: `(log 1/2)`,
		},
		{
			name: "log with rational base",
			code: `(log 8 2)`,
		},
		{
			name: "exp with rational",
			code: `(exp 1/2)`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
		})
	}
}

// ============================================================================
// Extended Transcendental Function Tests (Phase 6)
// ============================================================================

// Helper for checking float results with tolerance using Scheme code
func assertFloatResult(t *testing.T, code string, expected float64, tolerance float64) {
	t.Helper()
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	resultFloat, ok := result.(*values.Float)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected Float, got %T", result))
	diff := math.Abs(resultFloat.Value - expected)
	qt.Assert(t, diff < tolerance, qt.IsTrue,
		qt.Commentf("expected %v, got %v (diff: %v)", expected, resultFloat.Value, diff))
}

// Helper for checking complex results with tolerance
func assertComplexResult(t *testing.T, code string, expectedReal, expectedImag, tolerance float64) {
	t.Helper()
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	resultComplex, ok := result.(*values.Complex)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected Complex, got %T", result))
	realDiff := math.Abs(real(resultComplex.Value) - expectedReal)
	imagDiff := math.Abs(imag(resultComplex.Value) - expectedImag)
	qt.Assert(t, realDiff < tolerance, qt.IsTrue,
		qt.Commentf("real part: expected %v, got %v (diff: %v)", expectedReal, real(resultComplex.Value), realDiff))
	qt.Assert(t, imagDiff < tolerance, qt.IsTrue,
		qt.Commentf("imag part: expected %v, got %v (diff: %v)", expectedImag, imag(resultComplex.Value), imagDiff))
}

// TestExpExtended tests exp with various numeric types and edge cases
func TestExpExtended(t *testing.T) {
	t.Run("integer inputs", func(t *testing.T) {
		assertFloatResult(t, `(exp 0)`, 1.0, 1e-10)
		assertFloatResult(t, `(exp 1)`, math.E, 1e-10)
		assertFloatResult(t, `(exp 2)`, math.E*math.E, 1e-10)
		assertFloatResult(t, `(exp -1)`, 1/math.E, 1e-10)
		assertFloatResult(t, `(exp -2)`, 1/(math.E*math.E), 1e-10)
	})

	t.Run("float inputs", func(t *testing.T) {
		assertFloatResult(t, `(exp 0.0)`, 1.0, 1e-10)
		assertFloatResult(t, `(exp 0.5)`, math.Exp(0.5), 1e-10)
		assertFloatResult(t, `(exp -0.5)`, math.Exp(-0.5), 1e-10)
		assertFloatResult(t, `(exp 1.5)`, math.Exp(1.5), 1e-10)
	})

	t.Run("rational inputs", func(t *testing.T) {
		assertFloatResult(t, `(exp 1/2)`, math.Exp(0.5), 1e-10)
		assertFloatResult(t, `(exp -1/2)`, math.Exp(-0.5), 1e-10)
		assertFloatResult(t, `(exp 3/2)`, math.Exp(1.5), 1e-10)
	})

	t.Run("special values", func(t *testing.T) {
		// exp(+inf) = +inf
		result, err := runSchemeCode(t, `(exp +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		resultFloat, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(resultFloat.Value, 1), qt.IsTrue)

		// exp(-inf) = 0
		assertFloatResult(t, `(exp -inf.0)`, 0.0, 1e-10)

		// exp(nan) = nan
		result, err = runSchemeCode(t, `(exp +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		resultFloat, ok = result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(resultFloat.Value), qt.IsTrue)
	})
}

// TestExpErrors tests error conditions for exp
func TestExpErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "exp string", code: `(exp "hello")`},
		{name: "exp symbol", code: `(exp 'foo)`},
		{name: "exp list", code: `(exp '(1 2 3))`},
		{name: "exp boolean", code: `(exp #t)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestLogExtended tests log with various numeric types and edge cases
func TestLogExtended(t *testing.T) {
	t.Run("natural log integer inputs", func(t *testing.T) {
		assertFloatResult(t, `(log 1)`, 0.0, 1e-10)
		assertFloatResult(t, `(log 2)`, math.Log(2), 1e-10)
		assertFloatResult(t, `(log 10)`, math.Log(10), 1e-10)
	})

	t.Run("natural log float inputs", func(t *testing.T) {
		assertFloatResult(t, `(log 1.0)`, 0.0, 1e-10)
		assertFloatResult(t, `(log 2.718281828459045)`, 1.0, 1e-10)
		assertFloatResult(t, `(log 0.5)`, math.Log(0.5), 1e-10)
	})

	t.Run("natural log rational inputs", func(t *testing.T) {
		assertFloatResult(t, `(log 1/2)`, math.Log(0.5), 1e-10)
		assertFloatResult(t, `(log 2/1)`, math.Log(2), 1e-10)
		assertFloatResult(t, `(log 3/4)`, math.Log(0.75), 1e-10)
	})

	t.Run("log with base", func(t *testing.T) {
		assertFloatResult(t, `(log 8 2)`, 3.0, 1e-10)
		assertFloatResult(t, `(log 100 10)`, 2.0, 1e-10)
		assertFloatResult(t, `(log 27 3)`, 3.0, 1e-10)
		assertFloatResult(t, `(log 16 4)`, 2.0, 1e-10)
		assertFloatResult(t, `(log 1 10)`, 0.0, 1e-10) // log_b(1) = 0 for any base
	})

	t.Run("log with float base", func(t *testing.T) {
		assertFloatResult(t, `(log 8.0 2.0)`, 3.0, 1e-10)
		assertFloatResult(t, `(log 2.718281828459045 2.718281828459045)`, 1.0, 1e-10)
	})

	t.Run("log with rational base", func(t *testing.T) {
		assertFloatResult(t, `(log 4 2)`, 2.0, 1e-10)
		assertFloatResult(t, `(log 1/4 1/2)`, 2.0, 1e-10) // log_{1/2}(1/4) = 2
	})

	t.Run("negative numbers return Complex (R7RS)", func(t *testing.T) {
		// log(-1) = πi
		assertComplexResult(t, `(log -1)`, 0, math.Pi, 1e-10)
		// log(-e) ≈ 1 + πi
		assertComplexResult(t, `(log -2.718281828459045)`, 1.0, math.Pi, 1e-10)
		// log(-2) ≈ 0.693... + πi
		assertComplexResult(t, `(log -2)`, math.Log(2), math.Pi, 1e-10)
	})

	t.Run("special values", func(t *testing.T) {
		// log(+inf) = +inf
		result, err := runSchemeCode(t, `(log +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		resultFloat, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(resultFloat.Value, 1), qt.IsTrue)

		// log(0) = -inf
		result, err = runSchemeCode(t, `(log 0)`)
		qt.Assert(t, err, qt.IsNil)
		resultFloat, ok = result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(resultFloat.Value, -1), qt.IsTrue)

		// log(nan) = nan
		result, err = runSchemeCode(t, `(log +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		resultFloat, ok = result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(resultFloat.Value), qt.IsTrue)
	})
}

// TestLogErrors tests error conditions for log
func TestLogErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "log string", code: `(log "hello")`},
		{name: "log symbol", code: `(log 'foo)`},
		{name: "log list", code: `(log '(1 2 3))`},
		{name: "log boolean", code: `(log #t)`},
		{name: "log base string", code: `(log 10 "two")`},
		{name: "log base symbol", code: `(log 10 'two)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestSinExtended tests sin with various numeric types and edge cases
func TestSinExtended(t *testing.T) {
	t.Run("integer inputs", func(t *testing.T) {
		assertFloatResult(t, `(sin 0)`, 0.0, 1e-10)
		assertFloatResult(t, `(sin 1)`, math.Sin(1), 1e-10)
		assertFloatResult(t, `(sin -1)`, math.Sin(-1), 1e-10)
	})

	t.Run("float inputs", func(t *testing.T) {
		assertFloatResult(t, `(sin 0.0)`, 0.0, 1e-10)
		// sin(π/2) = 1
		assertFloatResult(t, `(sin 1.5707963267948966)`, 1.0, 1e-10)
		// sin(π) ≈ 0
		assertFloatResult(t, `(sin 3.141592653589793)`, 0.0, 1e-10)
		// sin(3π/2) = -1
		assertFloatResult(t, `(sin 4.71238898038469)`, -1.0, 1e-10)
	})

	t.Run("rational inputs", func(t *testing.T) {
		assertFloatResult(t, `(sin 1/2)`, math.Sin(0.5), 1e-10)
		assertFloatResult(t, `(sin -1/2)`, math.Sin(-0.5), 1e-10)
		assertFloatResult(t, `(sin 3/2)`, math.Sin(1.5), 1e-10)
	})

	t.Run("special values", func(t *testing.T) {
		// sin(+inf) = nan
		result, err := runSchemeCode(t, `(sin +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		resultFloat, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(resultFloat.Value), qt.IsTrue)

		// sin(-inf) = nan
		result, err = runSchemeCode(t, `(sin -inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		resultFloat, ok = result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(resultFloat.Value), qt.IsTrue)

		// sin(nan) = nan
		result, err = runSchemeCode(t, `(sin +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		resultFloat, ok = result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(resultFloat.Value), qt.IsTrue)
	})
}

// TestSinErrors tests error conditions for sin
func TestSinErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "sin string", code: `(sin "hello")`},
		{name: "sin symbol", code: `(sin 'foo)`},
		{name: "sin list", code: `(sin '(1 2 3))`},
		{name: "sin boolean", code: `(sin #t)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestCosExtended tests cos with various numeric types and edge cases
func TestCosExtended(t *testing.T) {
	t.Run("integer inputs", func(t *testing.T) {
		assertFloatResult(t, `(cos 0)`, 1.0, 1e-10)
		assertFloatResult(t, `(cos 1)`, math.Cos(1), 1e-10)
		assertFloatResult(t, `(cos -1)`, math.Cos(-1), 1e-10)
	})

	t.Run("float inputs", func(t *testing.T) {
		assertFloatResult(t, `(cos 0.0)`, 1.0, 1e-10)
		// cos(π/2) ≈ 0
		assertFloatResult(t, `(cos 1.5707963267948966)`, 0.0, 1e-10)
		// cos(π) = -1
		assertFloatResult(t, `(cos 3.141592653589793)`, -1.0, 1e-10)
		// cos(2π) = 1
		assertFloatResult(t, `(cos 6.283185307179586)`, 1.0, 1e-10)
	})

	t.Run("rational inputs", func(t *testing.T) {
		assertFloatResult(t, `(cos 1/2)`, math.Cos(0.5), 1e-10)
		assertFloatResult(t, `(cos -1/2)`, math.Cos(-0.5), 1e-10)
		assertFloatResult(t, `(cos 3/2)`, math.Cos(1.5), 1e-10)
	})

	t.Run("special values", func(t *testing.T) {
		// cos(+inf) = nan
		result, err := runSchemeCode(t, `(cos +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		resultFloat, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(resultFloat.Value), qt.IsTrue)

		// cos(-inf) = nan
		result, err = runSchemeCode(t, `(cos -inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		resultFloat, ok = result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(resultFloat.Value), qt.IsTrue)

		// cos(nan) = nan
		result, err = runSchemeCode(t, `(cos +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		resultFloat, ok = result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(resultFloat.Value), qt.IsTrue)
	})
}

// TestCosErrors tests error conditions for cos
func TestCosErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "cos string", code: `(cos "hello")`},
		{name: "cos symbol", code: `(cos 'foo)`},
		{name: "cos list", code: `(cos '(1 2 3))`},
		{name: "cos boolean", code: `(cos #t)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestTanExtended tests tan with various numeric types and edge cases
func TestTanExtended(t *testing.T) {
	t.Run("integer inputs", func(t *testing.T) {
		assertFloatResult(t, `(tan 0)`, 0.0, 1e-10)
		assertFloatResult(t, `(tan 1)`, math.Tan(1), 1e-10)
		assertFloatResult(t, `(tan -1)`, math.Tan(-1), 1e-10)
	})

	t.Run("float inputs", func(t *testing.T) {
		assertFloatResult(t, `(tan 0.0)`, 0.0, 1e-10)
		// tan(π/4) = 1
		assertFloatResult(t, `(tan 0.7853981633974483)`, 1.0, 1e-10)
		// tan(π) ≈ 0
		assertFloatResult(t, `(tan 3.141592653589793)`, 0.0, 1e-10)
	})

	t.Run("rational inputs", func(t *testing.T) {
		assertFloatResult(t, `(tan 1/4)`, math.Tan(0.25), 1e-10)
		assertFloatResult(t, `(tan -1/4)`, math.Tan(-0.25), 1e-10)
		assertFloatResult(t, `(tan 1/2)`, math.Tan(0.5), 1e-10)
	})

	t.Run("special values", func(t *testing.T) {
		// tan(+inf) = nan
		result, err := runSchemeCode(t, `(tan +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		resultFloat, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(resultFloat.Value), qt.IsTrue)

		// tan(nan) = nan
		result, err = runSchemeCode(t, `(tan +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		resultFloat, ok = result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(resultFloat.Value), qt.IsTrue)
	})
}

// TestTanErrors tests error conditions for tan
func TestTanErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "tan string", code: `(tan "hello")`},
		{name: "tan symbol", code: `(tan 'foo)`},
		{name: "tan list", code: `(tan '(1 2 3))`},
		{name: "tan boolean", code: `(tan #t)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestAsinExtended tests asin with various numeric types and edge cases
func TestAsinExtended(t *testing.T) {
	t.Run("integer inputs", func(t *testing.T) {
		assertFloatResult(t, `(asin 0)`, 0.0, 1e-10)
		assertFloatResult(t, `(asin 1)`, math.Pi/2, 1e-10)
		assertFloatResult(t, `(asin -1)`, -math.Pi/2, 1e-10)
	})

	t.Run("float inputs", func(t *testing.T) {
		assertFloatResult(t, `(asin 0.0)`, 0.0, 1e-10)
		assertFloatResult(t, `(asin 0.5)`, math.Asin(0.5), 1e-10)
		assertFloatResult(t, `(asin -0.5)`, math.Asin(-0.5), 1e-10)
		assertFloatResult(t, `(asin 1.0)`, math.Pi/2, 1e-10)
	})

	t.Run("rational inputs", func(t *testing.T) {
		assertFloatResult(t, `(asin 1/2)`, math.Asin(0.5), 1e-10)
		assertFloatResult(t, `(asin -1/2)`, math.Asin(-0.5), 1e-10)
		assertFloatResult(t, `(asin 3/4)`, math.Asin(0.75), 1e-10)
	})

	t.Run("outside domain returns Complex (R7RS)", func(t *testing.T) {
		// asin(2) is outside domain [-1, 1], returns complex per R7RS
		// asin(2) ≈ 1.5707963267948966 + 1.3169578969248166i (Go's branch cut convention)
		assertComplexResult(t, `(asin 2)`, 1.5707963267948966, 1.3169578969248166, 1e-10)
		// asin(-2) ≈ -1.5707963267948966 + 1.3169578969248166i (Go's branch cut convention)
		assertComplexResult(t, `(asin -2)`, -1.5707963267948966, 1.3169578969248166, 1e-10)
	})

	t.Run("special values", func(t *testing.T) {
		// asin(nan) = nan
		result, err := runSchemeCode(t, `(asin +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		resultFloat, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(resultFloat.Value), qt.IsTrue)
	})
}

// TestAsinErrors tests error conditions for asin
func TestAsinErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "asin string", code: `(asin "hello")`},
		{name: "asin symbol", code: `(asin 'foo)`},
		{name: "asin list", code: `(asin '(1 2 3))`},
		{name: "asin boolean", code: `(asin #t)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestAcosExtended tests acos with various numeric types and edge cases
func TestAcosExtended(t *testing.T) {
	t.Run("integer inputs", func(t *testing.T) {
		assertFloatResult(t, `(acos 1)`, 0.0, 1e-10)
		assertFloatResult(t, `(acos 0)`, math.Pi/2, 1e-10)
		assertFloatResult(t, `(acos -1)`, math.Pi, 1e-10)
	})

	t.Run("float inputs", func(t *testing.T) {
		assertFloatResult(t, `(acos 1.0)`, 0.0, 1e-10)
		assertFloatResult(t, `(acos 0.5)`, math.Acos(0.5), 1e-10)
		assertFloatResult(t, `(acos -0.5)`, math.Acos(-0.5), 1e-10)
		assertFloatResult(t, `(acos 0.0)`, math.Pi/2, 1e-10)
	})

	t.Run("rational inputs", func(t *testing.T) {
		assertFloatResult(t, `(acos 1/2)`, math.Acos(0.5), 1e-10)
		assertFloatResult(t, `(acos -1/2)`, math.Acos(-0.5), 1e-10)
		assertFloatResult(t, `(acos 3/4)`, math.Acos(0.75), 1e-10)
	})

	t.Run("outside domain returns Complex (R7RS)", func(t *testing.T) {
		// acos(2) is outside domain [-1, 1], returns complex per R7RS
		// acos(2) ≈ 0 - 1.3169578969248166i (Go's branch cut convention)
		assertComplexResult(t, `(acos 2)`, 0, -1.3169578969248166, 1e-10)
		// acos(-2) ≈ 3.141592653589793 - 1.3169578969248166i
		assertComplexResult(t, `(acos -2)`, math.Pi, -1.3169578969248166, 1e-10)
	})

	t.Run("special values", func(t *testing.T) {
		// acos(nan) = nan
		result, err := runSchemeCode(t, `(acos +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		resultFloat, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(resultFloat.Value), qt.IsTrue)
	})
}

// TestAcosErrors tests error conditions for acos
func TestAcosErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "acos string", code: `(acos "hello")`},
		{name: "acos symbol", code: `(acos 'foo)`},
		{name: "acos list", code: `(acos '(1 2 3))`},
		{name: "acos boolean", code: `(acos #t)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestAtanExtended tests atan with various numeric types and edge cases
func TestAtanExtended(t *testing.T) {
	t.Run("one-arg integer inputs", func(t *testing.T) {
		assertFloatResult(t, `(atan 0)`, 0.0, 1e-10)
		assertFloatResult(t, `(atan 1)`, math.Pi/4, 1e-10)
		assertFloatResult(t, `(atan -1)`, -math.Pi/4, 1e-10)
	})

	t.Run("one-arg float inputs", func(t *testing.T) {
		assertFloatResult(t, `(atan 0.0)`, 0.0, 1e-10)
		assertFloatResult(t, `(atan 1.0)`, math.Pi/4, 1e-10)
		assertFloatResult(t, `(atan -1.0)`, -math.Pi/4, 1e-10)
	})

	t.Run("one-arg rational inputs", func(t *testing.T) {
		assertFloatResult(t, `(atan 1/2)`, math.Atan(0.5), 1e-10)
		assertFloatResult(t, `(atan -1/2)`, math.Atan(-0.5), 1e-10)
		assertFloatResult(t, `(atan 3/4)`, math.Atan(0.75), 1e-10)
	})

	t.Run("two-arg atan2 integer inputs", func(t *testing.T) {
		assertFloatResult(t, `(atan 1 1)`, math.Pi/4, 1e-10)      // 45 degrees
		assertFloatResult(t, `(atan 1 0)`, math.Pi/2, 1e-10)      // 90 degrees
		assertFloatResult(t, `(atan 0 1)`, 0.0, 1e-10)            // 0 degrees
		assertFloatResult(t, `(atan -1 1)`, -math.Pi/4, 1e-10)    // -45 degrees
		assertFloatResult(t, `(atan 1 -1)`, 3*math.Pi/4, 1e-10)   // 135 degrees
		assertFloatResult(t, `(atan -1 -1)`, -3*math.Pi/4, 1e-10) // -135 degrees
	})

	t.Run("two-arg atan2 float inputs", func(t *testing.T) {
		assertFloatResult(t, `(atan 1.0 1.0)`, math.Pi/4, 1e-10)
		assertFloatResult(t, `(atan 2.0 2.0)`, math.Pi/4, 1e-10)
		assertFloatResult(t, `(atan 0.0 1.0)`, 0.0, 1e-10)
	})

	t.Run("two-arg atan2 rational inputs", func(t *testing.T) {
		assertFloatResult(t, `(atan 1/2 1/2)`, math.Pi/4, 1e-10)
		assertFloatResult(t, `(atan 1/2 3/4)`, math.Atan2(0.5, 0.75), 1e-10)
	})

	t.Run("special values one-arg", func(t *testing.T) {
		// atan(+inf) = π/2
		assertFloatResult(t, `(atan +inf.0)`, math.Pi/2, 1e-10)
		// atan(-inf) = -π/2
		assertFloatResult(t, `(atan -inf.0)`, -math.Pi/2, 1e-10)
		// atan(nan) = nan
		result, err := runSchemeCode(t, `(atan +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		resultFloat, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(resultFloat.Value), qt.IsTrue)
	})

	t.Run("special values two-arg", func(t *testing.T) {
		// atan(1, 0) = π/2
		assertFloatResult(t, `(atan 1 0)`, math.Pi/2, 1e-10)
		// atan(-1, 0) = -π/2
		assertFloatResult(t, `(atan -1 0)`, -math.Pi/2, 1e-10)
		// atan(0, 0) = 0
		assertFloatResult(t, `(atan 0 0)`, 0.0, 1e-10)
	})
}

// TestAtanErrors tests error conditions for atan
func TestAtanErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "atan string", code: `(atan "hello")`},
		{name: "atan symbol", code: `(atan 'foo)`},
		{name: "atan list", code: `(atan '(1 2 3))`},
		{name: "atan boolean", code: `(atan #t)`},
		{name: "atan2 second arg string", code: `(atan 1 "hello")`},
		{name: "atan2 second arg symbol", code: `(atan 1 'foo)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestTranscendentalComplex tests transcendental functions with complex inputs (R7RS).
// Expected values use Go's math/cmplx branch cut conventions.
// See CLAUDE.md "Transcendental Functions - Implementation Details" for branch cut documentation.
func TestTranscendentalComplex(t *testing.T) {
	t.Run("exp with complex", func(t *testing.T) {
		// exp(πi) = -1 (Euler's identity)
		assertComplexResult(t, `(exp 0+3.141592653589793i)`, -1.0, 0, 1e-10)
		// exp(1+i) ≈ e*cos(1) + e*sin(1)i ≈ 1.4686939399158851 + 2.2873552871788423i
		assertComplexResult(t, `(exp 1+1i)`, 1.4686939399158851, 2.2873552871788423, 1e-10)
		// exp(0+0i) = 1
		assertFloatResult(t, `(exp 0+0i)`, 1.0, 1e-10)
	})

	t.Run("log with complex", func(t *testing.T) {
		// log(i) = πi/2
		assertComplexResult(t, `(log 0+1i)`, 0, math.Pi/2, 1e-10)
		// log(-i) = -πi/2
		assertComplexResult(t, `(log 0-1i)`, 0, -math.Pi/2, 1e-10)
		// log(1+i) ≈ 0.3465735902799726 + 0.7853981633974483i
		assertComplexResult(t, `(log 1+1i)`, 0.3465735902799726, 0.7853981633974483, 1e-10)
	})

	t.Run("sin with complex", func(t *testing.T) {
		// sin(i) ≈ 0 + 1.1752011936438014i (sinh(1))
		assertComplexResult(t, `(sin 0+1i)`, 0, 1.1752011936438014, 1e-10)
		// sin(1+i) ≈ 1.2984575814159773 + 0.6349639147847361i
		assertComplexResult(t, `(sin 1+1i)`, 1.2984575814159773, 0.6349639147847361, 1e-10)
	})

	t.Run("cos with complex", func(t *testing.T) {
		// cos(i) ≈ 1.5430806348152437 + 0i (cosh(1))
		assertFloatResult(t, `(cos 0+1i)`, 1.5430806348152437, 1e-10)
		// cos(1+i) ≈ 0.8337300251311491 - 0.9888977057628651i
		assertComplexResult(t, `(cos 1+1i)`, 0.8337300251311491, -0.9888977057628651, 1e-10)
	})

	t.Run("tan with complex", func(t *testing.T) {
		// tan(i) ≈ 0 + 0.7615941559557649i (tanh(1))
		assertComplexResult(t, `(tan 0+1i)`, 0, 0.7615941559557649, 1e-10)
		// tan(1+i) ≈ 0.2717525853195117 + 1.0839233273386946i
		assertComplexResult(t, `(tan 1+1i)`, 0.2717525853195117, 1.0839233273386946, 1e-10)
	})

	t.Run("asin with complex", func(t *testing.T) {
		// asin(i) ≈ 0 + 0.8813735870195430i
		assertComplexResult(t, `(asin 0+1i)`, 0, 0.8813735870195430, 1e-10)
		// asin(1+i) ≈ 0.6662394324925153 + 1.0612750619050357i
		assertComplexResult(t, `(asin 1+1i)`, 0.6662394324925153, 1.0612750619050357, 1e-10)
	})

	t.Run("acos with complex", func(t *testing.T) {
		// acos(i) ≈ 1.5707963267948966 - 0.8813735870195430i
		assertComplexResult(t, `(acos 0+1i)`, math.Pi/2, -0.8813735870195430, 1e-10)
		// acos(1+i) ≈ 0.9045568943023813 - 1.0612750619050357i
		assertComplexResult(t, `(acos 1+1i)`, 0.9045568943023813, -1.0612750619050357, 1e-10)
	})

	t.Run("atan with complex (one-arg form)", func(t *testing.T) {
		// atan(0+2i) ≈ -π/2 + 0.5493061443340549i (Go's branch cut convention)
		assertComplexResult(t, `(atan 0+2i)`, -1.5707963267948968, 0.5493061443340549, 1e-10)
		// atan(1+i) ≈ 1.0172219678978514 + 0.4023594781085251i
		assertComplexResult(t, `(atan 1+1i)`, 1.0172219678978514, 0.4023594781085251, 1e-10)
	})
}

// TestTranscendentalIdentities tests mathematical identities
func TestTranscendentalIdentities(t *testing.T) {
	t.Run("sin^2 + cos^2 = 1", func(t *testing.T) {
		// For various x values, sin(x)^2 + cos(x)^2 should equal 1
		testValues := []string{"0", "1", "0.5", "1/2", "2", "3.14159"}
		for _, x := range testValues {
			code := `(let ((x ` + x + `)) (+ (* (sin x) (sin x)) (* (cos x) (cos x))))`
			assertFloatResult(t, code, 1.0, 1e-10)
		}
	})

	t.Run("tan = sin/cos", func(t *testing.T) {
		testValues := []string{"1", "0.5", "1/4", "2"}
		for _, x := range testValues {
			code := `(let ((x ` + x + `)) (- (tan x) (/ (sin x) (cos x))))`
			assertFloatResult(t, code, 0.0, 1e-10)
		}
	})

	t.Run("exp(log(x)) = x", func(t *testing.T) {
		testValues := []string{"1", "2", "0.5", "10", "100"}
		for _, x := range testValues {
			code := `(exp (log ` + x + `))`
			expected, _ := runSchemeCode(t, x)
			expectedFloat, ok := expected.(*values.Float)
			if !ok {
				if intVal, ok := expected.(*values.Integer); ok {
					expectedFloat = values.NewFloat(float64(intVal.Value))
				} else if ratVal, ok := expected.(*values.Rational); ok { //nolint:gocritic
					f, _ := ratVal.Rat().Float64()
					expectedFloat = values.NewFloat(f)
				}
			}
			assertFloatResult(t, code, expectedFloat.Value, 1e-10)
		}
	})

	t.Run("log(exp(x)) = x", func(t *testing.T) {
		testValues := []string{"0", "1", "-1", "0.5", "2"}
		for _, x := range testValues {
			code := `(log (exp ` + x + `))`
			expected, _ := runSchemeCode(t, x)
			var expectedVal float64
			switch v := expected.(type) {
			case *values.Float:
				expectedVal = v.Value
			case *values.Integer:
				expectedVal = float64(v.Value)
			case *values.Rational:
				expectedVal, _ = v.Rat().Float64()
			}
			assertFloatResult(t, code, expectedVal, 1e-10)
		}
	})

	t.Run("asin(sin(x)) = x for small x", func(t *testing.T) {
		// Only works for x in [-π/2, π/2]
		testValues := []string{"0", "0.5", "-0.5", "1", "-1"}
		for _, x := range testValues {
			code := `(asin (sin ` + x + `))`
			expected, _ := runSchemeCode(t, x)
			var expectedVal float64
			switch v := expected.(type) {
			case *values.Float:
				expectedVal = v.Value
			case *values.Integer:
				expectedVal = float64(v.Value)
			}
			assertFloatResult(t, code, expectedVal, 1e-10)
		}
	})

	t.Run("acos(cos(x)) = x for x in [0, π]", func(t *testing.T) {
		testValues := []string{"0", "0.5", "1", "2", "3"}
		for _, x := range testValues {
			code := `(acos (cos ` + x + `))`
			expected, _ := runSchemeCode(t, x)
			var expectedVal float64
			switch v := expected.(type) {
			case *values.Float:
				expectedVal = v.Value
			case *values.Integer:
				expectedVal = float64(v.Value)
			}
			assertFloatResult(t, code, expectedVal, 1e-10)
		}
	})

	t.Run("atan(tan(x)) = x for small x", func(t *testing.T) {
		// Only works for x in (-π/2, π/2)
		testValues := []string{"0", "0.5", "-0.5", "1", "-1"}
		for _, x := range testValues {
			code := `(atan (tan ` + x + `))`
			expected, _ := runSchemeCode(t, x)
			var expectedVal float64
			switch v := expected.(type) {
			case *values.Float:
				expectedVal = v.Value
			case *values.Integer:
				expectedVal = float64(v.Value)
			}
			assertFloatResult(t, code, expectedVal, 1e-10)
		}
	})
}
