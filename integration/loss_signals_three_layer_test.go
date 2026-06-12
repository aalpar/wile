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

package integration_test

import (
	"context"
	"errors"
	"fmt"
	"math/big"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/wile"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"

	qt "github.com/frankban/quicktest"
)

// TestLossSignalsThreeLayerAgreement asserts that the Go helper, the
// FFI converter, and the Scheme primitive all report the same accuracy
// outcome for the same numeric input. A bug at any single layer
// manifests as inter-layer disagreement that per-layer unit tests
// cannot catch.
//
// For each (input, expected) row:
//
//	Layer 1: values.ToFloat64WithAccuracy(input) → acc, isReal (positional)
//	Layer 2: FFI converter strict mode → succeeds iff acc == Exact &&
//	         isReal; otherwise wraps ErrLossyConversion
//	Layer 3: (inexact-accuracy input) → same accuracy symbol(s)
//
// All three must agree, on every row.
func TestLossSignalsThreeLayerAgreement(t *testing.T) {
	cases := []struct {
		name        string
		schemeInput string
		goInput     func() values.Number
		wantAcc     big.Accuracy
		wantIsReal  bool
	}{
		{
			name:        "integer-exact",
			schemeInput: "7",
			goInput:     func() values.Number { return values.NewInteger(7) },
			wantAcc:     big.Exact,
			wantIsReal:  true,
		},
		{
			name:        "rational-onethird-below",
			schemeInput: "1/3",
			goInput:     func() values.Number { return values.NewRational(1, 3) },
			wantAcc:     big.Below,
			wantIsReal:  true,
		},
		{
			// 2^100 + 1 rounds DOWN to 2^100 in float64 — the next
			// representable step above 2^100 is 2^100 + 2^47, so +1
			// is well below half-ulp and the float64 representation
			// is below the true value.
			name:        "bigint-2to100-plus-one-below",
			schemeInput: "(+ (expt 2 100) 1)",
			goInput: func() values.Number {
				return values.NewBigInteger(new(big.Int).Add(
					new(big.Int).Lsh(big.NewInt(1), 100),
					big.NewInt(1)))
			},
			wantAcc:    big.Below,
			wantIsReal: true,
		},
		{
			// 10^100 ≈ 2^332.2; the float64 representation rounds
			// to the nearest float64 mantissa, which lands above
			// the true value (the exponent-aligned half-ulp is
			// positive).
			name:        "bigint-10to100-above",
			schemeInput: "(expt 10 100)",
			goInput: func() values.Number {
				return values.NewBigInteger(new(big.Int).Exp(
					big.NewInt(10), big.NewInt(100), nil))
			},
			wantAcc:    big.Above,
			wantIsReal: true,
		},
		{
			name:        "complex-3plus4i-exact-but-nonreal",
			schemeInput: "(make-rectangular 3 4)",
			goInput:     func() values.Number { return values.NewComplex(complex(3, 4)) },
			wantAcc:     big.Exact,
			wantIsReal:  false,
		},
		{
			// Real-part lossy (BigInteger 10^100 mantissa overflow,
			// rounds Above) AND imag-part lossy (Rational 1/3,
			// non-power-of-2 denominator, rounds Below). The
			// real-part accuracy is what Layer 1's float64 helper
			// reports, but Layer 2's strict-FFI must reject the
			// projection because !isReal.
			name:        "bigcomplex-mixed-lossy",
			schemeInput: "(make-rectangular (expt 10 100) 1/3)",
			goInput: func() values.Number {
				re := values.NewBigInteger(new(big.Int).Exp(
					big.NewInt(10), big.NewInt(100), nil))
				return values.NewBigComplex(re, values.NewRational(1, 3))
			},
			wantAcc:    big.Above,
			wantIsReal: false,
		},
	}

	ctx := context.Background()
	c := qt.New(t)
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)

			// Register a float64-taking callback per-subtest so the
			// fnCalled signal is scoped to this iteration. (A
			// shared package-level var would race if the test ever
			// adds t.Parallel().)
			var fnCalled bool
			probeName := fmt.Sprintf("layer2-probe-float64-%s",
				strings.ReplaceAll(tc.name, "-", "_"))
			err := eng.RegisterFunc(probeName, func(_ float64) {
				fnCalled = true
			})
			c.Assert(err, qt.IsNil)

			// --- Layer 1: Go helper directly. ---
			_, acc, isReal, helperErr := values.ToFloat64WithAccuracy(tc.goInput())
			c.Assert(helperErr, qt.IsNil)
			c.Assert(acc, qt.Equals, tc.wantAcc,
				qt.Commentf("Layer 1: accuracy disagreement on %s", tc.name))
			c.Assert(isReal, qt.Equals, tc.wantIsReal,
				qt.Commentf("Layer 1: isReal disagreement on %s", tc.name))

			// --- Layer 2: FFI converter (strict mode). ---
			code := fmt.Sprintf("(%s %s)", probeName, tc.schemeInput)
			_, ffiErr := eng.EvalMultiple(ctx, code)
			if tc.wantAcc == big.Exact && tc.wantIsReal {
				c.Assert(ffiErr, qt.IsNil,
					qt.Commentf("Layer 2: strict-mode FFI should succeed for %s (Layer 1 reported Exact+isReal)", tc.name))
				c.Assert(fnCalled, qt.IsTrue)
			} else {
				c.Assert(errors.Is(ffiErr, werr.ErrLossyConversion), qt.IsTrue,
					qt.Commentf("Layer 2: strict-mode FFI should error for %s (Layer 1 reported %v / isReal=%v), got: %v",
						tc.name, tc.wantAcc, tc.wantIsReal, ffiErr))
				c.Assert(fnCalled, qt.IsFalse)
			}

			// --- Layer 3: Scheme primitive (inexact-accuracy). ---
			//
			// For real input N, (inexact-accuracy N) → 1 symbol.
			// For complex N, (inexact-accuracy N) → 2 symbols; the
			// first matches the real-part accuracy.
			//
			// We unify both cases by collecting via call-with-values
			// and inspecting the car of the resulting list.
			schemeCode := fmt.Sprintf(
				"(call-with-values (lambda () (inexact-accuracy %s)) list)",
				tc.schemeInput)
			schemeResult, schemeErr := eng.EvalMultiple(ctx, schemeCode)
			c.Assert(schemeErr, qt.IsNil)

			list, ok := schemeResult.Internal().(values.Tuple)
			c.Assert(ok, qt.IsTrue,
				qt.Commentf("Layer 3: expected a list result, got %T", schemeResult.Internal()))
			firstSym, ok := list.Car().(*values.Symbol)
			c.Assert(ok, qt.IsTrue,
				qt.Commentf("Layer 3: expected first element to be a symbol, got %T", list.Car()))

			wantSym := values.BigAccuracyToSymbol(tc.wantAcc)
			c.Assert(firstSym.Key, qt.Equals, wantSym.Key,
				qt.Commentf("Layer 3: primitive disagreement on %s — want %s, got %s",
					tc.name, wantSym.Key, firstSym.Key))
		})
	}
}
