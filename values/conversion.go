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

package values

import (
	"math/big"

	"github.com/aalpar/wile/werr"
)

// Complex128Result captures complex-domain conversion with per-component
// accuracy. Field-named so RealAcc/ImagAcc swaps are caught at compile time,
// not surfaced only as wrong output in tests.
//
// Zero value is {Value: 0+0i, RealAcc: Exact, ImagAcc: Exact} since
// big.Exact == 0 in the stdlib enum. That happens to coincide with the
// "perfectly converted zero" reading; callers receiving an error should
// still treat the result as unspecified.
//
// See design plan §"Decision record: return shape — hybrid (positional + struct)".
type Complex128Result struct {
	Value   complex128   // complex128 representation
	RealAcc big.Accuracy // Below / Exact / Above for real component
	ImagAcc big.Accuracy // Below / Exact / Above for imaginary component
}

// ToFloat64WithAccuracy is the primary loss-signal-aware conversion helper.
// ToFloat64Lossless wraps it.
//
// Returns 4-tuple positional:
//   - f:      the float64 representation, saturated to ±Inf for overflow per
//     Go (*big.Float).Float64() semantics
//   - acc:    Below/Exact/Above per Go big.Accuracy semantics, describing
//     *only the real-axis rounding direction* of the returned float64
//     against the original real-axis value:
//     Below: f < real-axis true value (rounded down)
//     Exact: f == real-axis true value (lossless)
//     Above: f > real-axis true value (rounded up)
//     For Complex/BigComplex with non-zero imag, acc describes the real
//     component only; the imaginary-drop signal is carried solely by isReal.
//     The two channels (acc, isReal) are orthogonal — never collapse a
//     non-real input's loss into acc.
//   - isReal: false iff n was Complex/BigComplex with non-zero imaginary part
//     (the imaginary information is dropped — caller should use
//     ToComplex128WithAccuracy for full complex semantics)
//   - err:    ErrNotANumber (wrapped) on a defensive nil-Number input. The
//     signature is `n Number`, so a non-Number value cannot be passed
//     — the nil case is the only reachable error path.
//
// No information loss from the Go big package is introduced by this helper —
// every signal Go's stdlib surfaces is exposed through the four positional slots.
//
// NaN/Inf contract (per design Q-6 resolution): NaN inputs return
// (NaN, Exact, true, nil) — NaN→NaN is bit-pattern identity in IEEE 754, so
// accuracy is Exact mechanically. A *true* infinite input (*Float(math.Inf(1)))
// returns (+Inf, Exact, true, nil). Finite values that overflow during
// conversion return (±Inf, Above|Below, true, nil). Callers checking "is this
// a meaningful real number?" must screen finiteness independently via
// math.IsNaN(f) and math.IsInf.
//
// FFI lossy-allowed callers use this directly via the discard pattern
// `f, _, _, _ := ToFloat64WithAccuracy(n)`; FFI strict callers use
// ToFloat64Lossless.
func ToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool, error) {
	if n == nil {
		return 0, big.Exact, true, werr.WrapForeignErrorf(
			werr.ErrNotANumber, "ToFloat64WithAccuracy: nil input")
	}
	spec := LookupNumericSpec(n.Kind())
	f, acc, isReal := spec.ToFloat64WithAccuracy(n)
	return f, acc, isReal, nil
}

// ToFloat64Lossless is the FFI-strict convenience wrapper. Returns the raw
// float64 (callers in strict mode don't need the accuracy slot — they just
// want the value or an error). Returns ErrLossyConversion (wrapped, with
// direction info) if the conversion would lose precision OR drop the imaginary
// part.
func ToFloat64Lossless(n Number) (float64, error) {
	f, acc, isReal, err := ToFloat64WithAccuracy(n)
	if err != nil {
		return 0, err
	}
	if acc != big.Exact {
		return f, werr.WrapForeignErrorf(werr.ErrLossyConversion,
			"ToFloat64Lossless: %T rounded %s (lost precision)", n, acc)
	}
	if !isReal {
		return f, werr.WrapForeignErrorf(werr.ErrLossyConversion,
			"ToFloat64Lossless: %T has non-zero imaginary part dropped", n)
	}
	return f, nil
}

// ToComplex128WithAccuracy is the primary complex-domain helper. Returns a
// Complex128Result struct (named fields prevent realAcc/imagAcc swap bugs at
// call sites; same-type adjacency would otherwise admit silent swaps the
// compiler can't catch).
//
// For real-only inputs (Integer/BigInteger/Float/BigFloat/Rational),
// res.ImagAcc is always big.Exact.
//
// For nil-Number defensive input, returns ErrNotANumber.
func ToComplex128WithAccuracy(n Number) (Complex128Result, error) {
	if n == nil {
		return Complex128Result{RealAcc: big.Exact, ImagAcc: big.Exact},
			werr.WrapForeignErrorf(werr.ErrNotANumber,
				"ToComplex128WithAccuracy: nil input")
	}
	spec := LookupNumericSpec(n.Kind())
	return spec.ToComplex128WithAccuracy(n), nil
}

// ToComplex128Lossless returns the raw complex128, or ErrLossyConversion if
// either component's accuracy is non-Exact.
func ToComplex128Lossless(n Number) (complex128, error) {
	res, err := ToComplex128WithAccuracy(n)
	if err != nil {
		return 0, err
	}
	if res.RealAcc != big.Exact {
		return res.Value, werr.WrapForeignErrorf(werr.ErrLossyConversion,
			"ToComplex128Lossless: %T real part rounded %s (lost precision)", n, res.RealAcc)
	}
	if res.ImagAcc != big.Exact {
		return res.Value, werr.WrapForeignErrorf(werr.ErrLossyConversion,
			"ToComplex128Lossless: %T imaginary part rounded %s (lost precision)", n, res.ImagAcc)
	}
	return res.Value, nil
}
