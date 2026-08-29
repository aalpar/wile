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

package helpers

import (
	"math"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// ToComplex128 converts a Scheme number to a Go complex128, supporting every
// numeric kind (Integer, BigInteger, Float, BigFloat, Rational, Complex,
// BigComplex). Non-numbers are rejected with werr.ErrNotANumber.
//
// The per-kind conversion is the numeric registry's single source of truth
// (values.NumberToComplex128Lossy); this helper only adds the Value→Number
// screening + error that the registry function (taking a typed Number) omits.
func ToComplex128(v values.Value) (complex128, error) {
	n, ok := v.(values.Number)
	if !ok {
		return 0, werr.WrapForeignErrorf(werr.ErrNotANumber, "expected a number but got %T", v)
	}
	return values.NumberToComplex128Lossy(n), nil
}

// ComplexOrFloat returns a Float if the imaginary part is zero,
// otherwise returns a Complex. This follows R7RS behavior where
// real results are returned as real numbers.
// Special case: If both parts are NaN, returns Float(NaN) since
// this typically results from operations on real NaN inputs.
func ComplexOrFloat(c complex128) values.Value {
	r := real(c)
	i := imag(c)
	if i == 0 || (math.IsNaN(r) && math.IsNaN(i)) {
		return values.NewFloat(r)
	}
	return values.NewComplex(c)
}

// ToFloat64 converts a Scheme real number to a Go float64 with a lossless
// guarantee. Real inputs (*Integer, *BigInteger, *Float, *BigFloat, *Rational)
// that fit float64 exactly succeed; ones that don't fit return
// werr.ErrLossyConversion (wrapped, message names direction Below/Above).
// Complex inputs (*Complex, *BigComplex) return werr.ErrNotAReal — the helper
// is real-domain only.
//
// Tightened in PR 2 of the numeric loss signals plan: previously this function
// silently truncated *BigFloat overflow, *BigInteger overflow, and *Rational
// with non-representable denominators (e.g., 1/3). Callers that need the
// silent-truncation behavior should call values.ToFloat64WithAccuracy directly
// and discard the accuracy slot. See CHANGELOG for migration guidance.
func ToFloat64(v values.Value) (float64, error) {
	n, err := screenReal(v)
	if err != nil {
		return 0, err
	}
	return values.ToFloat64Lossless(n)
}

// ToFloat64Lossy is the silent-truncation counterpart to ToFloat64: it shares
// the same real-domain screening (non-numbers and complex numbers are rejected
// with werr.ErrNotAReal) but, instead of erroring on precision loss, returns
// the rounded float64 from values.ToFloat64WithAccuracy with the accuracy slot
// discarded. ±Inf saturation on overflow follows (*big.Float).Float64().
//
// Use this only where lossy conversion is semantically correct because the
// result is inherently inexact — e.g. (atan y x) per R7RS §6.2.6. Callers that
// need a lossless guarantee must use ToFloat64. See
// memory/2026-05-14-numeric-loss-signals-design.md §R8.
func ToFloat64Lossy(v values.Value) (float64, error) {
	n, err := screenReal(v)
	if err != nil {
		return 0, err
	}
	// screenReal guarantees a non-nil real Number, so the error slot of
	// ToFloat64WithAccuracy (nil-input only) is unreachable; the accuracy and
	// isReal slots are intentionally discarded — silent truncation is the
	// whole point of this variant.
	f, _, _, _ := values.ToFloat64WithAccuracy(n)
	return f, nil
}

// screenReal asserts v is a real Scheme number, rejecting non-numbers and
// complex numbers with werr.ErrNotAReal. It is the shared screening for the
// strict (ToFloat64) and lossy (ToFloat64Lossy) extraction policies — the loss
// policy is the only thing that differs between them.
//
// Domain dispatch via the ComplexNumber interface — matches Hashable/Tuple/
// Indexable precedent in values/. Avoids enumerating *Complex and *BigComplex
// by name (would need updating for any new complex kind).
func screenReal(v values.Value) (values.Number, error) {
	n, ok := v.(values.Number)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAReal, "expected a real number but got %T", v)
	}
	_, isComplex := n.(values.ComplexNumber)
	if isComplex {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAReal, "expected a real number but got %T", v)
	}
	return n, nil
}

// ExtractReal extracts a float64 from a real number, tracking exactness.
// Returns the float64 value, whether the input was exact, and any error.
//
// R7RS §6.2.6: Division procedures work on all real numbers.
//
// Both halves defer to the numeric registry rather than enumerating the five
// real kinds here. That is not only volume: a *BigFloat NaN is carried by an
// out-of-band flag because big.Float has no NaN, so the obvious open-coded
// BigFloatValue().Float64() reads every NaN as 0.0 — which is what this
// function used to do, while its *Float sibling propagated NaN correctly.
func ExtractReal(v values.Value, name string) (float64, bool, error) {
	n, err := screenReal(v)
	if err != nil {
		return 0, false, werr.WrapForeignErrorf(err, "%s", name)
	}
	// screenReal guarantees a non-nil real Number, so the error slot
	// (nil-input only) and the isReal slot (complex-only) are both unreachable.
	// The accuracy slot is discarded deliberately: an operand of an inexact
	// division is rounded by definition, and R7RS §6.2.6 asks for no signal.
	f, _, _, _ := values.ToFloat64WithAccuracy(n)
	return f, n.IsExact(), nil
}
