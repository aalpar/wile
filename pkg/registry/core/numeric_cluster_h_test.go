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

package core_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
)

// Review wave 5, cluster H — the Scheme-visible half of six numeric defects.
// The embedder-only halves are Go tests next to their types
// (TestBigComplex_HashCodeAgreesWithRealForExactZeroImag for 2.2.32,
// TestBigComplex_Exactness for the 2.3.14 representation).
//
// Compared as rendered text rather than as values because NaN is not eqv? to
// itself: the defect DESTROYS the complexness, so "did the imaginary part
// survive" is exactly what the assertion has to see.
func TestNumericClusterH(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		// 2.2.31 — BigFloat's Multiply/Divide tested NaN AHEAD of the type
		// dispatch, so a mixed-kind operand was collapsed to a real +nan.0,
		// destroying both the real part and the complexness.
		{
			name: "2.2.31 multiply: BigFloat by a complex with a NaN imaginary part",
			code: `(* #m2.0 (make-rectangular 3.0 +nan.0))`,
			want: "6.0+nan.0i",
		},
		{
			name: "2.2.31 control: the reverse operand order already worked",
			code: `(* (make-rectangular 3.0 +nan.0) #m2.0)`,
			want: "6.0+nan.0i",
		},
		{
			name: "2.2.31 control: the Float sibling already worked",
			code: `(* 2.0 (make-rectangular 3.0 +nan.0))`,
			want: "6.0+nan.0i",
		},
		{
			// The + twin was the control that isolated this to * and /: Add
			// already places the NaN test inside its same-type branch.
			name: "2.2.31 multiply is commutative over #m x NaN",
			code: `(eqv? (* #m1.5 +nan.0) (* +nan.0 #m1.5))`,
			want: "#t",
		},
		{
			name: "2.2.31 control: addition was already commutative",
			code: `(eqv? (+ #m1.5 +nan.0) (+ +nan.0 #m1.5))`,
			want: "#t",
		},
		{
			// The / analogue: an edit to Multiply alone would leave this broken.
			// The target is the Float sibling's answer, not the reverse order's —
			// division is not commutative.
			name: "2.2.31 divide: BigFloat by a complex with a NaN imaginary part",
			code: `(/ #m2.0 (make-rectangular 3.0 +nan.0))`,
			want: "+nan.0+nan.0i",
		},
		{
			name: "2.2.31 control: the Float sibling of the divide",
			code: `(/ 2.0 (make-rectangular 3.0 +nan.0))`,
			want: "+nan.0+nan.0i",
		},

		// 2.3.11 — R7RS §3.4's storage model makes a freshly allocated string
		// mutable unless the procedure's own description says otherwise.
		// values.NewString defaults to IMMUTABLE, so every allocator that wants
		// the default has to say so explicitly.
		{
			name: "2.3.11 number->string returns a mutable string",
			code: `(let ((s (number->string 42))) (string-set! s 0 #\5) s)`,
			want: `"52"`,
		},
		{
			name: "2.3.11 number->string in a non-decimal radix is mutable too",
			code: `(let ((s (number->string 255 16))) (string-set! s 0 #\0) s)`,
			want: `"0f"`,
		},
		{
			name: "2.3.11 utf8->string returns a mutable string",
			code: `(let ((s (utf8->string (string->utf8 "abc")))) (string-set! s 0 #\z) s)`,
			want: `"zbc"`,
		},
		{
			name: "2.3.11 control: string-copy was already mutable",
			code: `(let ((s (string-copy "abc"))) (string-set! s 0 #\z) s)`,
			want: `"zbc"`,
		},

		// 2.3.14 — inexact conversion of a BigComplex is PER PART, and the
		// inexact representation of a real is float64. Producing BigFloat parts
		// made a value that printed as "3.0+4.0i" and read back as a *Complex,
		// so R7RS §6.2.7's write/read round trip failed.
		{
			name: "2.3.14 two inexact complexes that print alike are eqv?",
			code: `(eqv? (inexact (make-rectangular 3 4)) (make-rectangular 3.0 4.0))`,
			want: "#t",
		},
		{
			name: "2.3.14 the real part is a float64, not a BigFloat",
			code: `(real-part (inexact 3+4i))`,
			want: "3.0",
		},
		{
			name: "2.3.14 write/read round trip",
			code: `(eqv? (string->number (number->string (inexact 3+4i))) (inexact 3+4i))`,
			want: "#t",
		},
		{
			// The IsExact() conjunction guard made this branch a no-op: a
			// mixed-exactness BigComplex was returned unchanged, converting
			// nothing. Per-part conversion has no such branch.
			name: "2.3.14 a mixed-exactness complex actually converts",
			code: `(inexact (make-rectangular #m3 4))`,
			want: "3.0+4.0i",
		},
		{
			// ACCEPTED loss, stated so it is not rediscovered as a regression:
			// float64 saturation, the same loss rational.go documents as "what
			// Chez gives".
			name: "2.3.14 accepted: a real part beyond float64 range saturates",
			code: `(inexact (make-rectangular (expt 10 400) 1))`,
			want: "+inf.0+1.0i",
		},
		{
			name: "2.3.14 control: construction is untouched, #m parts still build a BigComplex",
			code: `(exact? (make-rectangular #m3 #m4))`,
			want: "#f",
		},
		{
			// Wave 1 §3.4 owns these two; cited rather than duplicated, because
			// this phase's job on them is preservation.
			name: "2.3.14 control: (eqv? 3.0 #m3.0) stays #f",
			code: `(eqv? 3.0 #m3.0)`,
			want: "#f",
		},
		{
			name: "2.3.14 control: (= 3.0 #m3.0) stays #t",
			code: `(= 3.0 #m3.0)`,
			want: "#t",
		},
		{
			name: "2.3.14 control: BigFloat still round-trips through its own syntax",
			code: `(number->string (string->number "2.5l0"))`,
			want: `"2.5l0"`,
		},

		// 2.2.33 — min and max are ordering operations, so they take reals.
		{
			name: "2.2.33 control: min over reals is unchanged",
			code: `(min 3 1 2)`,
			want: "1",
		},
		{
			name: "2.2.33 control: max keeps exactness contagion",
			code: `(max 1.0 2)`,
			want: "2.0",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// 2.2.33 — min and max were the only numeric primitives still reading
// BigComplex.LessThan, whose own doc says nothing may read it as such, so
// (min 1+2i 3) answered 1+2i: a non-real result from a real-valued procedure.
// The ordering predicates already rejected the same input, which is what makes
// this an internal-consistency defect rather than a conformance one — R7RS
// §6.2.6 already calls the inputs erroneous, so no conforming program observed
// the wrong value.
func TestNumericClusterH_MinMaxRejectNonRealComplex(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "min, complex second", Code: `(min 1+2i 3)`},
		{Name: "min, complex first", Code: `(min 3 1+2i)`},
		{Name: "max, complex second", Code: `(max 1+2i 3)`},
		{Name: "max, complex first", Code: `(max 3 1+2i)`},
		{Name: "min, complex alone", Code: `(min 1+2i)`},
		{Name: "min, complex in the 3+ argument path", Code: `(min 1 2 1+2i)`},
		// Controls: the ordering predicates and abs already rejected these, which
		// is the inconsistency min/max were on the wrong side of.
		{Name: "control: < rejects", Code: `(< 1+2i 3)`},
		{Name: "control: abs rejects", Code: `(abs 1+2i)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
