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

// EqvNumber reports whether two numbers are eqv? per R7RS §6.1. It is the SINGLE
// authority on numeric equivalence in Wile.
//
// Everything that asks "are these the same number?" routes here: eqv? (and so
// memv, assv, case) via registry/helpers.Eqv, and equal? (and so member, assoc,
// equal?-keyed hashtables) via each numeric type's EqualTo. That is not tidiness
// — §6.1 says equal? "returns the same as eqv? when applied to … numbers", with
// no latitude, so the two predicates MUST agree by construction rather than by
// two implementations happening to concur. They did not: the rule used to be
// written three times (helpers.Eqv, the per-type EqualTo methods, and the
// compiler's literalIdentical) and the copies disagreed on both signed zero and
// cross-representation inexacts.
//
// The rules, each traceable to §6.1:
//
//   - Identity first. Reflexivity is not optional, whatever the payload: eqv?
//     settles identity before it looks at a value, and equal? may never be finer
//     than eqv?. Without this a NaN would not be eqv? to itself, and (memv x lst)
//     would fail to find the very object it was handed.
//
//   - Exact vs inexact ⟹ #f. "one of obj1 and obj2 is an exact number but the
//     other is an inexact number."
//
//   - Both exact ⟹ compare numerically ACROSS representations. "both exact
//     numbers and are numerically equal (in the sense of =)." An Integer 1 and a
//     BigInteger 1 are the same number; how they are stored is not observable to
//     a Scheme program, so representation must not be compared.
//
//   - Both inexact ⟹ representation IS observable, so the kinds must match. A
//     float64 and an arbitrary-precision BigFloat of equal value are NOT
//     substitutable: (+ x 1e-20) tells them apart, which is exactly the
//     "yield the same results … under any finite composition of Scheme's standard
//     arithmetic procedures" test §6.1 states. This asymmetry with the exact case
//     is the whole subtlety — same code shape, opposite verdicts, and exactness is
//     what discriminates.
//
//   - Signed zero is DISTINGUISHED. §6.1's note says (eqv? 0.0 -0.0) is #f "if
//     negative zero is distinguished" — conditioned on the implementation, and
//     Wile distinguishes it: (/ 1.0 -0.0) is -inf.0 while (/ 1.0 0.0) is +inf.0.
//     That is a finite composition of standard arithmetic yielding different,
//     non-NaN results, so the #f clause fires. Numeric comparison cannot see this
//     (IEEE-754 says 0.0 == -0.0), which is why SignBit is consulted separately.
//
//   - NaN ⟹ #f across distinct objects. §6.1: "As an exception, the behavior of
//     eqv? is unspecified when both obj1 and obj2 are NaN." Both answers conform.
//     Wile answers #f; Chez answers #t. This is the one line to change if that
//     policy is ever revisited — see the plan's F3.
func EqvNumber(a, b Number) bool {
	// A typed-nil numeric pointer is void, and every method below would fault on
	// it. The per-type EqualTo methods this replaces each carried their own nil
	// guard; the guard survives their collapse, in one place.
	if IsVoid(a) || IsVoid(b) {
		return IsVoid(a) == IsVoid(b)
	}
	// Safe interface compare: every Value is Go-comparable by contract (see the
	// Value doc comment), and every Number is pointer-shaped.
	if a == b {
		return true
	}
	if a.IsExact() != b.IsExact() {
		return false
	}

	// Complex is decided by its components, at any exactness. Recursing rather
	// than calling Compare is deliberate: there is no ordering on the complex
	// plane, so Compare is not meaningful there, and the components need the
	// signed-zero and precision rules applied to each of them anyway.
	ca, aIsComplex := a.(ComplexNumber)
	cb, bIsComplex := b.(ComplexNumber)
	if aIsComplex || bIsComplex {
		if !aIsComplex || !bIsComplex {
			return false
		}
		if !EqvNumber(ca.RealPart(), cb.RealPart()) {
			return false
		}
		return EqvNumber(ca.ImagPart(), cb.ImagPart())
	}

	if a.IsExact() {
		return a.Compare(b) == 0
	}

	// Inexact from here.
	if a.IsNaN() || b.IsNaN() {
		return false
	}
	if a.Kind() != b.Kind() {
		return false
	}
	if a.Compare(b) != 0 {
		return false
	}
	// Numerically equal and same kind. Only the sign bit can still separate them,
	// and only at zero — for every other value the sign is already reflected in
	// the comparison above.
	ra, aIsReal := a.(RealNumber)
	rb, bIsReal := b.(RealNumber)
	if !aIsReal || !bIsReal {
		return true
	}
	return ra.SignBit() == rb.SignBit()
}

// eqvNumberValue is the Value-typed adapter every numeric EqualTo delegates to.
// It exists so that "equal? agrees with eqv? on numbers" (R7RS §6.1) is a
// property of the type system rather than of seven methods remembering to agree.
func eqvNumberValue(p Number, v Value) bool {
	n, ok := v.(Number)
	if !ok {
		return false
	}
	return EqvNumber(p, n)
}
