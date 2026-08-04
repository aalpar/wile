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
//   - NaN ⟹ #t, but only against another NaN of the same kind. §6.1: "As an
//     exception, the behavior of eqv? is unspecified when both obj1 and obj2 are
//     NaN", so both answers conform; Wile follows Chez and Racket. Consult IsNaN,
//     never IEEE `==`: eqv? is an equivalence relation and must be reflexive,
//     which IEEE equality deliberately is not.
func EqvNumber(a, b Number) bool {
	// A typed-nil numeric pointer is void, and every method below would fault on
	// it. The per-type EqualTo methods this replaces each carried their own nil
	// guard; the guard survives their collapse, in one place.
	//
	// The answer is the interface compare, NOT `IsVoid(a) == IsVoid(b)`. A nil *Float
	// and a nil *BigInteger are both void, and they are not the same value — the older
	// form said they were, which was a WIDENING of what the per-type guards did (each
	// compared a typed pointer, so a nil of another kind fell through to false). A nil
	// Number is a Go-side defect in every case, and collapsing two distinct defects into
	// #t deletes the signal. Interface equality already carries the dynamic type, so it
	// gets this right for free.
	if IsVoid(a) || IsVoid(b) {
		return a == b
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
	// than comparing the numbers whole is deliberate: there is no ordering on the
	// complex plane, and the components need the signed-zero and precision rules
	// applied to each of them anyway.
	ca, aIsComplex := a.(ComplexNumber)
	cb, bIsComplex := b.(ComplexNumber)
	if aIsComplex != bIsComplex {
		// One complex, one real. Almost always #f — but an EXACT complex with an
		// EXACT-ZERO imaginary part IS a real number, and R7RS §6.1 requires two exact
		// numbers that are = to be eqv?. Rejecting outright made `=` and `eqv?`
		// contradict each other on such a value, which is precisely the lattice
		// violation this file exists to prevent.
		//
		// Scheme cannot construct one: make-rectangular canonicalizes, so
		// (make-rectangular 1 0) evaluates to 1. But values.NewBigComplex is PUBLIC API
		// and does not canonicalize, so an embedder can hand us one, and EqvNumber
		// claims to be the single authority on numeric equivalence. It does not get to
		// depend on an invariant it neither owns nor states.
		//
		// INEXACT is not the same case and must not collapse: 1.0+0.0i is a distinct
		// object from 1.0 (its imaginary part is an inexact zero, not an absent one),
		// and (eqv? 1.0 1.0+0.0i) is #f in Chez.
		cx, re := ca, b
		if bIsComplex {
			cx, re = cb, a
		}
		if !cx.IsExact() || !isExactZero(cx.ImagPart()) {
			return false
		}
		return EqvNumber(cx.RealPart(), re)
	}
	if aIsComplex {
		// Both complex. Decided by components, at any exactness. Recursing rather than
		// comparing the numbers whole is deliberate: there is no ordering on the complex
		// plane, and the components need the signed-zero and precision rules applied to
		// each of them anyway.
		if !EqvNumber(ca.RealPart(), cb.RealPart()) {
			return false
		}
		return EqvNumber(ca.ImagPart(), cb.ImagPart())
	}

	if a.IsExact() {
		// Exact operands are never NaN, so numEqual's precondition holds structurally.
		return numEqual(a, b)
	}

	// Inexact from here. Kind first: precision is observable for inexact numbers,
	// and that holds for NaN too — a float64 NaN and a BigFloat NaN are not the same
	// object in the same domain.
	if a.Kind() != b.Kind() {
		return false
	}
	// NaN is eqv? to NaN. §6.1 leaves this explicitly unspecified ("As an exception,
	// the behavior of eqv? is unspecified when both obj1 and obj2 are NaN"), so both
	// answers conform; Wile follows Chez and Racket in answering #t.
	//
	// The alternative — #f, which IEEE-754's own != gives you for free — makes
	// (memv +nan.0 lst) unable to find a NaN it did not itself allocate, and makes
	// the (case x ((+nan.0) …)) arm dead code that can never fire. Neither is useful,
	// and neither is what a Scheme programmer expects.
	//
	// Note this is NOT the same as IEEE equality, and must not be implemented with
	// it: eqv? is an EQUIVALENCE relation and so must be reflexive, while IEEE `==`
	// is deliberately not. Consult IsNaN, never `x == x`.
	aNaN := a.IsNaN()
	bNaN := b.IsNaN()
	if aNaN || bNaN {
		return aNaN && bNaN
	}
	if !numEqual(a, b) {
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

// numEqual reports whether two numbers are numerically equal, via antisymmetry:
// neither is less than the other. LessThan is the tower's only ordering
// primitive, so this is how equality is spelled here.
//
// CALLERS MUST HAVE EXCLUDED NaN. LessThan is false in both directions for a NaN
// operand, and this would read that as equal. The precondition is not checked
// because both call sites discharge it more cheaply than a check would: the exact
// path structurally (an exact number cannot be NaN), the inexact path by the
// explicit IsNaN guard that owns the NaN-is-eqv?-to-NaN rule anyway.
//
// This replaced a Compare(Number) int method on the Number interface. Compare
// answered a four-state question (less, equal, greater, unordered) in a
// three-state return, giving NaN a 0 that read as "equal" — so `Compare(b) == 0`,
// which is all this package ever used it for, was the one spelling its own doc
// called wrong. Both spare states are gone with it: NaN reaches neither branch of
// LessThan's bool, and there is no longer a public method whose contract has to be
// read to be used safely.
func numEqual(a, b Number) bool {
	return !a.LessThan(b) && !b.LessThan(a)
}

// Eqv implements eqv? per R7RS §6.1. It is the single authority, and it composes
// three rules rather than restating any of them:
//
//	identity  -> EqIdentity
//	numbers   -> EqvNumber   (also what equal? uses, via each numeric EqualTo)
//	chars     -> char= on the code point
//
// It lives here rather than in registry/helpers because Hashtable dispatches an
// eqv-kind table's key comparison through it, and pkg/values sits below
// registry. registry/helpers.Eqv re-exports it for API stability.
func Eqv(a, b Value) bool {
	if EqIdentity(a, b) {
		return true
	}
	na, ok := a.(Number)
	if ok {
		nb, ok := b.(Number)
		if !ok {
			return false
		}
		return EqvNumber(na, nb)
	}
	ca, ok := a.(*Character)
	if ok {
		cb, ok := b.(*Character)
		if !ok {
			return false
		}
		return ca.Value == cb.Value
	}
	return false
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
