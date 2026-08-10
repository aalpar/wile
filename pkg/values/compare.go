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

import "math/big"

// Ordering is the verdict of a numeric comparison. It has FOUR states, and the
// fourth is the whole point: the reals extended with NaN are not totally
// ordered, so any three-state answer has to give NaN one of the three and
// thereby lie about it. A Compare(Number) int used to live on the Number
// interface and did exactly that, handing NaN a 0 that every caller read as
// "equal".
type Ordering int

const (
	// OrderLess means the left operand is strictly less than the right.
	OrderLess Ordering = iota
	// OrderEqual means the two operands denote the same number. It is the only
	// verdict `=` accepts.
	OrderEqual
	// OrderGreater means the left operand is strictly greater than the right.
	OrderGreater
	// OrderUnordered means no ordering relation holds. All five of `=`, `<`,
	// `>`, `<=`, `>=` are false. Reached by a NaN operand, by a pair of
	// non-equal complex numbers, and by a Number outside the tower's kinds.
	OrderUnordered
)

// CompareNumbers is the numeric tower's total comparison kernel: the single
// authority behind `=`, `<`, `>`, `<=` and `>=`.
//
// ITS CONTRACT IS THAT IT NEVER ROUNDS AN OPERAND. Comparison yields a BOOLEAN,
// so nothing about it is inexact, and rounding an operand to reach a common
// domain throws away exactly the information the answer depends on. That claim
// used to be a body comment inside a dispatch generator while the code did the
// opposite; it is now this function's contract and the property test in
// pkg/wile/comparison_order_gate_test.go holds it to it.
//
// The construction, arm by arm:
//
//   - Both exact: compare exactly.
//   - Mixed exact/inexact, both finite: lift the INEXACT operand to its exact
//     rational value and compare exactly. Every finite float64 and every finite
//     big.Float is exactly a rational, so this direction always exists. The
//     other direction -- lifting the exact operand into the float's
//     representation -- never does, and R7RS §6.2.6 names the consequence:
//     "The implementation approach of converting all arguments to inexact
//     numbers if any argument is inexact is not transitive."
//   - Either operand infinite: compare by sign class. Equal infinities of the
//     same sign are OrderEqual, whatever representation carries them, so
//     (= x x) holds for an infinity in any kind.
//   - Either operand NaN: OrderUnordered.
//   - Complex: equality is component-wise. There is no ordering on the plane,
//     so an unequal pair is OrderUnordered; the ordering predicates raise on a
//     non-real operand before they ever reach here.
//
// eqv? is deliberately NOT derived from this kernel. EqvNumber gates on
// exactness and on kind, which R7RS §6.1 requires and `=` must not do, and
// (eqv? +nan.0 +nan.0) is #t while (= +nan.0 +nan.0) is #f.
//
// COST. Lifting to a rational allocates on the mixed-exactness path. That path
// is not hot in any Gabriel benchmark (they are float-float or fixnum-fixnum,
// both of which the same-kind fast paths below answer without allocating), and
// an interleaved A/B measured no end-to-end delta.
func CompareNumbers(a, b Number) Ordering {
	// Same-kind fast paths first, and cheapest first. These are the shapes the
	// VM's promoted comparison ops see, so they must not pay for the complex or
	// special-value analysis below.
	ia, ok := a.(*Integer)
	if ok {
		ib, ok := b.(*Integer)
		if ok {
			return orderInt64(ia.Value, ib.Value)
		}
	}
	fa, ok := a.(*Float)
	if ok {
		fb, ok := b.(*Float)
		if ok {
			return orderFloat64(fa.Value, fb.Value)
		}
	}
	return compareWide(a, b)
}

// crossKindLessThan is Number.LessThan's cross-kind arm, for every numeric type.
//
// It is NOT simply CompareNumbers(a, b) == OrderLess, and the difference is
// confined to complex operands. LessThan orders a complex by its REAL PART --
// see Complex.LessThan and BigComplex.LessThan, which have said so since before
// the kernel existed -- while the kernel answers OrderUnordered for two unequal
// complexes, because there is no order on the plane.
//
// Both are right for their callers. The five Scheme predicates reject a non-real
// operand before they reach either (NumericChainCompareReal, popTwoReals), so
// the only things that observe LessThan on a complex are min and max, where the
// real-part convention is what shipped. Routing the real parts through the
// kernel keeps that convention AND stops it rounding: 2^53+1 no longer compares
// equal to complex(2^53, 0).
//
// The tempting collapse is the trap the design named: deriving `=` from
// !a.LessThan(b) && !b.LessThan(a) makes (= 1.0+2.0i 1.0+5.0i) answer #t,
// because real-part ordering cannot see the imaginary components at all.
func crossKindLessThan(a, b Number) bool {
	ca, aIsComplex := a.(ComplexNumber)
	cb, bIsComplex := b.(ComplexNumber)
	if aIsComplex {
		a = ca.RealPart()
	}
	if bIsComplex {
		b = cb.RealPart()
	}
	return CompareNumbers(a, b) == OrderLess
}

// compareWide handles everything the same-kind fast paths did not: mixed kinds,
// arbitrary precision, and complex operands.
func compareWide(a, b Number) Ordering {
	_, aIsComplex := a.(ComplexNumber)
	_, bIsComplex := b.(ComplexNumber)
	if !aIsComplex && !bIsComplex {
		return compareReal(a, b)
	}
	// Component-wise, at any exactness, mirroring EqvNumber's complex arm. A
	// real operand contributes an exact zero imaginary part rather than a
	// manufactured inexact one.
	ra, ima := complexParts(a)
	rb, imb := complexParts(b)
	if compareReal(ra, rb) != OrderEqual {
		return OrderUnordered
	}
	if compareReal(ima, imb) != OrderEqual {
		return OrderUnordered
	}
	return OrderEqual
}

// complexParts splits a number into its real and imaginary components. A real
// number's imaginary part is an EXACT zero, not 0.0: promotion into complex128
// manufactures an inexact zero, and a manufactured inexactness is precisely the
// kind of editorializing this kernel exists to avoid.
func complexParts(n Number) (Number, Number) {
	c, ok := n.(ComplexNumber)
	if ok {
		return c.RealPart(), c.ImagPart()
	}
	return n, NewInteger(0)
}

// compareReal compares two real-valued numbers, which is every kind except
// Complex and BigComplex.
func compareReal(a, b Number) Ordering {
	if a.IsNaN() || b.IsNaN() {
		return OrderUnordered
	}
	sa := infiniteSign(a)
	sb := infiniteSign(b)
	if sa < sb {
		return OrderLess
	}
	if sa > sb {
		return OrderGreater
	}
	if sa != 0 {
		// Same non-zero sign class: two infinities of the same sign. This is
		// the arm that makes (= x x) hold for an infinity in a non-*Float
		// representation, where subtraction gave NaN and answered #f.
		return OrderEqual
	}
	return compareFinite(a, b)
}

// infiniteSign classifies a non-NaN real as -1 (negative infinity), 0 (finite)
// or +1 (positive infinity). Callers must have excluded NaN, which IsFinite
// also reports as non-finite and which has no meaningful sign.
func infiniteSign(n Number) int {
	if n.IsFinite() {
		return 0
	}
	r, ok := n.(RealNumber)
	if !ok {
		return 0
	}
	if r.SignBit() {
		return -1
	}
	return 1
}

// compareFinite compares two finite reals through their exact rational values,
// which is the only common domain that holds both without rounding either.
func compareFinite(a, b Number) Ordering {
	ra := exactRatOf(a)
	rb := exactRatOf(b)
	if ra == nil || rb == nil {
		return OrderUnordered
	}
	cmp := ra.Cmp(rb)
	if cmp < 0 {
		return OrderLess
	}
	if cmp > 0 {
		return OrderGreater
	}
	return OrderEqual
}

// exactRatOf returns the exact rational value of a finite real, or nil for a
// Number outside the tower's kinds.
//
// The result is READ-ONLY: the Rational arm hands back the value's own big.Rat
// rather than copying it, so a caller that mutates would corrupt a live number.
// Cmp is the only thing this file does with it.
//
// nil rather than a panic, and nil rather than a float64 fallback: Number is a
// public interface, an embedder can implement it, and the two alternatives are
// respectively an unwrapped panic out of `<` and a silent rounding that
// reintroduces the defect this kernel exists to remove. An unknown kind is
// unordered against everything, which is the same answer NaN gets and the same
// answer every predicate can state.
func exactRatOf(n Number) *big.Rat {
	switch v := n.(type) {
	case *Integer:
		return new(big.Rat).SetInt64(v.Value)
	case *BigInteger:
		return new(big.Rat).SetInt(v.value)
	case *Rational:
		return v.value
	case *Float:
		return new(big.Rat).SetFloat64(v.Value)
	case *BigFloat:
		q, _ := v.value.Rat(nil)
		return q
	}
	return nil
}

// orderInt64 is the fixnum-pair verdict.
func orderInt64(a, b int64) Ordering {
	if a < b {
		return OrderLess
	}
	if a > b {
		return OrderGreater
	}
	return OrderEqual
}

// orderFloat64 is the float64-pair verdict. A NaN operand makes all three IEEE
// comparisons false, which lands on OrderUnordered without a separate check.
func orderFloat64(a, b float64) Ordering {
	if a < b {
		return OrderLess
	}
	if a > b {
		return OrderGreater
	}
	if a == b {
		return OrderEqual
	}
	return OrderUnordered
}
