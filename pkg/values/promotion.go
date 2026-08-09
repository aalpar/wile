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
	"math"
	"math/big"
	"sync"

	"github.com/aalpar/wile/pkg/werr"
)

// Join-semilattice (Davey & Priestley 2002). Numeric promotion is a
// symmetric, associative, idempotent binary operation on NumericKind.
//
//	T(a,b) = T(b,a)             commutativity
//	T(T(a,b),c) = T(a,T(b,c))  associativity
//	T(a,a) = a                  idempotency
//
//	where T = promotionTable[a][b], a/b/c ∈ NumericKind.
//	L = L_precision × L_complexity (product of two total orders).
//
//	Invariant: exactness is monotone w.r.t. T. If a is exact and b is
//	  exact, T(a,b) must also be exact-capable. Exact types never route
//	  through float64 (lossy). Enforced by initPromotionTable.
//	Constrains: the arithmetic and ordering dispatch tables (all 245 entries
//	  assume LUB correctness), Simplify (demotes within same exactness class).
//	Constrained by: Exactness lattice {exact < inexact} — the transfer
//	  function for arithmetic must be monotone in this ordering.
//
// See BIBLIOGRAPHY.md "Numeric Promotion Lattice".
//

// promotionTable maps (kindA, kindB) → resultKind, the least upper bound (LUB)
// in the numeric promotion lattice. The table is symmetric:
// promotionTable[a][b] == promotionTable[b][a] for all a, b.
//
// The lattice is NOT linear. It has two independent dimensions — precision
// and complexity — forming a partial order:
//
//	Exact chain:    Integer → BigInteger → Rational
//	Inexact real:   Float → BigFloat
//	Complex:        Complex → BigComplex
//
// Meeting an exact kind with an inexact one lands at the INEXACT operand's own
// kind — Float stays Float, BigFloat stays BigFloat — never one step up the
// precision axis. That is exactness contagion (R7RS §6.2.2): the exact operand
// is absorbed, and the inexact operand's representation decides the result.
//
//	Integer/BigInteger/Rational × Float      → Float       (contagion, lossy)
//	Integer/BigInteger/Rational × BigFloat   → BigFloat
//	Integer/BigInteger/Rational × Complex    → Complex     (contagion, lossy)
//	Integer/BigInteger/Rational × BigComplex → BigComplex
//	Float × BigFloat → BigFloat        Float × Complex → Complex
//	BigFloat × Complex → BigComplex    (BigFloat cannot fit in complex128)
//
// EXACT→INEXACT PROMOTIONS ARE LOSSY, deliberately. Rounding an exact operand to
// float64 is the contagion, not a defect of it; an exact value beyond float64's
// range becomes ±Inf, matching Chez. Exact×exact never touches float64, so
// exactness is never lost while it is still being claimed.
//
// THE THREE LAWS ARE LOAD-BEARING, not decoration. Associativity in particular: the
// result KIND is observable through eqv?/equal? (R7RS §6.1 makes representation
// observable for inexacts), so a non-associative join means (+ a b c) and (+ c a b) can
// produce values that are = and print identically but are not eqv?. Zone 3 once escalated
// exact × Complex to BigComplex while Zone 2 contaminated exact × Float down to Float,
// which broke associativity on 12 of the 343 triples and did exactly that.
// TestPromotionTable_Associativity holds this table to all three laws;
// TestLattice_PredictionsVsActual holds it to an independently-derived model.
//
// A previous linear tower (TowerAdd, etc.) was prototyped and abandoned because
// it forced exact reals through float64 ON THE WAY TO an exact-capable result,
// destroying exactness where exactness was still promised. That is a different
// thing from the lossy contagion above, which only ever lands on a kind that is
// already declaring itself inexact. See docs/numeric/tower.md.
var promotionTable [numKinds][numKinds]NumericKind

// promoter maps (srcKind, resultKind) → conversion function.
// promoter[src][dst](n) converts a Number of kind src to a Number of kind dst.
// Only entries reachable via promotionTable are populated; others are nil.
// Exact→exact and exact→Big* conversions are lossless. Exact→Float and
// exact→Complex are the exactness-contagion promotions and ARE lossy: the exact
// operand is rounded to float64 (±Inf beyond range). See initPromoters.
//
// Diagonal entries (src == dst) are identity functions.
var promoter [numKinds][numKinds]func(Number) Number

var promotionOnce sync.Once

// ensurePromotionInit lazily initializes the promotion tables. Called by the
// arithmetic dispatch generators (makeArithmeticDispatch and friends), which may
// run from type-file init() functions before promotion.go's own init().
// makeLessThanDispatch does NOT need it: comparison no longer promotes.
// Startup cost: populates one 7×7 kind table (49 entries) and the 7×7
// promoter-function table. The 245 dispatch-table entries are built separately,
// by each numeric type's init().
func ensurePromotionInit() {
	promotionOnce.Do(func() {
		initPromotionTable()
		initPromoters()
	})
}

func init() {
	ensurePromotionInit()
}

// initPromotionTable populates the symmetric 7×7 result type matrix.
//
// Three zones:
//  1. Exact×Exact (Integer, BigInteger, Rational): stays exact
//  2. Exact×InexactReal: lands at the inexact operand's kind (Float→Float, BigFloat→BigFloat)
//  3. Anything×Complex: the same contagion (exact→Complex); BigFloat×Complex→BigComplex
func initPromotionTable() {
	I := KindInteger
	BI := KindBigInteger
	F := KindFloat
	BF := KindBigFloat
	R := KindRational
	C := KindComplex
	BC := KindBigComplex

	// Helper: set both (a,b) and (b,a) for symmetry.
	set := func(a, b, result NumericKind) {
		promotionTable[a][b] = result
		promotionTable[b][a] = result
	}

	// Zone 1: Exact × Exact
	set(I, I, I)
	set(I, BI, BI)
	set(I, R, R)
	set(BI, BI, BI)
	set(BI, R, R)
	set(R, R, R)

	// Zone 2: Exact × Inexact Real → the INEXACT OPERAND'S precision.
	//
	// This is R7RS §6.2.2 exactness contagion, and the contagion is the whole
	// point: (+ 1.5 2) is 3.5 as a Float, not as a 256-bit BigFloat. The exact
	// operand is absorbed into the inexact one's representation; it does not drag
	// the result up the precision axis. Every other Scheme does this — Chez gives
	// (+ 1.5 2) => 3.5 flonum, and (+ 1.5 (expt 2 2000)) => +inf.0.
	//
	// Yes, that is a lossy promotion, and it is the correct one. "Inexact" is a
	// promise that precision may be lost; a program that wants the precision must
	// stay exact, or say so with an explicit BigFloat (#m) operand — which routes
	// through the BF rows below and is preserved.
	//
	// This used to send exact × Float to BigFloat "to preserve precision," on the
	// theory that Simplify would demote afterwards. Per-op demotion was never
	// wired (Simplify runs at parse time only), so ordinary float arithmetic
	// silently minted 256-bit bignums that never came back down: (+ 1.5 2) was a
	// *BigFloat. It went unnoticed for as long as it did because the numeric
	// EqualTo methods compared across kinds, so a BigFloat 3.5 tested equal to a
	// Float 3.5 and the tests passed. Making equal? agree with eqv? (R7RS §6.1)
	// took that cover away. See docs/numeric/tower.md.
	for _, exact := range []NumericKind{I, BI, R} {
		set(exact, F, F)
		set(exact, BF, BF)
	}

	// Float × Float, Float × BigFloat, BigFloat × BigFloat
	set(F, F, F)
	set(F, BF, BF)
	set(BF, BF, BF)

	// Zone 3: Anything × Complex — the SAME contagion as Zone 2, on the complex axis.
	//
	// An exact real meeting an inexact Complex yields a Complex, exactly as an exact
	// real meeting a Float yields a Float. It has to: this table is a join-semilattice,
	// and the three laws in the doc comment above are not decoration. Given Zone 2's
	// exact ⊔ Float = Float and the (unavoidable) Float ⊔ Complex = Complex,
	// associativity FORCES exact ⊔ Complex = Complex:
	//
	//	(exact ⊔ Float) ⊔ Complex  =  Float ⊔ Complex  =  Complex
	//	exact ⊔ (Float ⊔ Complex)  =  exact ⊔ Complex  =  ???
	//
	// Any two of the three entries determine the third. This one used to say BigComplex
	// while Zone 2 said Float, which broke the law on 12 of the 343 triples and made
	// (+ 1 1.5 2.0+0.0i) and (+ 2.0+0.0i 1 1.5) produce values that are =, print
	// identically, and are NOT eqv?. It also minted a 256-bit BigComplex out of
	// (* 1.0+2.0i 1) — multiplication by exact 1 — which then failed equal? against
	// the very literal it came from. TestPromotionTable_Associativity pins the law.
	//
	// THE EXACT-ZERO HAZARD IS REAL, AND IT IS NOT SOLVED BY THIS TABLE.
	//
	// Promoting an exact real into complex128 manufactures an imaginary part, and a
	// manufactured +0.0 is not an exact 0: the exact-zero rules that give complex
	// arithmetic its signs stop applying, and the sign is eaten.
	//
	//	(/ 10 2.0+0.0i)  =>  5.0-0.0i
	//
	// The imaginary part is NEGATIVE zero, because the general formula computes it as
	// (b*c - a*d) = 0 - 0.0, and an EXACT zero minus an inexact one negates. Chez and
	// Racket agree. Route the 10 through complex128 first and it becomes 10+0.0i, the
	// subtraction becomes 0.0 - 0.0 = +0.0, and the answer is silently wrong.
	//
	// Escalating to BigComplex was one way to dodge that — it kept the exact zero
	// alive in an exact component — but it bought signed-zero correctness with a
	// broken lattice, and it was not even the way FLOAT solved the same problem.
	// Float ⊔ Complex has always been Complex, and Float has always protected the sign
	// at the OPERATION, not in the table: a real operand contributes no imaginary
	// component, so real ⊕ complex is computed part-wise and never manufactures one.
	// See the real ⊕ complex helpers in complex.go. The exact kinds now do the same,
	// which is why they can safely share Float's row here.
	//
	// The rule, stated once: exactness contagion is a PROMOTION question and the table
	// owns it; the exact zero is an OPERATION question and complex.go owns it. They
	// were tangled together, and the tangle cost the semilattice.
	for _, exact := range []NumericKind{I, BI, R} {
		set(exact, C, C)
		set(exact, BC, BC)
	}
	// Float + Complex → Complex (both float64-based, no loss).
	set(F, C, C)
	// Float + BigComplex → BigComplex.
	set(F, BC, BC)
	// BigFloat + Complex → BigComplex (BigFloat can't fit in complex128).
	set(BF, C, BC)
	// BigFloat + BigComplex → BigComplex.
	set(BF, BC, BC)
	// Complex × Complex, Complex × BigComplex, BigComplex × BigComplex.
	set(C, C, C)
	set(C, BC, BC)
	set(BC, BC, BC)
}

// initPromoters populates the conversion functions.
// promoter[src][dst] converts a Number of kind src to kind dst.
// Only entries reachable from promotionTable are populated.
//
// Exact→exact and exact→Big* conversions are lossless. Exact→Float and
// exact→Complex are NOT: they are the exactness-contagion promotions (Zone 2/3
// above), and rounding to float64 is precisely what they are for. An integer too
// large for float64 becomes ±Inf, which is the same answer Chez gives.
func initPromoters() {
	// Identity promoters (diagonal): every type promotes to itself.
	for k := range numKinds {
		promoter[k][k] = func(n Number) Number {
			return n
		}
	}

	// Exact → Float and Exact → Complex: the contagion promotions (promotionTable
	// Zones 2 and 3). Lossy by design — rounding the exact operand IS the contagion.
	// Written as a loop over the exact kinds because the conversion is identical for
	// all three; NumberToFloat64 already dispatches on kind.
	//
	// THE EXACT → COMPLEX PROMOTER IS NOT SAFE FOR ARITHMETIC, and arithmetic does not
	// use it. It manufactures a +0.0 imaginary part, which is an INEXACT zero; the
	// exact-zero sign rules then stop applying and (+ 1 5.0-0.0i) comes back 6.0+0.0i
	// instead of 6.0-0.0i. That is a fact about the OPERATION, not about the
	// promotion: as a pure value-level embedding of a real into complex128, r ↦ r+0.0i
	// is the right answer, and that is what Promote (the public accessor) means.
	//
	// The four arithmetic operations never route a real operand through here — they
	// compute real ⊕ complex part-wise (the four real⊕complex helpers in complex.go,
	// wired into the dispatch generators as realComplexOp), which never
	// manufactures an imaginary component at all. Float has worked this way since
	// Float ⊔ Complex was Complex; the exact kinds now do too. If you find yourself
	// reaching for this promoter inside an arithmetic path, that is the bug.
	for _, exact := range []NumericKind{KindInteger, KindBigInteger, KindRational} {
		promoter[exact][KindFloat] = func(n Number) Number {
			return NewFloat(NumberToFloat64(n))
		}
		promoter[exact][KindComplex] = func(n Number) Number {
			return NewComplex(complex(NumberToFloat64(n), 0))
		}
	}

	// Integer → BigInteger
	promoter[KindInteger][KindBigInteger] = func(n Number) Number {
		p := n.(*Integer)
		return NewBigIntegerFromInt64(p.Value)
	}

	// Integer → Rational
	promoter[KindInteger][KindRational] = func(n Number) Number {
		p := n.(*Integer)
		return NewRationalFromRat(new(big.Rat).SetInt64(p.Value))
	}

	// Integer → BigFloat
	promoter[KindInteger][KindBigFloat] = func(n Number) Number {
		p := n.(*Integer)
		return &BigFloat{value: new(big.Float).SetPrec(DefaultBigFloatPrecision).SetInt64(p.Value)}
	}

	// Integer → BigComplex (exact: BigInteger parts)
	promoter[KindInteger][KindBigComplex] = func(n Number) Number {
		p := n.(*Integer)
		return NewBigComplex(NewBigIntegerFromInt64(p.Value), NewBigIntegerFromInt64(0))
	}

	// BigInteger → Rational
	promoter[KindBigInteger][KindRational] = func(n Number) Number {
		p := n.(*BigInteger)
		return NewRationalFromRat(new(big.Rat).SetInt(p.value))
	}

	// BigInteger → BigFloat
	promoter[KindBigInteger][KindBigFloat] = func(n Number) Number {
		p := n.(*BigInteger)
		return &BigFloat{value: new(big.Float).SetPrec(DefaultBigFloatPrecision).SetInt(p.value)}
	}

	// BigInteger → BigComplex (exact: BigInteger parts)
	promoter[KindBigInteger][KindBigComplex] = func(n Number) Number {
		p := n.(*BigInteger)
		return NewBigComplex(p, NewBigIntegerFromInt64(0))
	}

	// Rational → BigFloat
	promoter[KindRational][KindBigFloat] = func(n Number) Number {
		p := n.(*Rational)
		return &BigFloat{value: new(big.Float).SetPrec(DefaultBigFloatPrecision).SetRat(p.value)}
	}

	// Rational → BigComplex (exact: Rational real part, BigInteger zero imag)
	// BigComplex accepts *Rational parts directly — preserves exactness.
	promoter[KindRational][KindBigComplex] = func(n Number) Number {
		p := n.(*Rational)
		return NewBigComplex(p, NewBigIntegerFromInt64(0))
	}

	// Float → BigFloat
	promoter[KindFloat][KindBigFloat] = func(n Number) Number {
		p := n.(*Float)
		if math.IsNaN(p.Value) {
			return NewBigFloatNaN()
		}
		return &BigFloat{value: new(big.Float).SetPrec(DefaultBigFloatPrecision).SetFloat64(p.Value)}
	}

	// Float → Complex
	promoter[KindFloat][KindComplex] = func(n Number) Number {
		p := n.(*Float)
		return NewComplex(complex(p.Value, 0))
	}

	// Float → BigComplex
	//
	// The manufactured imaginary part is an EXACT zero, not an inexact 0.0, and that
	// is load-bearing rather than cosmetic. A real number has NO imaginary component;
	// the zero we invent here is a MATHEMATICAL zero, not an IEEE value that happens
	// to measure zero. Spell it inexact and IEEE gets to act on it: -0.0 + 0.0 is
	// +0.0, so (+ 5.0-0.0i 2.0) silently loses the sign of the imaginary part.
	//
	// Spelled EXACT, the exact-zero rules do the work for free (exact_zero.go): the
	// additive identity hands the other operand back untouched, sign and all, and the
	// annihilation rule kills the cross terms in a product. The exact reals -- whose
	// promoters have always used an exact zero, just below -- were never broken, which
	// is what pointed at this.
	//
	// Contagion is NOT compromised by the exact zero: the operand pair is still
	// inexact (BigComplex.IsExact() is false when either part is), and the complex
	// arithmetic re-imposes the join via contagionOverParts, so no exact component
	// leaks into an inexact result.
	promoter[KindFloat][KindBigComplex] = func(n Number) Number {
		p := n.(*Float)
		return NewBigComplex(NewBigFloatFromFloat64(p.Value), NewBigIntegerFromInt64(0))
	}

	// BigFloat → BigComplex. Exact zero imaginary part, for the reason above.
	promoter[KindBigFloat][KindBigComplex] = func(n Number) Number {
		p := n.(*BigFloat)
		return NewBigComplex(p, NewBigIntegerFromInt64(0))
	}

	// Complex → BigComplex
	promoter[KindComplex][KindBigComplex] = func(n Number) Number {
		p := n.(*Complex)
		return NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(p.Value)),
			NewBigFloatFromFloat64(imag(p.Value)),
		)
	}

	validatePromotionTable()
}

// validatePromotionTable asserts that every cross-type promotion path has a
// non-nil promoter function. This is a startup assertion — if it fails, a
// numeric type was added without completing its promotion entries. Panics at
// program init, never at runtime.
// It used to walk a map of two tables, promotion and comparison. Comparison no
// longer promotes, so there is one table and the loop over the map went with the
// second entry.
func validatePromotionTable() {
	for src := range numKinds {
		for dst := range numKinds {
			if src == dst {
				continue
			}
			lub := promotionTable[src][dst]
			if promoter[src][lub] == nil {
				panic(werr.WrapForeignErrorf(werr.ErrInvariantViolation,
					"incomplete promotion table: promoter[%d][%d] is nil (src=%d dst=%d lub=%d)",
					src, lub, src, dst, lub))
			}
			if promoter[dst][lub] == nil {
				panic(werr.WrapForeignErrorf(werr.ErrInvariantViolation,
					"incomplete promotion table: promoter[%d][%d] is nil (src=%d dst=%d lub=%d)",
					dst, lub, src, dst, lub))
			}
		}
	}
}

// Promote converts a Number to the target NumericKind using the promoter table.
// Conversions to Float/Complex from an exact kind are the lossy contagion
// promotions; all others are lossless. Panics if no promotion path exists
// (indicates a bug in the promotion table — all reachable paths should be
// populated).
func Promote(n Number, target NumericKind) Number {
	src := n.Kind()
	if src == target {
		return n
	}
	fn := promoter[src][target]
	if fn == nil {
		panic(werr.WrapForeignErrorf(werr.ErrNotANumber, "Promote: no promoter from kind %d to kind %d", src, target))
	}
	return fn(n)
}

// isSpecialFloat reports whether a Float holds IEEE 754 Inf or NaN.
// Used by the arithmetic dispatches (makeArithmeticDispatch, makeDivideDispatch)
// to fall back to float64/complex128 arithmetic when the LUB is
// BigFloat/BigComplex. The comparison dispatch deliberately has no such guard.
func isSpecialFloat(f *Float) bool {
	return math.IsInf(f.Value, 0) || math.IsNaN(f.Value)
}

// NumberToFloat64 converts any Number to a best-effort float64 approximation.
//
// Behavior across kinds:
//   - Integer/BigInteger/Float/BigFloat/Rational: silent precision loss is
//     possible (BigInteger > 2^53, BigFloat with extra precision, Rational
//     like 1/3). Use ToFloat64WithAccuracy via the spec for loss signals.
//   - Complex/BigComplex with imag == 0: returns the real part (lossless
//     since no information is discarded).
//   - Complex/BigComplex with imag != 0: panics with ErrNotAReal; the
//     imaginary component cannot be carried in a float64. Callers in
//     extensions/math should Simplify() the value first if they want
//     zero-imag complex inputs to flow through transparently.
func NumberToFloat64(n Number) float64 {
	f, _, ok := LookupNumericSpec(n.Kind()).ToFloat64WithAccuracy(n)
	if !ok {
		panic(werr.WrapForeignErrorf(werr.ErrNotAReal,
			"NumberToFloat64: cannot convert %T to float64", n))
	}
	return f
}

// NumberToComplex128Lossy converts any Number to complex128, discarding
// per-component precision-loss signals. BigFloat and BigComplex values are
// reduced to float64/complex128 precision. Intended for paths where
// precision loss is acceptable, such as IEEE 754 Inf/NaN guards and inexact
// complex arithmetic in extensions. Callers needing loss signals should use
// ToComplex128WithAccuracy directly.
func NumberToComplex128Lossy(n Number) complex128 {
	return LookupNumericSpec(n.Kind()).ToComplex128WithAccuracy(n).Value
}

// makeArithmeticDispatch generates a dispatch table for an arithmetic
// operation (Add, Subtract, Multiply, Divide). The same-type entry uses
// the hand-written sameTypeOp (preserving hot-path performance — e.g.,
// Integer+Integer overflow detection). Cross-type entries promote both
// operands to the LUB type via the promotion table and delegate to the
// LUB type's operation via applyOp.
//
// IEEE 754 special-value guard: when the LUB goes beyond float64/complex128
// (Float×BigFloat → BigFloat, Float×BigComplex → BigComplex) and the Float
// operand holds Inf or NaN, precision is irrelevant: the result is determined by
// the special value. The guard short-circuits to float64 arithmetic in that
// case, preserving the *Float return type.
//
// For the Float×BigComplex → BigComplex case, the guard uses complex128
// arithmetic but wraps the result in BigComplex (not Complex) so the imaginary
// part of the BigComplex operand is preserved. This is the fix for issue #362.
func makeArithmeticDispatch[T Number](
	srcKind NumericKind,
	sameTypeOp func(T, Number) Number,
	applyOp func(Number, Number) Number,
	float64Op func(float64, float64) float64,
	complex128Op func(complex128, complex128) complex128,
	realComplexOp func(float64, complex128) complex128,
) [numKinds]func(T, Number) Number {
	ensurePromotionInit()
	var table [numKinds]func(T, Number) Number
	table[srcKind] = sameTypeOp
	for dstKind := range numKinds {
		if dstKind == srcKind {
			continue
		}
		lubKind := promotionTable[srcKind][dstKind]
		promSrc := promoter[srcKind][lubKind]
		promDst := promoter[dstKind][lubKind]

		// IEEE 754 special-value guard: when Float is the receiver and the LUB
		// goes beyond float64/complex128, short-circuit for Inf/NaN values.
		lubNeedsGuard := lubKind != KindFloat && lubKind != KindComplex

		switch {
		case dstKind == KindComplex && lubKind == KindComplex:
			// A REAL receiver meeting a Complex operand. The kinds that land here are
			// exactly those absorbed into complex128 — Float, Integer, BigInteger,
			// Rational — and for every one of them the promoter would manufacture a
			// +0.0 imaginary part the receiver does not have, letting IEEE eat the sign
			// of a signed-zero component in o. Compute part-wise instead; the imaginary
			// component is never invented. See the real ⊕ complex helpers in complex.go.
			//
			// The mirror case (Complex receiver, real operand) is caught earlier, by
			// realPartsOf inside Complex's own methods, and never reaches a table.
			table[dstKind] = func(p T, o Number) Number {
				return NewComplex(realComplexOp(NumberToFloat64(p), o.(*Complex).Value))
			}
		case srcKind == KindFloat && lubNeedsGuard:
			// Receiver is Float, might have Inf/NaN.
			lubIsComplex := lubKind == KindBigComplex
			table[dstKind] = func(p T, o Number) Number {
				if isSpecialFloat(any(p).(*Float)) {
					if lubIsComplex {
						// Return BigComplex so the imaginary part of the
						// BigComplex operand is preserved (fix for #362).
						z := complex128Op(NumberToComplex128Lossy(p), NumberToComplex128Lossy(o))
						return NewBigComplexFromBigFloats(
							NewBigFloatFromFloat64(real(z)),
							NewBigFloatFromFloat64(imag(z)),
						)
					}
					return NewFloat(float64Op(NumberToFloat64(p), NumberToFloat64(o)))
				}
				return applyOp(promSrc(p), promDst(o))
			}
		case dstKind == KindFloat && lubNeedsGuard:
			// Operand is Float, might have Inf/NaN.
			lubIsComplex := lubKind == KindBigComplex
			table[dstKind] = func(p T, o Number) Number {
				if isSpecialFloat(o.(*Float)) {
					if lubIsComplex {
						z := complex128Op(NumberToComplex128Lossy(p), NumberToComplex128Lossy(o))
						return NewBigComplexFromBigFloats(
							NewBigFloatFromFloat64(real(z)),
							NewBigFloatFromFloat64(imag(z)),
						)
					}
					return NewFloat(float64Op(NumberToFloat64(p), NumberToFloat64(o)))
				}
				return applyOp(promSrc(p), promDst(o))
			}
		default:
			table[dstKind] = func(p T, o Number) Number {
				return applyOp(promSrc(p), promDst(o))
			}
		}
	}
	return table
}

// makeAddDispatch generates a dispatch table for the Add operation.
func makeAddDispatch[T Number](srcKind NumericKind, sameTypeAdd func(T, Number) Number) [numKinds]func(T, Number) Number {
	return makeArithmeticDispatch(srcKind, sameTypeAdd,
		func(a, b Number) Number {
			return a.Add(b)
		},
		func(a, b float64) float64 {
			return a + b
		},
		func(a, b complex128) complex128 {
			return a + b
		},
		realAddComplex,
	)
}

// makeSubtractDispatch generates a dispatch table for the Subtract operation.
func makeSubtractDispatch[T Number](srcKind NumericKind, sameTypeSub func(T, Number) Number) [numKinds]func(T, Number) Number {
	return makeArithmeticDispatch(srcKind, sameTypeSub,
		func(a, b Number) Number {
			return a.Subtract(b)
		},
		func(a, b float64) float64 {
			return a - b
		},
		func(a, b complex128) complex128 {
			return a - b
		},
		realSubtractComplex,
	)
}

// makeMultiplyDispatch generates a dispatch table for the Multiply operation.
func makeMultiplyDispatch[T Number](srcKind NumericKind, sameTypeMul func(T, Number) Number) [numKinds]func(T, Number) Number {
	return makeArithmeticDispatch(srcKind, sameTypeMul,
		func(a, b Number) Number {
			return a.Multiply(b)
		},
		func(a, b float64) float64 {
			return a * b
		},
		func(a, b complex128) complex128 {
			return a * b
		},
		realMultiplyComplex,
	)
}

// makeDivideDispatch generates a dispatch table for the Divide operation.
// Unlike the other arithmetic dispatchers, Divide returns (Number, error)
// because division by exact zero is a runtime error, not a panic.
func makeDivideDispatch[T Number](
	srcKind NumericKind,
	sameTypeDiv func(T, Number) (Number, error),
) [numKinds]func(T, Number) (Number, error) {
	ensurePromotionInit()
	var table [numKinds]func(T, Number) (Number, error)
	table[srcKind] = sameTypeDiv
	for dstKind := range numKinds {
		if dstKind == srcKind {
			continue
		}
		lubKind := promotionTable[srcKind][dstKind]
		promSrc := promoter[srcKind][lubKind]
		promDst := promoter[dstKind][lubKind]

		// IEEE 754 special-value guard: when Float is the receiver and the LUB
		// goes beyond float64/complex128, short-circuit for Inf/NaN values.
		lubNeedsGuard := lubKind != KindFloat && lubKind != KindComplex

		switch {
		case dstKind == KindComplex && lubKind == KindComplex:
			// A REAL dividend divided by a Complex divisor. See the twin case in
			// makeArithmeticDispatch: promoting the dividend into complex128 invents a
			// +0.0 imaginary part it does not have, and the sign of a signed-zero
			// component dies. realDivideComplex keeps the dividend real.
			table[dstKind] = func(p T, o Number) (Number, error) {
				return NewComplex(realDivideComplex(NumberToFloat64(p), o.(*Complex).Value)), nil
			}
		case srcKind == KindFloat && lubNeedsGuard:
			// Receiver is Float, might have Inf/NaN.
			lubIsComplex := lubKind == KindBigComplex
			table[dstKind] = func(p T, o Number) (Number, error) {
				if isSpecialFloat(any(p).(*Float)) {
					if lubIsComplex {
						z := NumberToComplex128Lossy(p) / NumberToComplex128Lossy(o)
						return NewBigComplexFromBigFloats(
							NewBigFloatFromFloat64(real(z)),
							NewBigFloatFromFloat64(imag(z)),
						), nil
					}
					return NewFloat(NumberToFloat64(p) / NumberToFloat64(o)), nil
				}
				return promSrc(p).Divide(promDst(o))
			}
		case dstKind == KindFloat && lubNeedsGuard:
			// Operand is Float, might have Inf/NaN.
			lubIsComplex := lubKind == KindBigComplex
			table[dstKind] = func(p T, o Number) (Number, error) {
				if isSpecialFloat(o.(*Float)) {
					if lubIsComplex {
						z := NumberToComplex128Lossy(p) / NumberToComplex128Lossy(o)
						return NewBigComplexFromBigFloats(
							NewBigFloatFromFloat64(real(z)),
							NewBigFloatFromFloat64(imag(z)),
						), nil
					}
					return NewFloat(NumberToFloat64(p) / NumberToFloat64(o)), nil
				}
				return promSrc(p).Divide(promDst(o))
			}
		default:
			table[dstKind] = func(p T, o Number) (Number, error) {
				return promSrc(p).Divide(promDst(o))
			}
		}
	}
	return table
}

// makeLessThanDispatch generates a dispatch table for the LessThan operation.
// The same-kind entry is the type's own hand-written comparison; every
// cross-kind entry defers to CompareNumbers (compare.go).
//
// This USED to promote both operands to a lossless-lattice LUB drawn from a
// comparisonTable and call that kind's LessThan. The lattice was not lossless:
// exact × Float landed on BigFloat, whose DefaultBigFloatPrecision is 256, so an
// exact operand needing 301 significant bits was rounded on the way in and
// trichotomy failed --
//
//	(let ((f (expt 2.0 300)) (a (+ (expt 2 300) 1)))
//	  (list (< f a) (> f a) (= f a)))   =>  (#f #f #f)
//
// -- with <= and >= both #t at the same time. The kernel reaches a common domain
// by lifting the INEXACT operand to its exact rational instead, which is a
// direction that always exists, so there is no LUB to get wrong and no table to
// keep honest.
func makeLessThanDispatch[T Number](
	srcKind NumericKind,
	sameTypeLT func(T, Number) bool,
) [numKinds]func(T, Number) bool {
	var table [numKinds]func(T, Number) bool
	table[srcKind] = sameTypeLT
	crossKind := func(p T, o Number) bool {
		return crossKindLessThan(p, o)
	}
	for dstKind := range numKinds {
		if dstKind == srcKind {
			continue
		}
		table[dstKind] = crossKind
	}
	return table
}
