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
)

// promotionTable maps (kindA, kindB) → resultKind, the least upper bound (LUB)
// in the numeric promotion lattice. The table is symmetric:
// promotionTable[a][b] == promotionTable[b][a] for all a, b.
//
// The lattice is NOT linear. It has two independent dimensions — precision
// and complexity — forming a partial order:
//
//	Exact chain:    Integer → BigInteger → Rational
//	                    ↓          ↓           ↓
//	Inexact real:   BigFloat ← BigFloat ← BigFloat
//	                    ↑
//	Float native:   Float → BigFloat
//	                  ↓
//	Complex:        Complex → BigComplex ← BigFloat
//	                              ↑
//	                    Integer/BigInt/Rational (exact parts)
//
// NO LOSSY PROMOTIONS: exact types (Integer, BigInteger, Rational) never
// promote to float64 or complex128. They go to BigFloat (meeting inexact reals)
// or BigComplex (meeting complex). BigFloat never goes to complex128.
//
// A previous linear tower (TowerAdd, etc.) was prototyped and abandoned because
// it forced exact reals through float64 on the way to Complex, destroying
// exactness. See docs/dev/NUMERIC_TOWER.md.
var promotionTable [numKinds][numKinds]NumericKind

// promoter maps (srcKind, resultKind) → conversion function.
// promoter[src][dst](n) converts a Number of kind src to a Number of kind dst.
// Only entries reachable via promotionTable are populated; others are nil.
// All conversions are lossless — no float64 truncation of exact types.
//
// Diagonal entries (src == dst) are identity functions.
var promoter [numKinds][numKinds]func(Number) Number

var promotionOnce sync.Once

// ensurePromotionInit lazily initializes the promotion tables. Called
// by the dispatch generators (makeArithmeticDispatch, makeLessThanDispatch,
// makeCompareDispatch) which may run from type-file init() functions
// before promotion.go's own init().
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
//  2. Exact×InexactReal (×Float, ×BigFloat): always BigFloat
//  3. Anything×Complex: exact→BigComplex; Float+Complex→Complex; BigFloat+Complex→BigComplex
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

	// Zone 2: Exact × Inexact Real → BigFloat
	// Exact types never truncate to float64. BigFloat preserves precision (256-bit).
	for _, exact := range []NumericKind{I, BI, R} {
		set(exact, F, BF)
		set(exact, BF, BF)
	}

	// Float × Float, Float × BigFloat, BigFloat × BigFloat
	set(F, F, F)
	set(F, BF, BF)
	set(BF, BF, BF)

	// Zone 3: Anything × Complex
	// Exact types → BigComplex (preserve exactness, never go through complex128).
	for _, exact := range []NumericKind{I, BI, R} {
		set(exact, C, BC)
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

// initPromoters populates the lossless conversion functions.
// promoter[src][dst] converts a Number of kind src to kind dst.
// Only entries reachable from promotionTable are populated.
func initPromoters() {
	// Identity promoters (diagonal): every type promotes to itself.
	for k := range numKinds {
		promoter[k][k] = func(n Number) Number {
			return n
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
		return &BigFloat{value: new(big.Float).SetPrec(DefaultBigFloatPrecision).SetFloat64(p.Value)}
	}

	// Float → Complex
	promoter[KindFloat][KindComplex] = func(n Number) Number {
		p := n.(*Float)
		return NewComplex(complex(p.Value, 0))
	}

	// Float → BigComplex
	promoter[KindFloat][KindBigComplex] = func(n Number) Number {
		p := n.(*Float)
		return NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(p.Value),
			NewBigFloatFromFloat64(0),
		)
	}

	// BigFloat → BigComplex
	promoter[KindBigFloat][KindBigComplex] = func(n Number) Number {
		p := n.(*BigFloat)
		return NewBigComplex(p, NewBigFloatFromFloat64(0))
	}

	// Complex → BigComplex
	promoter[KindComplex][KindBigComplex] = func(n Number) Number {
		p := n.(*Complex)
		return NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(p.Value)),
			NewBigFloatFromFloat64(imag(p.Value)),
		)
	}
}

// PromotionResultKind returns the result type when operands of kindA and kindB
// are combined in an arithmetic operation. The result is the least upper bound
// (LUB) in the promotion lattice — symmetric and lossless.
func PromotionResultKind(kindA, kindB NumericKind) NumericKind {
	return promotionTable[kindA][kindB]
}

// Promote converts a Number to the target NumericKind using the lossless
// promoter table. Panics if no promotion path exists (indicates a bug in
// the promotion table — all reachable paths should be populated).
func Promote(n Number, target NumericKind) Number {
	src := n.Kind()
	if src == target {
		return n
	}
	fn := promoter[src][target]
	if fn == nil {
		panic(ErrNotANumber)
	}
	return fn(n)
}

// isSpecialFloat reports whether a Float holds IEEE 754 Inf or NaN.
// BigFloat cannot represent these values (SetFloat64 panics on NaN,
// and arithmetic on BigFloat Inf is undefined).
func isSpecialFloat(f *Float) bool {
	return math.IsInf(f.Value, 0) || math.IsNaN(f.Value)
}

// numberToFloat64 converts any Number to float64. Precision loss is acceptable
// because the result is only used in the IEEE 754 Inf/NaN guard, where the
// special value dominates the result regardless of the other operand's magnitude.
func numberToFloat64(n Number) float64 {
	switch v := n.(type) {
	case *Integer:
		return float64(v.Value)
	case *BigInteger:
		return float64FromBigInt(v.value)
	case *Float:
		return v.Value
	case *BigFloat:
		f, _ := v.value.Float64()
		return f
	case *Rational:
		return v.Float64()
	case *Complex:
		return real(v.Value)
	case *BigComplex:
		return toBigFloat(v.real).Float64()
	}
	panic(ErrNotANumber)
}

// cmpFloat64 compares two float64 values, returning -1, 0, or 1.
// Used by makeCompareDispatch for the IEEE 754 special-value guard.
func cmpFloat64(a, b float64) int {
	if a < b {
		return -1
	} else if a > b {
		return 1
	}
	return 0
}

// makeAddDispatch generates a dispatch table for the Add operation.
// The same-type entry uses the hand-written sameTypeAdd (preserving
// the hot-path performance — e.g., Integer+Integer overflow detection).
// Cross-type entries promote both operands to the LUB type via the
// promotion table and delegate to the LUB type's Add method.
//
// IEEE 754 guard: when Float is involved and the LUB is BigFloat or
// BigComplex, Inf/NaN cannot be promoted. For these cases the generated
// dispatch checks for special values and returns the Float directly
// (any finite ± Inf = Inf, any ± NaN = NaN).
// makeArithmeticDispatch generates a dispatch table for an arithmetic
// operation (Add, Subtract, Multiply, Divide). The same-type entry uses
// the hand-written sameTypeOp (preserving hot-path performance — e.g.,
// Integer+Integer overflow detection). Cross-type entries promote both
// operands to the LUB type via the promotion table and delegate to the
// LUB type's operation via applyOp.
//
// IEEE 754 guard: when Float is involved and the LUB is BigFloat or
// BigComplex, Inf/NaN cannot be promoted losslessly. For these cases
// the generated dispatch checks for special values and falls back to
// float64 arithmetic via float64Op(numberToFloat64(src), numberToFloat64(dst)).
// This is correct because the Inf/NaN dominates the result.
func makeArithmeticDispatch[T Number](
	srcKind NumericKind,
	sameTypeOp func(T, Number) Number,
	applyOp func(Number, Number) Number,
	float64Op func(float64, float64) float64,
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

		// IEEE 754 special-value guard: BigFloat/BigComplex cannot handle Inf/NaN.
		// When Float participates and the LUB goes beyond float64/complex128,
		// check for special values and short-circuit via float64 arithmetic.
		lubNeedsGuard := lubKind != KindFloat && lubKind != KindComplex

		switch {
		case srcKind == KindFloat && lubNeedsGuard:
			// Receiver is Float, might have Inf/NaN.
			table[dstKind] = func(p T, o Number) Number {
				if isSpecialFloat(any(p).(*Float)) {
					return NewFloat(float64Op(numberToFloat64(p), numberToFloat64(o)))
				}
				return applyOp(promSrc(p), promDst(o))
			}
		case dstKind == KindFloat && lubNeedsGuard:
			// Operand is Float, might have Inf/NaN.
			table[dstKind] = func(p T, o Number) Number {
				if isSpecialFloat(o.(*Float)) {
					return NewFloat(float64Op(numberToFloat64(p), numberToFloat64(o)))
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
	)
}

// makeDivideDispatch generates a dispatch table for the Divide operation.
func makeDivideDispatch[T Number](srcKind NumericKind, sameTypeDiv func(T, Number) Number) [numKinds]func(T, Number) Number {
	return makeArithmeticDispatch(srcKind, sameTypeDiv,
		func(a, b Number) Number {
			return a.Divide(b)
		},
		func(a, b float64) float64 {
			return a / b
		},
	)
}

// makeLessThanDispatch generates a dispatch table for the LessThan operation.
// Cross-type entries promote both operands to the LUB type and compare via
// the LUB type's LessThan method.
//
// IEEE 754 guard: when Float has Inf/NaN and the LUB is BigFloat/BigComplex,
// falls back to float64 comparison.
func makeLessThanDispatch[T Number](
	srcKind NumericKind,
	sameTypeLT func(T, Number) bool,
) [numKinds]func(T, Number) bool {
	ensurePromotionInit()
	var table [numKinds]func(T, Number) bool
	table[srcKind] = sameTypeLT
	for dstKind := range numKinds {
		if dstKind == srcKind {
			continue
		}
		lubKind := promotionTable[srcKind][dstKind]
		promSrc := promoter[srcKind][lubKind]
		promDst := promoter[dstKind][lubKind]

		lubNeedsGuard := lubKind != KindFloat && lubKind != KindComplex

		switch {
		case srcKind == KindFloat && lubNeedsGuard:
			table[dstKind] = func(p T, o Number) bool {
				if isSpecialFloat(any(p).(*Float)) {
					return numberToFloat64(p) < numberToFloat64(o)
				}
				return promSrc(p).LessThan(promDst(o))
			}
		case dstKind == KindFloat && lubNeedsGuard:
			table[dstKind] = func(p T, o Number) bool {
				if isSpecialFloat(o.(*Float)) {
					return numberToFloat64(p) < numberToFloat64(o)
				}
				return promSrc(p).LessThan(promDst(o))
			}
		default:
			table[dstKind] = func(p T, o Number) bool {
				return promSrc(p).LessThan(promDst(o))
			}
		}
	}
	return table
}

// makeCompareDispatch generates a dispatch table for the Compare operation.
// Cross-type entries promote both operands to the LUB type and compare via
// the LUB type's Compare method.
//
// IEEE 754 guard: when Float has Inf/NaN and the LUB is BigFloat/BigComplex,
// falls back to float64 comparison via cmpFloat64.
func makeCompareDispatch[T Number](
	srcKind NumericKind,
	sameTypeCmp func(T, Number) int,
) [numKinds]func(T, Number) int {
	ensurePromotionInit()
	var table [numKinds]func(T, Number) int
	table[srcKind] = sameTypeCmp
	for dstKind := range numKinds {
		if dstKind == srcKind {
			continue
		}
		lubKind := promotionTable[srcKind][dstKind]
		promSrc := promoter[srcKind][lubKind]
		promDst := promoter[dstKind][lubKind]

		lubNeedsGuard := lubKind != KindFloat && lubKind != KindComplex

		switch {
		case srcKind == KindFloat && lubNeedsGuard:
			table[dstKind] = func(p T, o Number) int {
				if isSpecialFloat(any(p).(*Float)) {
					return cmpFloat64(numberToFloat64(p), numberToFloat64(o))
				}
				return promSrc(p).Compare(promDst(o))
			}
		case dstKind == KindFloat && lubNeedsGuard:
			table[dstKind] = func(p T, o Number) int {
				if isSpecialFloat(o.(*Float)) {
					return cmpFloat64(numberToFloat64(p), numberToFloat64(o))
				}
				return promSrc(p).Compare(promDst(o))
			}
		default:
			table[dstKind] = func(p T, o Number) int {
				return promSrc(p).Compare(promDst(o))
			}
		}
	}
	return table
}
