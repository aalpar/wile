// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//	http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package values

import (
	"github.com/aalpar/wile/pkg/werr"
)

// The exact-zero rule.
//
// PREMISE: exactness, not magnitude, decides.
//
//   - An exact 0 is a MATHEMATICAL zero: the annihilator of *, the identity of +.
//     It has no sign. It is not an IEEE value, so IEEE 754's rules do not govern
//     it -- which is why (* 0 +inf.0) is 0 and not NaN.
//   - An inexact 0.0 is an IEEE float that merely compares equal to zero. It
//     carries a sign bit, and IEEE 754 does govern it.
//
// So n.IsZero() is NEVER a sufficient test. The predicate is always isExactZero.
//
// The rule is a family of transfer functions, one per operation, and it lives here
// as DATA rather than as guards hand-written into each of the seven numeric kinds.
// That is not a tidiness preference. When the family was hand-unrolled across four
// files, the / member simply did not exist, and nobody noticed: a missing member is
// invisible when there is nothing to be missing FROM. As a table, an absent row is
// a hole you have to look at.
//
// Abstract interpretation (Cousot & Cousot 1977): exactness is the two-point
// lattice {exact < inexact} and ordinary contagion is the join. The exact-zero
// cases are STRONG UPDATES -- strictly more precise than the join -- licensed
// because the mathematical result is known exactly. See BIBLIOGRAPHY.md,
// "Exactness as Abstract Interpretation".

// isExactZero reports whether n is an exact zero, i.e. a mathematical zero rather
// than an IEEE value that happens to compare equal to zero.
//
// This is the ONE predicate. A bare n.IsZero() elsewhere in this package is either
// a bug or a deliberately different question -- see the isReal loss-signal in
// bigComplexToFloat64WithAccuracy, which asks "did I drop information", not "is
// this a mathematical zero", and is correct to ignore exactness.
func isExactZero(n Number) bool {
	return n.IsZero() && n.IsExact()
}

// zeroAction is what the rule does when one operand is an exact zero.
type zeroAction uint8

const (
	zeroFallThrough    zeroAction = iota // the rule does not apply; normal dispatch owns it
	zeroYieldExactZero                   // the result is an exact 0
	zeroYieldOther                       // the result is the OTHER operand, untouched
	zeroNegateOther                      // the result is the negation of the other operand
	zeroRaise                            // division by an exact zero: raise
)

// zeroOp indexes exactZeroTable.
type zeroOp uint8

const (
	zeroAdd zeroOp = iota
	zeroSub
	zeroMul
	zeroDiv
	numZeroOps
)

// exactZeroTable is the exact-zero rule, as data. One row per operation; the two
// columns are the action to take when the LEFT operand is an exact zero and when
// the RIGHT one is.
//
// Every row is pinned to Chez and Racket, which agree on all of them:
//
//	(* 0 +inf.0) => 0        (* 0 +nan.0) => 0        annihilates unconditionally
//	(+ 0 -0.0)   => -0.0     (+ -0.0 0)   => -0.0     identity: the operand is UNTOUCHED
//	(- 0 -0.0)   => 0.0      (- -0.0 0)   => -0.0     ASYMMETRIC: negate vs identity
//	(/ 0 +nan.0) => 0        (/ 1 0)      => raises   dividend annihilates; divisor raises
//
// The two non-obvious entries:
//
// zeroSub is ASYMMETRIC because subtraction is not commutative. (- x 0) is x, but
// (- 0 x) is -x. Reading that asymmetry off a table is the point; the last time it
// lived in prose, the negation was reasoned about correctly in a comment and then
// left out of the code.
//
// zeroDiv's left column is an annihilation, structurally identical to zeroMul's:
// an exact 0 divided by ANYTHING is exactly 0, including by NaN and by an inexact
// zero. (/ 0 0.0) is 0 in both oracles, NOT NaN -- the strong update overrides IEEE
// for the same reason (* 0 +inf.0) does.
var exactZeroTable = [numZeroOps]struct{ left, right zeroAction }{
	zeroAdd: {left: zeroYieldOther, right: zeroYieldOther},
	zeroSub: {left: zeroNegateOther, right: zeroYieldOther},
	zeroMul: {left: zeroYieldExactZero, right: zeroYieldExactZero},
	zeroDiv: {left: zeroYieldExactZero, right: zeroRaise},
}

// init enforces totality.
//
// zeroFallThrough is iota's zero value, so an operation added to the zeroOp enum
// but not to exactZeroTable would silently default to {zeroFallThrough,
// zeroFallThrough} and its rule would never fire. That is EXACTLY the failure this
// table exists to prevent -- a missing member that nothing makes you look at -- so
// silence is not an option. An operation with genuinely no exact-zero rule must say
// so, explicitly, in both columns.
//
// Mirrors registerNumericSpec, which likewise panics on an incomplete spec.
func init() {
	for op := range exactZeroTable {
		r := exactZeroTable[op]
		if r.left == zeroFallThrough && r.right == zeroFallThrough {
			panic(werr.WrapForeignErrorf(werr.ErrNotANumber,
				"exactZeroTable: op %d declares no exact-zero rule in either column; "+
					"declare zeroFallThrough explicitly in both if it genuinely has none", op))
		}
	}
}

// exactZeroRule applies the exact-zero rule for a binary arithmetic operation.
//
// Returns (result, true, nil) when the rule fires, (nil, true, err) when it fires
// and raises, and (nil, false, nil) to fall through to normal dispatch.
//
// The RIGHT operand is consulted FIRST, and that ordering is load-bearing: (/ 0 0)
// must RAISE rather than return an exact 0, so the divisor rule has to beat the
// dividend rule. Both oracles raise on it. The ordering is safe for the other three
// operations -- (+ 0 0), (- 0 0) and (* 0 0) give the same answer whichever operand
// is consulted first -- so right-first is a single invariant rather than a
// per-operation special case.
//
// Only zeroDiv can raise; the error return is unused by the other three. That is
// the price of keeping the four rules in one place, and it is worth paying: the
// last time division's rule lived somewhere else, it went missing entirely.
func exactZeroRule(op zeroOp, a, b Number) (Number, bool, error) {
	r := exactZeroTable[op]
	if isExactZero(b) {
		return applyZeroAction(r.right, a)
	}
	if isExactZero(a) {
		return applyZeroAction(r.left, b)
	}
	return nil, false, nil
}

// applyZeroAction performs one cell of the table. other is the operand that is NOT
// the exact zero -- the one the action acts upon.
func applyZeroAction(action zeroAction, other Number) (Number, bool, error) {
	switch action {
	case zeroYieldExactZero:
		return NewInteger(0), true, nil
	case zeroYieldOther:
		// UNTOUCHED, not "added to zero". IEEE addition of +0.0 would flip a -0.0
		// to +0.0; handing the operand back preserves its sign and its exactness.
		return other, true, nil
	case zeroNegateOther:
		return other.Negate(), true, nil
	case zeroRaise:
		return nil, true, werr.WrapForeignErrorf(werr.ErrDivisionByZero,
			"exactZeroRule: division by an exact zero")
	case zeroFallThrough:
		return nil, false, nil
	}
	panic(werr.WrapForeignErrorf(werr.ErrNotANumber,
		"applyZeroAction: unknown zeroAction %d", action))
}
