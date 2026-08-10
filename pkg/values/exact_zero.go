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
// Abstract interpretation (Cousot & Cousot 1977). Exactness is tracked in the
// two-point lattice {exact < inexact}:
//
//	α: Number → {exact, inexact}                    (abstraction function)
//	Transfer for most ops: α(a op b) = α(a) ⊔ α(b)  (join: inexact wins)
//	Transfer for (* 0 x) and (/ 0 x): α(result) = exact  if α(0) = exact
//
// The exact-zero cases are STRONG UPDATES: the transfer function returns a result
// strictly more precise than the naive join, because the mathematical result is
// known exactly. The update is licensed by the EXACTNESS of the zero ALONE --
// finiteness of the other operand is irrelevant, because an exact 0 is not an IEEE
// value. That is why (* 0 +inf.0) is 0 rather than NaN, and equally why
// (/ 0 +nan.0) is 0.
//
//	Constrains:     all arithmetic dispatch closures (must respect contagion),
//	                Simplify (must not change exactness class).
//	Constrained by: the promotion lattice (must be monotone w.r.t. the
//	                exactness ordering).
//
// An INEXACT zero must not short-circuit anywhere: contagion requires the result be
// inexact, and IEEE governs the sign. (* 5 0.0) is 0.0, not 0.
//
// See BIBLIOGRAPHY.md, "Exactness as Abstract Interpretation".

// SCOPE: the rule's operands are whole SCHEME NUMBERS, never the components of a
// complex.
//
// This is not pedantry, it is the sharp edge. Exactness contagion is a property of
// the OPERAND PAIR -- α(a op b) = α(a) ⊔ α(b) -- so the strong updates are licensed
// only when the exact zero IS an operand. Apply (/ 0 x) => exact 0 to a component
// and you get an exact zero sitting inside an inexact number: (/ 0+1i 1.5) comes
// back as 0+0.666i, whose real part is exact while the number is not. Both oracles
// say 0.0+0.666i. Such a value even breaks write/read invariance, since it prints in
// a syntax that reads back as something else.
//
// Complex arithmetic that divides part-wise therefore has to re-impose contagion
// afterwards -- see contagionOverParts, which exists solely because the parts are
// routed through the public Number methods, and those methods rightly apply the rule
// to what they are handed.

// WHICH KINDS CAN BE THE EXACT ZERO: only Integer, BigInteger, Rational, and a
// BigComplex with exact parts. Float, BigFloat and Complex return IsExact() == false
// unconditionally, so a guard on THEIR receiver can never fire.
//
// Those three kinds therefore do NOT carry a receiver-side guard in Add or Subtract,
// and that asymmetry is deliberate rather than an oversight. isExactZero is not
// inlinable (see below), so a guard that can never fire is a real call on a hot path
// bought for nothing. Do not "restore uniformity" by adding them back.
//
// Uniformity was never the safety property anyway -- the conformance test is. If a
// new EXACT kind is added and forgets its guard, the rule WILL fire for it and
// TestExactZeroCallSitesMatchTheTable fails. A dead guard, by contrast, agrees with
// its table row vacuously, so its presence proves nothing and its absence costs
// nothing.

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

// contagionOverParts re-imposes exactness contagion on a complex result whose parts
// were computed by dividing/multiplying them individually.
//
// The part-wise paths hand each component to the PUBLIC Number arithmetic, which
// applies the exact-zero rule to the value it is given -- correctly, for an operand.
// But a component is not an operand. An exact-zero real part divided by an inexact
// real yields an exact 0 by the dividend rule, and that exact 0 is then sitting in a
// number the operand pair says must be wholly inexact.
//
// So: if EITHER operand is inexact, every component of the result is inexact. That
// is just α(a op b) = α(a) ⊔ α(b), applied where the part-wise shortcut skipped it.
//
//	(/ 0+1i 1.5)  =>  0.0+0.666...i   in Chez and Racket, NOT 0+0.666...i
func contagionOverParts(a, b Number, re, im Number) (Number, Number) {
	if a.IsExact() && b.IsExact() {
		return re, im
	}
	return re.ToInexact(), im.ToInexact()
}

// exactZeroEither reports whether EITHER operand is an exact zero -- the annihilation
// question, asked by * and by /'s dividend rule.
//
// It hand-expands isExactZero instead of calling it twice, and that is a deliberate,
// measured specialization rather than the spelling drift this file exists to end.
// isExactZero costs 122 in the inliner's budget (two interface method calls) against
// a budget of 80, so it is NEVER inlined. Calling it twice on the multiply hot path
// therefore costs two real calls where one will do:
//
//	BenchmarkIntegerMultiply   1 call: 3.82 ns/op    2 calls: 4.37 ns/op   (+14%)
//	Gabriel suite (geomean)                                                 (+1.5%)
//
// The two predicates MUST agree. TestExactZeroPredicatesAgree pins that they do, so
// this stays a specialization of one rule rather than a second copy of it.
func exactZeroEither(a, b Number) bool {
	return (a.IsZero() && a.IsExact()) || (b.IsZero() && b.IsExact())
}

// exactZeroDivideAction answers BOTH of division's exact-zero questions in ONE call,
// returning the row of exactZeroTable[zeroDiv] that applies:
//
//	zeroRaise           the DIVISOR is an exact zero -- (/ x 0) is an error
//	zeroYieldExactZero  the DIVIDEND is an exact zero -- (/ 0 x) is exactly 0
//	zeroFallThrough     neither; normal dispatch owns it
//
// The divisor is checked first, and that ordering is the rule: (/ 0 0) RAISES in
// both oracles rather than returning an exact 0.
//
// Same measured motivation as exactZeroEither, and the same caveat. Division's guard
// used to be written inline, so it cost ZERO function calls; replacing it with two
// isExactZero calls (neither inlinable) cost +3.2% on diviter and +3.5% on divrec.
// One call restores that. Returning the action rather than the result lets each kind
// keep its own error message ("Integer.Divide: ...") instead of a generic one.
//
// TestExactZeroCallSitesMatchTheTable pins that this agrees with the table row it
// claims to implement, so this stays a specialisation of the rule, not a copy.
func exactZeroDivideAction(a, b Number) zeroAction {
	if b.IsZero() && b.IsExact() {
		return zeroRaise
	}
	if a.IsZero() && a.IsExact() {
		return zeroYieldExactZero
	}
	return zeroFallThrough
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

// zeroRow is one operation's rule: what to do when the LEFT operand is an exact
// zero, and what to do when the RIGHT one is.
type zeroRow struct {
	left  zeroAction
	right zeroAction
}

// exactZeroTable is the exact-zero rule, as data. One row per operation; the two
// columns are the action to take when the LEFT operand is an exact zero and when
// the RIGHT one is.
//
// WHAT LICENSES EACH ROW, which is not the same question as what the references
// do. R7RS §6.2.6 leaves an implementation latitude here: an exact zero operand
// may yield an exact result. Every row below sits INSIDE that latitude, so none
// of them is obligated by the spec and none of them deviates from it. Chez and
// Racket are the TIE-BREAK within the permitted space, not the authority — a
// distinction the review plugin's conformance-authorities.md draws, and which an
// earlier revision of this comment ("every row is pinned to Chez and Racket")
// collapsed. Keep them apart: an implementation cited as authority is how a
// permitted choice gets mistaken for an obligation, in either direction.
//
// Measured 2026-08-09 against petite 10.x and racket. All three agree on every
// row, and Wile changes nothing here:
//
//	(* 0 +inf.0) => 0        (* 0 +nan.0) => 0        annihilates unconditionally
//	(+ 0 +nan.0) => +nan.0   (- 0 +nan.0) => +nan.0   identity/negate: NaN survives
//	(+ 0 -0.0)   => -0.0     (+ -0.0 0)   => -0.0     identity: the operand is UNTOUCHED
//	(- 0 -0.0)   => 0.0      (- -0.0 0)   => -0.0     ASYMMETRIC: negate vs identity
//	(/ 0 +nan.0) => 0        (/ 1 0)      => raises   dividend annihilates; divisor raises
//
// A 2026-08-07 review filed the (/ 0 +nan.0) row as a defect, quoting R7RS
// §6.2.6 as "an implementation may return an exact zero unless one of the other
// arguments is a NaN". NOT TAKEN, for two reasons that are worth leaving here so
// it is not re-opened a third time: the quoted clause would condemn
// (* 0 +nan.0) identically, which the same review keeps as conformant; and the
// quote could not be verified against the document. Anyone re-opening this needs
// the spec text in hand and an answer for whether it binds * as well as /. If it
// does, this is a three-row change AND a docs/reference/r7rs-differences.md row,
// because it puts Wile on the far side of both references.
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
// zero. (/ 0 0.0) is 0 in both references, NOT NaN -- the strong update overrides
// IEEE for the same reason (* 0 +inf.0) does.
//
// Note which rows the strong update reaches: only the ANNIHILATORS. Add and
// subtract are identity/negate, so their other operand is returned untouched and
// a NaN survives -- which is why (+ 0 +nan.0) is +nan.0 while (* 0 +nan.0) is 0.
// That is the table's shape, not an inconsistency between them.
var exactZeroTable = [numZeroOps]zeroRow{
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
// Mirrors registerNumericSpec, which likewise panics on an incomplete spec -- and,
// like it, factors the check into a function that takes the table as a PARAMETER, so
// a test can call it with a deliberately holed table and prove the guard fires. A
// guard that has only ever run against the good table is a comment.
func init() {
	validateExactZeroTable(exactZeroTable)
}

// validateExactZeroTable panics unless every op declares a rule in at least one
// column. Takes the table as a parameter so tests can feed it crafted bad state;
// validateNumericSpecs (numeric_registry.go) is the same shape for the same reason.
func validateExactZeroTable(table [numZeroOps]zeroRow) {
	for op := range table {
		r := table[op]
		if r.left == zeroFallThrough && r.right == zeroFallThrough {
			panic(werr.WrapForeignErrorf(werr.ErrNumericRegistry,
				"validateExactZeroTable: op %d declares no exact-zero rule in either "+
					"column; declare zeroFallThrough explicitly in both if it genuinely "+
					"has none", op))
		}
	}
}

// exactZeroRule applies the exact-zero rule for a binary arithmetic operation.
//
// THIS IS THE EXECUTABLE SPECIFICATION, NOT THE HOT PATH. The seven numeric kinds
// deliberately hand-inline their rows instead of calling it, because isExactZero
// costs 122 against the inliner's budget of 80 and is never inlined -- so routing
// Add through a wrapper that then calls it adds a second real call and a three-word
// return, which measured +1.5% geomean on the Gabriel suite. Hand-inlined, the call
// sites are structurally identical to the guards they replaced (+0.65% geomean,
// inside the noise band).
//
// The conformance test (TestExactZeroCallSitesMatchTheTable) is what makes that
// safe: it drives every kind's PUBLIC Add/Subtract/Multiply/Divide and asserts the
// result equals what this reference says, for every operand pairing. So the table
// remains the single source of truth for the rule, and a call site that disagrees
// with its row is a test failure rather than a bug someone finds a year later.
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

// applyZeroAction performs one cell of exactZeroTable. other is the operand that is
// NOT the exact zero -- the one the action acts upon.
func applyZeroAction(action zeroAction, other Number) (Number, bool, error) {
	switch action {
	case zeroYieldExactZero:
		return NewInteger(0), true, nil
	case zeroYieldOther:
		// UNTOUCHED, not "added to zero". IEEE addition of +0.0 would flip a -0.0 to
		// +0.0; handing the operand back preserves its sign AND its exactness.
		return other, true, nil
	case zeroNegateOther:
		return other.Negate(), true, nil
	case zeroRaise:
		return nil, true, werr.WrapForeignErrorf(werr.ErrDivisionByZero,
			"exactZeroRule: division by an exact zero")
	case zeroFallThrough:
		return nil, false, nil
	}
	panic(werr.WrapForeignErrorf(werr.ErrNumericRegistry,
		"applyZeroAction: unknown zeroAction %d", action))
}
