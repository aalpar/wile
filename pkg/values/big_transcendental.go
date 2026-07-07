package values

import (
	"math/big"
	"sync"
)

// Arbitrary-precision arctangent and π on *big.Float. Go's math/big provides a
// native Sqrt but no transcendental functions, so these are built from scratch:
// argument-reduction Taylor for atan, Machin's formula for π. They exist so that
// BigComplex.Phase and the math extension's atan/angle big-operand paths deliver
// the precision the *BigFloat type promises instead of truncating to float64
// (which collapses large-magnitude operands with a finite ratio to a wrong angle
// once both components saturate to ±Inf).

// atanWorkPrec returns a working precision with headroom above prec, guarding the
// argument-reduction and series-accumulation steps against cancellation. The
// proportional term keeps the margin sound above the 256-bit default.
func atanWorkPrec(prec uint) uint {
	return prec + prec/16 + 32
}

var (
	bigPiCacheMu sync.Mutex
	bigPiCache   = map[uint]*big.Float{}
)

// piAtPrec returns π rounded to prec bits via Machin's formula
// π = 16·atan(1/5) − 4·atan(1/239). Both arguments are < 1, so atanSeries never
// needs π and there is no recursion back into piAtPrec. Results are cached per
// precision; a defensive copy is returned so callers cannot mutate the cache.
func piAtPrec(prec uint) *big.Float {
	bigPiCacheMu.Lock()
	defer bigPiCacheMu.Unlock()
	p, ok := bigPiCache[prec]
	if ok {
		return new(big.Float).Set(p)
	}
	wp := prec + 32
	one := new(big.Float).SetPrec(wp).SetInt64(1)
	fifth := new(big.Float).SetPrec(wp).Quo(one, new(big.Float).SetPrec(wp).SetInt64(5))
	inv239 := new(big.Float).SetPrec(wp).Quo(one, new(big.Float).SetPrec(wp).SetInt64(239))

	a := atanSeries(fifth, wp)
	b := atanSeries(inv239, wp)

	sixteenA := new(big.Float).SetPrec(wp).Mul(a, new(big.Float).SetPrec(wp).SetInt64(16))
	fourB := new(big.Float).SetPrec(wp).Mul(b, new(big.Float).SetPrec(wp).SetInt64(4))
	pi := new(big.Float).SetPrec(wp).Sub(sixteenA, fourB)

	result := new(big.Float).SetPrec(prec).Set(pi)
	bigPiCache[prec] = new(big.Float).Set(result)
	return result
}

// atanSeries computes atan(x) for 0 ≤ x ≤ 1 at precision wp. It halves the
// argument via atan(x) = 2·atan(x/(1+√(1+x²))) until it is below 2⁻⁸ (a bounded
// number of steps since x starts ≤ 1), sums the Taylor series
// t − t³/3 + t⁵/5 − …, then scales back by 2ʳ.
func atanSeries(x *big.Float, wp uint) *big.Float {
	if x.Sign() == 0 {
		return new(big.Float).SetPrec(wp)
	}
	one := new(big.Float).SetPrec(wp).SetInt64(1)
	// threshold = 2⁻⁸
	threshold := new(big.Float).SetPrec(wp).SetMantExp(one, -8)

	t := new(big.Float).SetPrec(wp).Set(x)
	r := 0
	for t.Cmp(threshold) > 0 {
		t2 := new(big.Float).SetPrec(wp).Mul(t, t)
		rooted := new(big.Float).SetPrec(wp).Sqrt(new(big.Float).SetPrec(wp).Add(one, t2))
		denom := new(big.Float).SetPrec(wp).Add(one, rooted)
		t = new(big.Float).SetPrec(wp).Quo(t, denom)
		r++
	}

	// Taylor: sum = Σ (-1)ⁿ t^(2n+1)/(2n+1), n from 0.
	sum := new(big.Float).SetPrec(wp).Set(t)
	t2 := new(big.Float).SetPrec(wp).Mul(t, t)
	power := new(big.Float).SetPrec(wp).Set(t) // t^(2n+1)
	negligibleExp := -int(wp) - 4
	for n := 1; ; n++ {
		power = new(big.Float).SetPrec(wp).Mul(power, t2)
		frac := new(big.Float).SetPrec(wp).Quo(power, new(big.Float).SetPrec(wp).SetInt64(int64(2*n+1)))
		if n%2 == 1 {
			sum.Sub(sum, frac)
		} else {
			sum.Add(sum, frac)
		}
		if frac.Sign() == 0 || frac.MantExp(nil) < negligibleExp {
			break
		}
	}

	if r > 0 {
		scale := new(big.Float).SetPrec(wp).SetMantExp(one, r) // 2ʳ
		sum.Mul(sum, scale)
	}
	return sum
}

// atanAt computes atan(x) over the full real line at precision wp. For |x| > 1 it
// uses atan(x) = π/2 − atan(1/x) to keep atanSeries in its (0,1] domain.
func atanAt(x *big.Float, wp uint) *big.Float {
	if x.Sign() == 0 {
		return new(big.Float).SetPrec(wp)
	}
	neg := x.Sign() < 0
	ax := new(big.Float).SetPrec(wp).Abs(x)
	one := new(big.Float).SetPrec(wp).SetInt64(1)

	var q *big.Float
	if ax.Cmp(one) > 0 {
		inv := new(big.Float).SetPrec(wp).Quo(one, ax)
		inner := atanSeries(inv, wp)
		halfPi := new(big.Float).SetPrec(wp).Quo(piAtPrec(wp), new(big.Float).SetPrec(wp).SetInt64(2))
		q = new(big.Float).SetPrec(wp).Sub(halfPi, inner)
	} else {
		q = atanSeries(ax, wp)
	}
	if neg {
		q.Neg(q)
	}
	return q
}

// BigPi returns π rounded to prec bits.
func BigPi(prec uint) *big.Float {
	return piAtPrec(prec)
}

// BigAtan returns the arctangent of x rounded to prec bits.
func BigAtan(x *big.Float, prec uint) *big.Float {
	wp := atanWorkPrec(prec)
	xw := new(big.Float).SetPrec(wp).Set(x)
	return new(big.Float).SetPrec(prec).Set(atanAt(xw, wp))
}

// BigAtan2 returns atan2(y, x) — the angle of the point (x, y) — rounded to prec
// bits. The quadrant adjustment runs on the big-precision ratio y/x, which stays
// finite for any finite operands (big.Float's exponent range is ~±10⁹), so
// large-magnitude components no longer overflow to a wrong angle.
func BigAtan2(y, x *big.Float, prec uint) *big.Float {
	wp := atanWorkPrec(prec)
	yw := new(big.Float).SetPrec(wp).Set(y)
	xw := new(big.Float).SetPrec(wp).Set(x)
	pi := piAtPrec(wp)
	two := new(big.Float).SetPrec(wp).SetInt64(2)

	var q *big.Float
	switch {
	case xw.Sign() > 0:
		q = atanAt(new(big.Float).SetPrec(wp).Quo(yw, xw), wp)
	case xw.Sign() < 0 && yw.Sign() >= 0:
		q = new(big.Float).SetPrec(wp).Add(atanAt(new(big.Float).SetPrec(wp).Quo(yw, xw), wp), pi)
	case xw.Sign() < 0: // yw.Sign() < 0
		q = new(big.Float).SetPrec(wp).Sub(atanAt(new(big.Float).SetPrec(wp).Quo(yw, xw), wp), pi)
	case yw.Sign() > 0: // xw == 0
		q = new(big.Float).SetPrec(wp).Quo(pi, two)
	case yw.Sign() < 0: // xw == 0
		q = new(big.Float).SetPrec(wp).Neg(new(big.Float).SetPrec(wp).Quo(pi, two))
	default: // (0, 0)
		q = new(big.Float).SetPrec(wp)
	}
	return new(big.Float).SetPrec(prec).Set(q)
}
