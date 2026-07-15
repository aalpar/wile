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

// bigPiCache memoizes π per precision, LOCK-FREE. This is engine machinery: no
// user-visible handle exists to serialize on (any thread doing high-precision
// math implicitly triggers it), so the engine keeps it safe itself rather than
// pushing it to the user. Cached values are immutable and a concurrent duplicate
// compute is harmless (identical result), so a sync.Map replaces the former
// mutex + map; callers still receive a defensive copy.
var bigPiCache sync.Map // uint prec -> *big.Float (immutable)

// piAtPrec returns π rounded to prec bits via Machin's formula
// π = 16·atan(1/5) − 4·atan(1/239). Both arguments are < 1, so atanSeries never
// needs π and there is no recursion back into piAtPrec. Results are cached per
// precision; a defensive copy is returned so callers cannot mutate the cache.
func piAtPrec(prec uint) *big.Float {
	cached, ok := bigPiCache.Load(prec)
	if ok {
		return new(big.Float).Set(cached.(*big.Float))
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
	bigPiCache.Store(prec, result)
	return new(big.Float).Set(result)
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
//
// atan(±0) is ±0: the sign is CARRIED THROUGH, not discarded. A fresh big.Float is
// +0, so returning one for a -0.0 input silently flips the sign -- which is exactly
// the trap Divide documents ("Sign() returns 0 for ±0 ... but Signbit() sees it"),
// and this function fell into it. It matters: atan2 routes a signed zero quotient
// through here, so (angle 1.0-0.0i) came back +0.0 instead of -0.0.
func atanAt(x *big.Float, wp uint) *big.Float {
	if x.Sign() == 0 {
		q := new(big.Float).SetPrec(wp)
		if x.Signbit() {
			q.Neg(q)
		}
		return q
	}
	neg := x.Signbit()
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

// atan2At computes atan2(y, x) at precision wp. The quadrant adjustment runs on
// the big-precision ratio y/x, which stays finite for any finite operands
// (big.Float's exponent range is ~±10⁹), so large-magnitude components no longer
// overflow to a wrong angle.
func atan2At(y, x *big.Float, wp uint) *big.Float {
	pi := piAtPrec(wp)
	two := new(big.Float).SetPrec(wp).SetInt64(2)

	// A ZERO y is where Sign() fails. It returns 0 for BOTH +0 and -0, so the sign of
	// the result -- which IS the sign of y -- becomes invisible, and the old
	// `y.Sign() >= 0` test sent a -0.0 into the +π branch: the wrong QUADRANT.
	//
	// These are IEEE 754's atan2 special cases, the same set Go's math.Atan2 lists:
	//
	//	atan2(±0, x > 0) = ±0        atan2(±0, +0) = ±0
	//	atan2(±0, x < 0) = ±π        atan2(±0, -0) = ±π
	//
	// x is tested with Signbit too, not Sign: a -0.0 abscissa is on the NEGATIVE side
	// of the axis, so atan2(+0, -0.0) is +π and not +0. Sign() cannot tell -0.0 from
	// +0.0, which is the whole reason this block exists.
	if y.Sign() == 0 {
		if x.Signbit() {
			return copySignBig(pi, y, wp)
		}
		return copySignBig(new(big.Float).SetPrec(wp), y, wp)
	}

	switch {
	case x.Sign() > 0:
		return atanAt(new(big.Float).SetPrec(wp).Quo(y, x), wp)
	case x.Sign() < 0 && y.Sign() > 0:
		return new(big.Float).SetPrec(wp).Add(atanAt(new(big.Float).SetPrec(wp).Quo(y, x), wp), pi)
	case x.Sign() < 0: // y.Sign() < 0
		return new(big.Float).SetPrec(wp).Sub(atanAt(new(big.Float).SetPrec(wp).Quo(y, x), wp), pi)
	case y.Sign() > 0: // x is zero
		return new(big.Float).SetPrec(wp).Quo(pi, two)
	default: // y.Sign() < 0, x is zero
		return new(big.Float).SetPrec(wp).Neg(new(big.Float).SetPrec(wp).Quo(pi, two))
	}
}

// copySignBig returns |magnitude| carrying sign's sign bit, at precision wp.
//
// This is math.Copysign for big.Float, and it exists because there is no other way to
// build a NEGATIVE ZERO: a fresh big.Float is +0, and Neg is the only operation that
// sets the sign bit on it. Sign() cannot even observe the result.
func copySignBig(magnitude, sign *big.Float, wp uint) *big.Float {
	q := new(big.Float).SetPrec(wp).Abs(magnitude)
	if sign.Signbit() {
		q.Neg(q)
	}
	return q
}

// BigAtan2 returns atan2(y, x) — the angle of the point (x, y) — rounded to prec bits.
func BigAtan2(y, x *big.Float, prec uint) *big.Float {
	wp := atanWorkPrec(prec)
	yw := new(big.Float).SetPrec(wp).Set(y)
	xw := new(big.Float).SetPrec(wp).Set(x)
	return new(big.Float).SetPrec(prec).Set(atan2At(yw, xw, wp))
}

// atanhSeries computes atanh(t) = Σ t^(2k+1)/(2k+1) for |t| < 1 at precision wp.
// Unlike the alternating atan series, every term is added.
func atanhSeries(t *big.Float, wp uint) *big.Float {
	if t.Sign() == 0 {
		return new(big.Float).SetPrec(wp)
	}
	sum := new(big.Float).SetPrec(wp).Set(t)
	t2 := new(big.Float).SetPrec(wp).Mul(t, t)
	power := new(big.Float).SetPrec(wp).Set(t)
	negligibleExp := -int(wp) - 4
	for n := 1; ; n++ {
		power = new(big.Float).SetPrec(wp).Mul(power, t2)
		frac := new(big.Float).SetPrec(wp).Quo(power, new(big.Float).SetPrec(wp).SetInt64(int64(2*n+1)))
		sum.Add(sum, frac)
		if frac.Sign() == 0 || frac.MantExp(nil) < negligibleExp {
			break
		}
	}
	return sum
}

// bigLn2Cache memoizes ln(2) per precision, LOCK-FREE (sync.Map). Same rationale
// as bigPiCache: engine machinery with no user handle, immutable cached values,
// harmless duplicate computes.
var bigLn2Cache sync.Map // uint prec -> *big.Float (immutable)

// bigLn2 returns ln(2) rounded to prec bits, computed as 2·atanh(1/3) (since
// (2−1)/(2+1) = 1/3). Cached per precision; a defensive copy is returned.
func bigLn2(prec uint) *big.Float {
	cached, ok := bigLn2Cache.Load(prec)
	if ok {
		return new(big.Float).Set(cached.(*big.Float))
	}
	wp := prec + 32
	third := new(big.Float).SetPrec(wp).Quo(new(big.Float).SetPrec(wp).SetInt64(1), new(big.Float).SetPrec(wp).SetInt64(3))
	ln2 := new(big.Float).SetPrec(wp).Mul(atanhSeries(third, wp), new(big.Float).SetPrec(wp).SetInt64(2))
	result := new(big.Float).SetPrec(prec).Set(ln2)
	bigLn2Cache.Store(prec, result)
	return new(big.Float).Set(result)
}

// bigECache memoizes e per precision, LOCK-FREE (sync.Map). Same rationale as
// bigPiCache: engine machinery with no user handle, immutable cached values,
// harmless duplicate computes.
var bigECache sync.Map // uint prec -> *big.Float (immutable)

// BigE returns Euler's number e rounded to prec bits (= exp(1)), cached per
// precision; a defensive copy is returned so callers cannot mutate the cache.
func BigE(prec uint) *big.Float {
	cached, ok := bigECache.Load(prec)
	if ok {
		return new(big.Float).Set(cached.(*big.Float))
	}
	result := BigExp(new(big.Float).SetPrec(prec).SetInt64(1), prec)
	bigECache.Store(prec, result)
	return new(big.Float).Set(result)
}

// logAt computes the natural logarithm of x at precision wp. It splits
// x = mant·2ᵉ (mant ∈ [0.5,1)) so ln(x) = ln(mant) + e·ln(2), with ln(mant)
// from the atanh series on (mant−1)/(mant+1) (|t| ≤ 1/3, fast convergence).
// x ≤ 0 yields −∞ — the only reachable case is x = 0, the atan(±i) pole.
func logAt(x *big.Float, wp uint) *big.Float {
	if x.Sign() <= 0 {
		return new(big.Float).SetPrec(wp).SetInf(true)
	}
	mant := new(big.Float).SetPrec(wp)
	e := x.MantExp(mant)
	one := new(big.Float).SetPrec(wp).SetInt64(1)
	num := new(big.Float).SetPrec(wp).Sub(mant, one)
	den := new(big.Float).SetPrec(wp).Add(mant, one)
	t := new(big.Float).SetPrec(wp).Quo(num, den)
	lnMant := new(big.Float).SetPrec(wp).Mul(atanhSeries(t, wp), new(big.Float).SetPrec(wp).SetInt64(2))
	eLn2 := new(big.Float).SetPrec(wp).Mul(bigLn2(wp), new(big.Float).SetPrec(wp).SetInt64(int64(e)))
	return new(big.Float).SetPrec(wp).Add(lnMant, eLn2)
}

// BigLog returns the natural logarithm of x (x > 0) rounded to prec bits.
func BigLog(x *big.Float, prec uint) *big.Float {
	wp := atanWorkPrec(prec)
	return new(big.Float).SetPrec(prec).Set(logAt(new(big.Float).SetPrec(wp).Set(x), wp))
}

// BigComplexAtan returns the complex arctangent atan(re + im·i) as (real, imag)
// parts rounded to prec bits, via atan(z) = (i/2)[ln(1 − iz) − ln(1 + iz)].
// Computing at big precision keeps components beyond the float64 range from
// overflowing the way cmplx.Atan on a truncated complex128 would.
//
// It agrees with math/cmplx.Atan on the whole plane except the branch cut along
// the imaginary axis for |Im z| > 1 with Re z = 0, where it returns the
// principal (Re > 0) value +π/2 rather than Go's signed-zero −π/2. Callers that
// need Go's convention on that cut should keep using cmplx.Atan in the float64
// range (this function is reached only when a component overflows float64, where
// cmplx.Atan yields NaN and has no value to preserve).
func BigComplexAtan(re, im *big.Float, prec uint) (*big.Float, *big.Float) {
	wp := atanWorkPrec(prec)
	a := new(big.Float).SetPrec(wp).Set(re)
	b := new(big.Float).SetPrec(wp).Set(im)
	one := new(big.Float).SetPrec(wp).SetInt64(1)
	two := new(big.Float).SetPrec(wp).SetInt64(2)
	four := new(big.Float).SetPrec(wp).SetInt64(4)

	// 1 − iz = (1+b) − a·i ; 1 + iz = (1−b) + a·i
	u1 := new(big.Float).SetPrec(wp).Add(one, b)
	v1 := new(big.Float).SetPrec(wp).Neg(a)
	u2 := new(big.Float).SetPrec(wp).Sub(one, b)
	v2 := new(big.Float).SetPrec(wp).Set(a)

	// Re(atan z) = (arg(1+iz) − arg(1−iz)) / 2
	arg1 := atan2At(v1, u1, wp)
	arg2 := atan2At(v2, u2, wp)
	reOut := new(big.Float).SetPrec(wp).Quo(new(big.Float).SetPrec(wp).Sub(arg2, arg1), two)

	// Im(atan z) = (ln|1−iz|² − ln|1+iz|²) / 4
	m1 := new(big.Float).SetPrec(wp).Add(new(big.Float).SetPrec(wp).Mul(u1, u1), new(big.Float).SetPrec(wp).Mul(v1, v1))
	m2 := new(big.Float).SetPrec(wp).Add(new(big.Float).SetPrec(wp).Mul(u2, u2), new(big.Float).SetPrec(wp).Mul(v2, v2))
	imOut := new(big.Float).SetPrec(wp).Quo(new(big.Float).SetPrec(wp).Sub(logAt(m1, wp), logAt(m2, wp)), four)

	return new(big.Float).SetPrec(prec).Set(reOut), new(big.Float).SetPrec(prec).Set(imOut)
}

// roundToInt rounds a *big.Float to the nearest integer (ties away from zero),
// returning a *big.Int.
func roundToInt(f *big.Float) *big.Int {
	half := new(big.Float).SetPrec(f.Prec()).SetFloat64(0.5)
	if f.Sign() < 0 {
		half.Neg(half)
	}
	adjusted := new(big.Float).SetPrec(f.Prec()).Add(f, half)
	q, _ := adjusted.Int(nil) // truncation toward zero, applied after the ±0.5 shift
	return q
}

// expSeries computes exp(r) = Σ rⁿ/n! at precision wp; converges fast for small r.
func expSeries(r *big.Float, wp uint) *big.Float {
	sum := new(big.Float).SetPrec(wp).SetInt64(1)
	term := new(big.Float).SetPrec(wp).SetInt64(1) // rⁿ/n!
	negligibleExp := -int(wp) - 4
	for n := 1; ; n++ {
		term = new(big.Float).SetPrec(wp).Quo(
			new(big.Float).SetPrec(wp).Mul(term, r),
			new(big.Float).SetPrec(wp).SetInt64(int64(n)))
		sum.Add(sum, term)
		if term.Sign() == 0 || term.MantExp(nil) < negligibleExp {
			break
		}
	}
	return sum
}

// BigExp returns eˣ rounded to prec bits. It range-reduces x = k·ln2 + r
// (k = round(x/ln2), |r| ≤ ln2/2), sums exp(r) by Taylor, and rescales by 2ᵏ via
// SetMantExp. Because 2ᵏ is a finite big.Float far below its exponent limit, this
// does not overflow the way math.Exp does past ~709 — exp(1000) is a finite big
// value. Astronomically large x (k beyond the big.Float exponent range) yields
// +Inf; astronomically negative x yields 0.
func BigExp(x *big.Float, prec uint) *big.Float {
	wp := atanWorkPrec(prec)
	xw := new(big.Float).SetPrec(wp).Set(x)
	if xw.Sign() == 0 {
		return new(big.Float).SetPrec(prec).SetInt64(1)
	}
	ln2 := bigLn2(wp)
	k := roundToInt(new(big.Float).SetPrec(wp).Quo(xw, ln2))
	if !k.IsInt64() {
		q := new(big.Float).SetPrec(prec)
		if k.Sign() > 0 {
			q.SetInf(false)
		}
		return q // +Inf for +huge, 0 for −huge
	}
	kf := new(big.Float).SetPrec(wp).SetInt(k)
	r := new(big.Float).SetPrec(wp).Sub(xw, new(big.Float).SetPrec(wp).Mul(kf, ln2))
	expR := expSeries(r, wp)
	scaled := new(big.Float).SetPrec(wp).SetMantExp(expR, int(k.Int64())) // expR · 2ᵏ
	return new(big.Float).SetPrec(prec).Set(scaled)
}

// sinCosWorkPrec returns a working precision that also holds the argument's
// integer part, so the mod-2π reduction of a large argument is exact to prec bits
// (the big-precision analogue of Payne–Hanek). x is an exact dyadic value, so its
// sine/cosine is well-defined at any magnitude; the cost is π to ~exponent bits.
func sinCosWorkPrec(x *big.Float, prec uint) uint {
	wp := atanWorkPrec(prec)
	e := x.MantExp(nil)
	if e > 0 {
		wp += uint(e)
	}
	return wp
}

// sinSeries computes sin(r) = Σ (−1)ⁿ r^(2n+1)/(2n+1)! at precision wp.
func sinSeries(r *big.Float, wp uint) *big.Float {
	sum := new(big.Float).SetPrec(wp).Set(r)
	term := new(big.Float).SetPrec(wp).Set(r)
	r2 := new(big.Float).SetPrec(wp).Mul(r, r)
	negligibleExp := -int(wp) - 4
	for n := 1; ; n++ {
		denom := new(big.Float).SetPrec(wp).SetInt64(int64(2*n) * int64(2*n+1))
		term = new(big.Float).SetPrec(wp).Quo(new(big.Float).SetPrec(wp).Mul(term, r2), denom)
		term.Neg(term)
		sum.Add(sum, term)
		if term.Sign() == 0 || term.MantExp(nil) < negligibleExp {
			break
		}
	}
	return sum
}

// cosSeries computes cos(r) = Σ (−1)ⁿ r^(2n)/(2n)! at precision wp.
func cosSeries(r *big.Float, wp uint) *big.Float {
	sum := new(big.Float).SetPrec(wp).SetInt64(1)
	term := new(big.Float).SetPrec(wp).SetInt64(1)
	r2 := new(big.Float).SetPrec(wp).Mul(r, r)
	negligibleExp := -int(wp) - 4
	for n := 1; ; n++ {
		denom := new(big.Float).SetPrec(wp).SetInt64(int64(2*n-1) * int64(2*n))
		term = new(big.Float).SetPrec(wp).Quo(new(big.Float).SetPrec(wp).Mul(term, r2), denom)
		term.Neg(term)
		sum.Add(sum, term)
		if term.Sign() == 0 || term.MantExp(nil) < negligibleExp {
			break
		}
	}
	return sum
}

// sinCosAt reduces x mod 2π to [−π,π], then to [−π/2,π/2] (where the Taylor
// series avoid the catastrophic cancellation of r near ±π), and returns both
// sin(x) and cos(x) at precision wp. sin is unchanged by the second reduction;
// cos flips sign when the reduction crossed ±π/2.
func sinCosAt(x *big.Float, wp uint) (sin, cos *big.Float) {
	pi := piAtPrec(wp)
	two := new(big.Float).SetPrec(wp).SetInt64(2)
	twoPi := new(big.Float).SetPrec(wp).Mul(pi, two)
	halfPi := new(big.Float).SetPrec(wp).Quo(pi, two)
	negHalfPi := new(big.Float).SetPrec(wp).Neg(halfPi)

	k := roundToInt(new(big.Float).SetPrec(wp).Quo(x, twoPi))
	kf := new(big.Float).SetPrec(wp).SetInt(k)
	r := new(big.Float).SetPrec(wp).Sub(x, new(big.Float).SetPrec(wp).Mul(kf, twoPi))

	cosNeg := false
	if r.Cmp(halfPi) > 0 {
		r = new(big.Float).SetPrec(wp).Sub(pi, r)
		cosNeg = true
	} else if r.Cmp(negHalfPi) < 0 {
		r = new(big.Float).SetPrec(wp).Sub(new(big.Float).SetPrec(wp).Neg(pi), r)
		cosNeg = true
	}
	sin = sinSeries(r, wp)
	cos = cosSeries(r, wp)
	if cosNeg {
		cos.Neg(cos)
	}
	return sin, cos
}

// BigSin returns sin(x) rounded to prec bits.
func BigSin(x *big.Float, prec uint) *big.Float {
	wp := sinCosWorkPrec(x, prec)
	s, _ := sinCosAt(new(big.Float).SetPrec(wp).Set(x), wp)
	return new(big.Float).SetPrec(prec).Set(s)
}

// BigCos returns cos(x) rounded to prec bits.
func BigCos(x *big.Float, prec uint) *big.Float {
	wp := sinCosWorkPrec(x, prec)
	_, c := sinCosAt(new(big.Float).SetPrec(wp).Set(x), wp)
	return new(big.Float).SetPrec(prec).Set(c)
}

// BigTan returns tan(x) = sin(x)/cos(x) rounded to prec bits. At a pole (cos = 0)
// it returns +Inf.
func BigTan(x *big.Float, prec uint) *big.Float {
	wp := sinCosWorkPrec(x, prec)
	s, c := sinCosAt(new(big.Float).SetPrec(wp).Set(x), wp)
	if c.Sign() == 0 {
		return new(big.Float).SetPrec(prec).SetInf(false)
	}
	return new(big.Float).SetPrec(prec).Quo(s, c)
}

// asinAt computes arcsin(x) at precision wp via asin(x) = atan(x/√(1−x²)),
// returning nil when |x| > 1 (that is the complex domain, handled elsewhere).
func asinAt(x *big.Float, wp uint) *big.Float {
	one := new(big.Float).SetPrec(wp).SetInt64(1)
	abs := new(big.Float).SetPrec(wp).Abs(x)
	if abs.Cmp(one) > 0 {
		return nil
	}
	if abs.Cmp(one) == 0 {
		halfPi := new(big.Float).SetPrec(wp).Quo(piAtPrec(wp), new(big.Float).SetPrec(wp).SetInt64(2))
		if x.Sign() < 0 {
			halfPi.Neg(halfPi)
		}
		return halfPi
	}
	x2 := new(big.Float).SetPrec(wp).Mul(x, x)
	denom := new(big.Float).SetPrec(wp).Sqrt(new(big.Float).SetPrec(wp).Sub(one, x2))
	return atanAt(new(big.Float).SetPrec(wp).Quo(x, denom), wp)
}

// BigAsin returns arcsin(x) rounded to prec bits, or nil when |x| > 1 (complex
// domain — the caller falls back to the complex path).
func BigAsin(x *big.Float, prec uint) *big.Float {
	wp := atanWorkPrec(prec)
	q := asinAt(new(big.Float).SetPrec(wp).Set(x), wp)
	if q == nil {
		return nil
	}
	return new(big.Float).SetPrec(prec).Set(q)
}

// BigAcos returns arccos(x) = π/2 − arcsin(x) rounded to prec bits, or nil when
// |x| > 1 (complex domain).
func BigAcos(x *big.Float, prec uint) *big.Float {
	wp := atanWorkPrec(prec)
	as := asinAt(new(big.Float).SetPrec(wp).Set(x), wp)
	if as == nil {
		return nil
	}
	halfPi := new(big.Float).SetPrec(wp).Quo(piAtPrec(wp), new(big.Float).SetPrec(wp).SetInt64(2))
	return new(big.Float).SetPrec(prec).Set(new(big.Float).SetPrec(wp).Sub(halfPi, as))
}
