package values

import "math/big"

// Complex transcendentals on *big.Float components, composed from the real
// kernels (BigExp / BigSin / BigCos / BigLog / BigAtan2) plus complex sqrt. Each
// takes (re, im) and returns (reOut, imOut) rounded to prec bits.
//
// These are reached only when the float64 math/cmplx path over/underflows on a
// finite operand (overflow-gating): in the float64 range the branch-cut-exact
// cmplx.* is used instead, so no branch-cut convention is replicated here. Out of
// that range cmplx.* is NaN, so the principal-value results below are an
// improvement with no reference to diverge from.

// sinhCoshAt returns sinh(x) and cosh(x) at precision wp via eˣ and e⁻ˣ.
func sinhCoshAt(x *big.Float, wp uint) (sinh, cosh *big.Float) {
	ex := new(big.Float).SetPrec(wp).Set(BigExp(x, wp))
	one := new(big.Float).SetPrec(wp).SetInt64(1)
	enx := new(big.Float).SetPrec(wp).Quo(one, ex)
	two := new(big.Float).SetPrec(wp).SetInt64(2)
	sinh = new(big.Float).SetPrec(wp).Quo(new(big.Float).SetPrec(wp).Sub(ex, enx), two)
	cosh = new(big.Float).SetPrec(wp).Quo(new(big.Float).SetPrec(wp).Add(ex, enx), two)
	return sinh, cosh
}

// BigComplexExp returns exp(re + im·i) = exp(re)·(cos im + i·sin im).
func BigComplexExp(re, im *big.Float, prec uint) (*big.Float, *big.Float) {
	wp := atanWorkPrec(prec)
	ea := BigExp(re, wp)
	s := BigSin(im, wp)
	c := BigCos(im, wp)
	reOut := new(big.Float).SetPrec(prec).Mul(ea, c)
	imOut := new(big.Float).SetPrec(prec).Mul(ea, s)
	return reOut, imOut
}

// BigComplexLog returns log(re + im·i) = ½·ln(re²+im²) + i·atan2(im, re) — the
// principal branch (atan2 gives Arg ∈ [−π, π]; the −π endpoint is reached only for
// a negative-zero imaginary part, per IEEE atan2(−0, x<0) = −π).
func BigComplexLog(re, im *big.Float, prec uint) (*big.Float, *big.Float) {
	wp := atanWorkPrec(prec)
	re2 := new(big.Float).SetPrec(wp).Mul(re, re)
	im2 := new(big.Float).SetPrec(wp).Mul(im, im)
	mag2 := new(big.Float).SetPrec(wp).Add(re2, im2)
	half := new(big.Float).SetPrec(wp).SetFloat64(0.5)
	reOut := new(big.Float).SetPrec(prec).Mul(BigLog(mag2, wp), half)
	imOut := new(big.Float).SetPrec(prec).Set(BigAtan2(im, re, wp))
	return reOut, imOut
}

// BigComplexSin returns sin(re + im·i) = sin(re)·cosh(im) + i·cos(re)·sinh(im).
func BigComplexSin(re, im *big.Float, prec uint) (*big.Float, *big.Float) {
	wp := atanWorkPrec(prec)
	s := BigSin(re, wp)
	c := BigCos(re, wp)
	sinh, cosh := sinhCoshAt(new(big.Float).SetPrec(wp).Set(im), wp)
	reOut := new(big.Float).SetPrec(prec).Mul(s, cosh)
	imOut := new(big.Float).SetPrec(prec).Mul(c, sinh)
	return reOut, imOut
}

// BigComplexCos returns cos(re + im·i) = cos(re)·cosh(im) − i·sin(re)·sinh(im).
func BigComplexCos(re, im *big.Float, prec uint) (*big.Float, *big.Float) {
	wp := atanWorkPrec(prec)
	s := BigSin(re, wp)
	c := BigCos(re, wp)
	sinh, cosh := sinhCoshAt(new(big.Float).SetPrec(wp).Set(im), wp)
	reOut := new(big.Float).SetPrec(prec).Mul(c, cosh)
	imOut := new(big.Float).SetPrec(prec).Neg(new(big.Float).SetPrec(wp).Mul(s, sinh))
	return reOut, imOut
}

// BigComplexTan returns tan(z) = sin(z)/cos(z) by complex division.
func BigComplexTan(re, im *big.Float, prec uint) (*big.Float, *big.Float) {
	wp := atanWorkPrec(prec)
	sr, si := BigComplexSin(re, im, wp)
	cr, ci := BigComplexCos(re, im, wp)
	// (sr + si·i) / (cr + ci·i) = [(sr·cr + si·ci) + i(si·cr − sr·ci)] / (cr² + ci²)
	denom := new(big.Float).SetPrec(wp).Add(
		new(big.Float).SetPrec(wp).Mul(cr, cr),
		new(big.Float).SetPrec(wp).Mul(ci, ci))
	reNum := new(big.Float).SetPrec(wp).Add(
		new(big.Float).SetPrec(wp).Mul(sr, cr),
		new(big.Float).SetPrec(wp).Mul(si, ci))
	imNum := new(big.Float).SetPrec(wp).Sub(
		new(big.Float).SetPrec(wp).Mul(si, cr),
		new(big.Float).SetPrec(wp).Mul(sr, ci))
	reOut := new(big.Float).SetPrec(prec).Quo(reNum, denom)
	imOut := new(big.Float).SetPrec(prec).Quo(imNum, denom)
	return reOut, imOut
}

// BigComplexAsin returns asin(z) = −i·ln(iz + √(1 − z²)).
func BigComplexAsin(re, im *big.Float, prec uint) (*big.Float, *big.Float) {
	wp := atanWorkPrec(prec)
	one := new(big.Float).SetPrec(wp).SetInt64(1)
	two := new(big.Float).SetPrec(wp).SetInt64(2)

	// z² = (re² − im²) + 2·re·im·i ; 1 − z² = (1 − re² + im²) − 2·re·im·i
	re2 := new(big.Float).SetPrec(wp).Mul(re, re)
	im2 := new(big.Float).SetPrec(wp).Mul(im, im)
	omz2re := new(big.Float).SetPrec(wp).Add(new(big.Float).SetPrec(wp).Sub(one, re2), im2)
	omz2im := new(big.Float).SetPrec(wp).Neg(
		new(big.Float).SetPrec(wp).Mul(two, new(big.Float).SetPrec(wp).Mul(re, im)))

	// √(1 − z²) — reuse the tested BigComplex.Sqrt (principal branch)
	sq := NewBigComplexFromBigFloats(NewBigFloat(omz2re), NewBigFloat(omz2im)).Sqrt()
	sqRe := sq.RealAsBigFloat().BigFloatValue()
	sqIm := sq.ImagAsBigFloat().BigFloatValue()

	// w = iz + √(1 − z²) ; iz = −im + re·i
	wRe := new(big.Float).SetPrec(wp).Sub(sqRe, im)
	wIm := new(big.Float).SetPrec(wp).Add(sqIm, re)

	// asin(z) = −i·ln(w) = Im(ln w) − i·Re(ln w)
	lRe, lIm := BigComplexLog(wRe, wIm, wp)
	reOut := new(big.Float).SetPrec(prec).Set(lIm)
	imOut := new(big.Float).SetPrec(prec).Neg(lRe)
	return reOut, imOut
}

// BigComplexAcos returns acos(z) = π/2 − asin(z).
func BigComplexAcos(re, im *big.Float, prec uint) (*big.Float, *big.Float) {
	wp := atanWorkPrec(prec)
	asRe, asIm := BigComplexAsin(re, im, prec)
	halfPi := new(big.Float).SetPrec(wp).Quo(piAtPrec(wp), new(big.Float).SetPrec(wp).SetInt64(2))
	reOut := new(big.Float).SetPrec(prec).Sub(halfPi, asRe)
	imOut := new(big.Float).SetPrec(prec).Neg(asIm)
	return reOut, imOut
}
