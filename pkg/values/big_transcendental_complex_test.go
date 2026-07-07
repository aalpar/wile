package values

import (
	"math"
	"math/big"
	"math/cmplx"
	"testing"
)

// agreeWithCmplx checks a big-complex kernel against math/cmplx at in-range
// points (chosen off the branch cuts), where cmplx.* is the reference.
func agreeWithCmplx(t *testing.T, name string,
	fn func(re, im *big.Float, prec uint) (*big.Float, *big.Float),
	ref func(complex128) complex128, zs []complex128) {
	t.Helper()
	for _, z := range zs {
		reB, imB := fn(big.NewFloat(real(z)), big.NewFloat(imag(z)), DefaultBigFloatPrecision)
		w := ref(z)
		gr, _ := reB.Float64()
		gi, _ := imB.Float64()
		if math.Abs(gr-real(w)) > 1e-12 || math.Abs(gi-imag(w)) > 1e-12 {
			t.Errorf("%s(%v) = (%v,%v), cmplx = (%v,%v)", name, z, gr, gi, real(w), imag(w))
		}
	}
}

func TestBigComplexExp_AgreesWithCmplx(t *testing.T) {
	agreeWithCmplx(t, "exp", BigComplexExp, cmplx.Exp,
		[]complex128{0, 1 + 1i, -2 + 3i, 0.5 - 0.5i, 3 + 0i, 0 + 2i})
}

func TestBigComplexLog_AgreesWithCmplx(t *testing.T) {
	// Off the negative-real-axis cut (all have nonzero imaginary part or re>0).
	agreeWithCmplx(t, "log", BigComplexLog, cmplx.Log,
		[]complex128{1 + 1i, 2 + 3i, -1 + 2i, 0.5 - 0.5i, 5 + 0i})
}

func TestBigComplexSinCos_AgreesWithCmplx(t *testing.T) {
	zs := []complex128{0.5 + 0.5i, 1 + 2i, -1 + 0.5i, 2 - 1i, 0 + 1i}
	agreeWithCmplx(t, "sin", BigComplexSin, cmplx.Sin, zs)
	agreeWithCmplx(t, "cos", BigComplexCos, cmplx.Cos, zs)
}

func TestBigComplexTan_AgreesWithCmplx(t *testing.T) {
	agreeWithCmplx(t, "tan", BigComplexTan, cmplx.Tan,
		[]complex128{0.5 + 0.5i, 1 + 2i, -1 + 0.5i, 0.3 - 0.7i})
}

func TestBigComplexAsinAcos_AgreesWithCmplx(t *testing.T) {
	// Off the real-axis |x|>1 branch cuts (all have nonzero imaginary part).
	zs := []complex128{0.5 + 0.5i, 0.3 + 0.7i, -0.4 + 0.2i, 0.5 + 2i, 0.2 - 1.5i}
	agreeWithCmplx(t, "asin", BigComplexAsin, cmplx.Asin, zs)
	agreeWithCmplx(t, "acos", BigComplexAcos, cmplx.Acos, zs)
}

func TestBigComplex_HugeNoOverflow(t *testing.T) {
	finite := func(re, im *big.Float) bool {
		return !re.IsInf() && !im.IsInf()
	}
	// exp with a real part past the float64 overflow point (cmplx.Exp → Inf).
	re, im := BigComplexExp(big.NewFloat(1000), big.NewFloat(1), DefaultBigFloatPrecision)
	if !finite(re, im) {
		t.Errorf("BigComplexExp(1000+i) not finite")
	}
	// sin with a huge imaginary part (cosh/sinh overflow float64).
	re, im = BigComplexSin(big.NewFloat(1), big.NewFloat(1000), DefaultBigFloatPrecision)
	if !finite(re, im) {
		t.Errorf("BigComplexSin(1+1000i) not finite")
	}
	// log of a component beyond the float64 range.
	beyond, _, _ := big.ParseFloat("1e400", 10, DefaultBigFloatPrecision, big.ToNearestEven)
	re, im = BigComplexLog(beyond, big.NewFloat(1), DefaultBigFloatPrecision)
	if !finite(re, im) {
		t.Errorf("BigComplexLog(1e400+i) not finite")
	}
	// log(1e400+i) ≈ ln(1e400) = 400·ln(10) ≈ 921.034 (imaginary part ≈ 0).
	back, _ := re.Float64()
	if math.Abs(back-921.0340371976184) > 1e-6 {
		t.Errorf("Re(BigComplexLog(1e400+i)) = %v, want ≈ 921.034", back)
	}
}
