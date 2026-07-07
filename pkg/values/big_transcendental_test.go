package values

import (
	"math"
	"math/big"
	"math/cmplx"
	"testing"
)

// piReference is π to 80 significant digits (Wikipedia), the non-circular anchor
// for the precision-honesty assertions below.
const piReference = "3.1415926535897932384626433832795028841971693993751058209749445923078164062862090"

// ln2Reference is ln(2) to 80 significant digits.
const ln2Reference = "0.69314718055994530941723212145817656807550013436025525412068000949339362196969471"

// eReference is Euler's number to 80 significant digits.
const eReference = "2.7182818284590452353602874713526624977572470936999595749669676277240766303535476"

func bigRef(t *testing.T, s string) *big.Float {
	t.Helper()
	f, _, err := big.ParseFloat(s, 10, 300, big.ToNearestEven)
	if err != nil {
		t.Fatalf("ParseFloat(%q): %v", s, err)
	}
	return f
}

// closeWithin reports whether |got-want| < 2^tolExp.
func closeWithin(got, want *big.Float, tolExp int) bool {
	diff := new(big.Float).Abs(new(big.Float).Sub(got, want))
	tol := new(big.Float).SetMantExp(big.NewFloat(1), tolExp)
	return diff.Cmp(tol) < 0
}

func f64(v *big.Float) float64 {
	r, _ := v.Float64()
	return r
}

func TestBigPi_MatchesReference(t *testing.T) {
	ref := bigRef(t, piReference)
	got := BigPi(DefaultBigFloatPrecision)
	// 256-bit π is accurate to ~2^-254; allow a couple of ULP of slack.
	if !closeWithin(got, ref, -240) {
		t.Fatalf("BigPi(256) = %s\n differs from reference beyond 2^-240", got.Text('g', 85))
	}
}

func TestBigAtan_PrecisionHonesty(t *testing.T) {
	// 4·atan(1) == π to (near) full 256-bit precision. This is what distinguishes
	// true big precision from a float64-truncating implementation, which would only
	// agree to ~16 digits (~2^-52).
	ref := bigRef(t, piReference)
	fourAtan1 := new(big.Float).Mul(BigAtan(big.NewFloat(1), DefaultBigFloatPrecision), big.NewFloat(4))
	if !closeWithin(fourAtan1, ref, -240) {
		t.Fatalf("4·BigAtan(1) = %s\n differs from π beyond 2^-240 (float64-precision impl would fail here)", fourAtan1.Text('g', 85))
	}
}

func TestBigAtan_KnownValues(t *testing.T) {
	prec := uint(DefaultBigFloatPrecision)
	pi := BigPi(prec)
	quarter := new(big.Float).Quo(pi, big.NewFloat(4)) // atan(1)
	sixth := new(big.Float).Quo(pi, big.NewFloat(6))   // atan(1/√3)
	third := new(big.Float).Quo(pi, big.NewFloat(3))   // atan(√3)
	sqrt3 := new(big.Float).SetPrec(prec).Sqrt(big.NewFloat(3))
	invSqrt3 := new(big.Float).SetPrec(prec).Quo(big.NewFloat(1), sqrt3)

	cases := []struct {
		name string
		x    *big.Float
		want *big.Float
	}{
		{"atan(0)", big.NewFloat(0), new(big.Float)},
		{"atan(1)=pi/4", big.NewFloat(1), quarter},
		{"atan(-1)=-pi/4", big.NewFloat(-1), new(big.Float).Neg(quarter)},
		{"atan(1/sqrt3)=pi/6", invSqrt3, sixth},
		{"atan(sqrt3)=pi/3", sqrt3, third},
	}
	for _, c := range cases {
		got := BigAtan(c.x, prec)
		if !closeWithin(got, c.want, -240) {
			t.Errorf("%s: got %s, want %s", c.name, got.Text('g', 50), c.want.Text('g', 50))
		}
	}
}

func TestBigAtan_AgreesWithMathAtan(t *testing.T) {
	for _, x := range []float64{-1000, -3.5, -1, -0.3, 0.0, 0.25, 0.9, 1, 7.0, 12345.6} {
		got := f64(BigAtan(big.NewFloat(x), DefaultBigFloatPrecision))
		want := math.Atan(x)
		if math.Abs(got-want) > 1e-14 {
			t.Errorf("BigAtan(%v) f64=%v, math.Atan=%v", x, got, want)
		}
	}
}

func TestBigAtan2_AllQuadrants(t *testing.T) {
	vals := []float64{-3, -1, 0, 1, 3}
	for _, y := range vals {
		for _, x := range vals {
			got := f64(BigAtan2(big.NewFloat(y), big.NewFloat(x), DefaultBigFloatPrecision))
			want := math.Atan2(y, x)
			if math.Abs(got-want) > 1e-14 {
				t.Errorf("BigAtan2(%v,%v) f64=%v, math.Atan2=%v", y, x, got, want)
			}
		}
	}
}

func TestBigAtan2_HugeOperandsNoOverflow(t *testing.T) {
	// The regression canary. angle of 1e400 + 1e401 i is atan2(1e401, 1e400) =
	// atan(10) ≈ 1.4711 — NOT π/4 (which the float64-truncating code returned once
	// both components saturated to +Inf).
	y := bigRef(t, "1e401")
	x := bigRef(t, "1e400")
	got := f64(BigAtan2(y, x, DefaultBigFloatPrecision))
	if math.Abs(got-math.Atan(10)) > 1e-12 {
		t.Fatalf("BigAtan2(1e401,1e400) = %v, want atan(10) = %v (overflow regression)", got, math.Atan(10))
	}

	// Equal huge components genuinely are π/4 — sanity that the fix did not overcorrect.
	eq := f64(BigAtan2(bigRef(t, "1e400"), bigRef(t, "1e400"), DefaultBigFloatPrecision))
	if math.Abs(eq-math.Pi/4) > 1e-12 {
		t.Fatalf("BigAtan2(1e400,1e400) = %v, want π/4 = %v", eq, math.Pi/4)
	}
}

func TestBigLog_MatchesReference(t *testing.T) {
	// ln(2) to (near) full 256-bit precision — a float64-precision impl would only
	// agree to ~16 digits.
	ref := bigRef(t, ln2Reference)
	got := BigLog(big.NewFloat(2), DefaultBigFloatPrecision)
	if !closeWithin(got, ref, -240) {
		t.Fatalf("BigLog(2) = %s\n differs from ln(2) beyond 2^-240", got.Text('g', 85))
	}
}

func TestBigLog_AgreesWithMathLog(t *testing.T) {
	for _, x := range []float64{0.01, 0.5, 1.0, 2.0, math.E, 10, 1000, 1e12, 1e300} {
		got := f64(BigLog(big.NewFloat(x), DefaultBigFloatPrecision))
		want := math.Log(x)
		if math.Abs(got-want) > 1e-13*math.Max(1, math.Abs(want)) {
			t.Errorf("BigLog(%v) f64=%v, math.Log=%v", x, got, want)
		}
	}
}

func TestBigExp_MatchesReference(t *testing.T) {
	ref := bigRef(t, eReference)
	got := BigExp(big.NewFloat(1), DefaultBigFloatPrecision)
	if !closeWithin(got, ref, -240) {
		t.Fatalf("BigExp(1) = %s\n differs from e beyond 2^-240", got.Text('g', 85))
	}
}

func TestBigExp_AgreesWithMathExp(t *testing.T) {
	for _, x := range []float64{-10, -3.5, -1, -0.25, 0, 0.5, 1, 2.5, 7, 20} {
		got := f64(BigExp(big.NewFloat(x), DefaultBigFloatPrecision))
		want := math.Exp(x)
		if math.Abs(got-want) > 1e-12*math.Max(1, want) {
			t.Errorf("BigExp(%v) f64=%v, math.Exp=%v", x, got, want)
		}
	}
}

func TestBigExp_NoOverflow(t *testing.T) {
	// math.Exp(1000) = +Inf (float64 overflows past ~709). BigExp stays finite; the
	// round trip BigLog(BigExp(1000)) = 1000 pins both finiteness and magnitude.
	e1000 := BigExp(big.NewFloat(1000), DefaultBigFloatPrecision)
	if e1000.IsInf() {
		t.Fatalf("BigExp(1000) overflowed to Inf")
	}
	back := f64(BigLog(e1000, DefaultBigFloatPrecision))
	if math.Abs(back-1000) > 1e-60 {
		t.Fatalf("BigLog(BigExp(1000)) = %v, want 1000", back)
	}
}

func TestBigSinCos_KnownValues(t *testing.T) {
	prec := uint(DefaultBigFloatPrecision)
	pi := BigPi(prec)
	sixth := new(big.Float).Quo(pi, big.NewFloat(6))
	third := new(big.Float).Quo(pi, big.NewFloat(3))
	half := big.NewFloat(0.5)
	if !closeWithin(BigSin(sixth, prec), half, -240) {
		t.Errorf("sin(π/6) != 1/2: %s", BigSin(sixth, prec).Text('g', 50))
	}
	if !closeWithin(BigCos(third, prec), half, -240) {
		t.Errorf("cos(π/3) != 1/2: %s", BigCos(third, prec).Text('g', 50))
	}
	if BigSin(big.NewFloat(0), prec).Sign() != 0 {
		t.Errorf("sin(0) != 0")
	}
	if !closeWithin(BigCos(big.NewFloat(0), prec), big.NewFloat(1), -240) {
		t.Errorf("cos(0) != 1")
	}
}

func TestBigSinCos_AgreesWithMath(t *testing.T) {
	// Includes 1e15: math.Sin/Cos are Payne–Hanek-correct there, so agreement
	// validates the big mod-2π reduction against the gold standard.
	for _, x := range []float64{-10, -3.1, -1, -0.5, 0, 0.5, 1, 2, 3, 6.28, 100, 1000, 1e15} {
		gs := f64(BigSin(big.NewFloat(x), DefaultBigFloatPrecision))
		gc := f64(BigCos(big.NewFloat(x), DefaultBigFloatPrecision))
		if math.Abs(gs-math.Sin(x)) > 1e-12 {
			t.Errorf("BigSin(%v) f64=%v, math.Sin=%v", x, gs, math.Sin(x))
		}
		if math.Abs(gc-math.Cos(x)) > 1e-12 {
			t.Errorf("BigCos(%v) f64=%v, math.Cos=%v", x, gc, math.Cos(x))
		}
	}
}

func TestBigTan_AgreesWithMath(t *testing.T) {
	for _, x := range []float64{-1.3, -0.5, 0, 0.5, 1, 1.3} {
		got := f64(BigTan(big.NewFloat(x), DefaultBigFloatPrecision))
		if math.Abs(got-math.Tan(x)) > 1e-12 {
			t.Errorf("BigTan(%v) f64=%v, math.Tan=%v", x, got, math.Tan(x))
		}
	}
}

func TestBigAsinAcos_KnownValues(t *testing.T) {
	prec := uint(DefaultBigFloatPrecision)
	pi := BigPi(prec)
	halfPi := new(big.Float).Quo(pi, big.NewFloat(2))
	sixth := new(big.Float).Quo(pi, big.NewFloat(6))
	third := new(big.Float).Quo(pi, big.NewFloat(3))
	// asin(1/2) = π/6 ; asin(1) = π/2 ; acos(1/2) = π/3 ; acos(1) = 0
	if !closeWithin(BigAsin(big.NewFloat(0.5), prec), sixth, -240) {
		t.Errorf("asin(1/2) != π/6")
	}
	if !closeWithin(BigAsin(big.NewFloat(1), prec), halfPi, -240) {
		t.Errorf("asin(1) != π/2")
	}
	if !closeWithin(BigAcos(big.NewFloat(0.5), prec), third, -240) {
		t.Errorf("acos(1/2) != π/3")
	}
	if BigAcos(big.NewFloat(1), prec).Sign() != 0 {
		t.Errorf("acos(1) != 0")
	}
}

func TestBigAsinAcos_AgreesWithMath(t *testing.T) {
	for _, x := range []float64{-1, -0.9, -0.5, 0, 0.3, 0.5, 0.99, 1} {
		as := f64(BigAsin(big.NewFloat(x), DefaultBigFloatPrecision))
		ac := f64(BigAcos(big.NewFloat(x), DefaultBigFloatPrecision))
		if math.Abs(as-math.Asin(x)) > 1e-13 {
			t.Errorf("BigAsin(%v)=%v math.Asin=%v", x, as, math.Asin(x))
		}
		if math.Abs(ac-math.Acos(x)) > 1e-13 {
			t.Errorf("BigAcos(%v)=%v math.Acos=%v", x, ac, math.Acos(x))
		}
	}
}

func TestBigAsinAcos_DeclineOutOfDomain(t *testing.T) {
	// |x| > 1 is the complex domain; the kernels decline (nil) so the primitive
	// falls back to the complex path.
	if BigAsin(big.NewFloat(2), DefaultBigFloatPrecision) != nil {
		t.Errorf("BigAsin(2) should decline (nil)")
	}
	if BigAcos(big.NewFloat(-1.5), DefaultBigFloatPrecision) != nil {
		t.Errorf("BigAcos(-1.5) should decline (nil)")
	}
}

func TestBigComplexAtan_AgreesWithCmplx(t *testing.T) {
	cases := []complex128{0, 1i * 0.5, 2 + 3i, -1 + 0.5i, 0.3 - 4i, 5 + 0i}
	for _, z := range cases {
		reB, imB := BigComplexAtan(big.NewFloat(real(z)), big.NewFloat(imag(z)), DefaultBigFloatPrecision)
		want := cmplx.Atan(z)
		if math.Abs(f64(reB)-real(want)) > 1e-13 || math.Abs(f64(imB)-imag(want)) > 1e-13 {
			t.Errorf("BigComplexAtan(%v) = (%v,%v), cmplx.Atan = (%v,%v)",
				z, f64(reB), f64(imB), real(want), imag(want))
		}
	}
}

func TestBigComplexAtan_ImaginaryAxisBranch(t *testing.T) {
	// On the branch cut (Re z = 0, |Im z| > 1) BigComplexAtan returns the
	// principal Re = +π/2, which diverges from math/cmplx.Atan's signed-zero
	// −π/2. Documented and intentional; the primitive only reaches this function
	// out of float64 range, where cmplx.Atan is NaN anyway. Im part = atanh(1/2).
	reB, imB := BigComplexAtan(big.NewFloat(0), big.NewFloat(2), DefaultBigFloatPrecision)
	if math.Abs(f64(reB)-math.Pi/2) > 1e-13 {
		t.Errorf("Re(atan(2i)) = %v, want principal +π/2 = %v", f64(reB), math.Pi/2)
	}
	if math.Abs(f64(imB)-math.Atanh(0.5)) > 1e-13 {
		t.Errorf("Im(atan(2i)) = %v, want atanh(1/2) = %v", f64(imB), math.Atanh(0.5))
	}
}

func TestBigComplexAtan_HugeNoOverflow(t *testing.T) {
	// atan of a first-quadrant BigComplex with components beyond the float64 range:
	// as |z| → ∞ the value approaches π/2 (real) + 0i. cmplx.Atan on the truncated
	// complex128 sees (+Inf,+Inf) and returns NaN — this must not.
	re := bigRef(t, "1e400")
	im := bigRef(t, "1e400")
	reB, imB := BigComplexAtan(re, im, DefaultBigFloatPrecision)
	if math.Abs(f64(reB)-math.Pi/2) > 1e-6 {
		t.Fatalf("Re(atan(1e400+1e400i)) = %v, want ≈ π/2 = %v", f64(reB), math.Pi/2)
	}
	if math.Abs(f64(imB)) > 1e-6 {
		t.Fatalf("Im(atan(1e400+1e400i)) = %v, want ≈ 0", f64(imB))
	}
}
