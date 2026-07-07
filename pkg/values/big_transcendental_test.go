package values

import (
	"math"
	"math/big"
	"testing"
)

// piReference is π to 80 significant digits (Wikipedia), the non-circular anchor
// for the precision-honesty assertions below.
const piReference = "3.1415926535897932384626433832795028841971693993751058209749445923078164062862090"

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
