package values

import (
	"errors"
	"fmt"
	"math/big"
	"reflect"
	"testing"

	"github.com/aalpar/wile/werr"
)

// TestAllDispatchEntriesPopulated verifies that every dispatch table
// has all numKinds entries populated (no nil function pointers).
// This catches the case where a new numeric type is added without
// updating all dispatch tables.
func TestAllDispatchEntriesPopulated(t *testing.T) {
	tables := []struct {
		name  string
		table any // [numKinds]func(...)
	}{
		{"integerAdd", integerAdd},
		{"bigIntegerAdd", bigIntegerAdd},
		{"floatAdd", floatAdd},
		{"bigFloatAdd", bigFloatAdd},
		{"rationalAdd", rationalAdd},
		{"complexAdd", complexAdd},
		{"bigComplexAdd", bigComplexAdd},
		{"integerSubtract", integerSubtract},
		{"bigIntegerSubtract", bigIntegerSubtract},
		{"floatSubtract", floatSubtract},
		{"bigFloatSubtract", bigFloatSubtract},
		{"rationalSubtract", rationalSubtract},
		{"complexSubtract", complexSubtract},
		{"bigComplexSubtract", bigComplexSubtract},
		{"integerLessThan", integerLessThan},
		{"bigIntegerLessThan", bigIntegerLessThan},
		{"floatLessThan", floatLessThan},
		{"bigFloatLessThan", bigFloatLessThan},
		{"rationalLessThan", rationalLessThan},
		{"complexLessThan", complexLessThan},
		{"integerCompare", integerCompare},
		{"bigIntegerCompare", bigIntegerCompare},
		{"floatCompare", floatCompare},
		{"bigFloatCompare", bigFloatCompare},
		{"rationalCompare", rationalCompare},
		{"complexCompare", complexCompare},
		{"bigComplexCompare", bigComplexCompare},
		{"integerMultiply", integerMultiply},
		{"bigIntegerMultiply", bigIntegerMultiply},
		{"floatMultiply", floatMultiply},
		{"bigFloatMultiply", bigFloatMultiply},
		{"rationalMultiply", rationalMultiply},
		{"complexMultiply", complexMultiply},
		{"bigComplexMultiply", bigComplexMultiply},
		{"integerDivide", integerDivide},
		{"bigIntegerDivide", bigIntegerDivide},
		{"floatDivide", floatDivide},
		{"bigFloatDivide", bigFloatDivide},
		{"rationalDivide", rationalDivide},
		{"complexDivide", complexDivide},
		{"bigComplexDivide", bigComplexDivide},
	}

	for _, tt := range tables {
		t.Run(tt.name, func(t *testing.T) {
			v := reflect.ValueOf(tt.table)
			if v.Kind() != reflect.Array {
				t.Fatalf("expected array, got %s", v.Kind())
			}
			for i := 0; i < v.Len(); i++ {
				if v.Index(i).IsNil() {
					t.Errorf("%s[%d] is nil", tt.name, i)
				}
			}
		})
	}
}

// TestTypeSwitchFunctionsHandleAllTypes verifies that standalone functions
// with type switches over Number handle all concrete numeric types without
// panicking. This complements TestAllDispatchEntriesPopulated, which covers
// the [numKinds] dispatch tables.
//
// If a new numeric type is added and these functions are not updated,
// this test will panic (caught by the deferred recover).
func TestTypeSwitchFunctionsHandleAllTypes(t *testing.T) {
	allTypes := []Number{
		NewInteger(1),
		NewBigInteger(big.NewInt(1)),
		NewFloat(1.0),
		NewBigFloat(big.NewFloat(1.0)),
		NewRational(1, 1),
		// Complex/BigComplex inputs to NumberToFloat64 must have imag==0; a
		// nonzero imag is the documented panic path tested below.
		NewComplex(complex(1, 0)),
		NewBigComplex(NewBigFloat(big.NewFloat(1.0)), NewBigFloat(big.NewFloat(0))),
	}

	funcs := []struct {
		name string
		call func(Number)
	}{
		{"NumberToFloat64", func(n Number) { NumberToFloat64(n) }},
		{"NumberToComplex128", func(n Number) { NumberToComplex128Lossy(n) }},
		{"Simplify", func(n Number) { Simplify(n) }},
		{"ExactnessOf", func(n Number) { ExactnessOf(n) }},
	}

	for _, fn := range funcs {
		for _, n := range allTypes {
			name := fmt.Sprintf("%s/%T", fn.name, n)
			t.Run(name, func(t *testing.T) {
				defer func() {
					r := recover()
					if r == nil {
						return
					}
					// Surface unexpected panics as typed test failures, not
					// silent recovers. Any panic in this no-arg roster is a
					// regression.
					err, ok := r.(error)
					if ok {
						t.Errorf("unexpected panic with error: %v", err)
						return
					}
					t.Errorf("unexpected non-error panic: %v", r)
				}()
				fn.call(n)
			})
		}
	}
}

// TestNumberToFloat64PanicsOnNonzeroImag locks in the contract that
// NumberToFloat64 panics with an ErrNotAReal-rooted error when given a
// complex value with a nonzero imaginary component. The cause-preserving
// wrap (werr.WrapForeignErrorWithCause) means errors.Is(panicVal, ErrNotAReal)
// must succeed.
func TestNumberToFloat64PanicsOnNonzeroImag(t *testing.T) {
	cases := []Number{
		NewComplex(complex(3, 4)),
		NewBigComplex(NewBigFloat(big.NewFloat(3)), NewBigFloat(big.NewFloat(4))),
	}
	for _, n := range cases {
		t.Run(fmt.Sprintf("%T", n), func(t *testing.T) {
			defer func() {
				r := recover()
				if r == nil {
					t.Fatal("expected panic, got none")
				}
				err, ok := r.(error)
				if !ok {
					t.Fatalf("expected error panic, got %T: %v", r, r)
				}
				if !errors.Is(err, werr.ErrNotAReal) {
					t.Errorf("panic chain does not contain ErrNotAReal: %v", err)
				}
			}()
			NumberToFloat64(n)
		})
	}
}
