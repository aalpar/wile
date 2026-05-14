package values

import (
	"fmt"
	"math/big"
	"reflect"
	"testing"
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
		NewComplex(complex(1, 1)),
		NewBigComplex(NewBigFloat(big.NewFloat(1.0)), NewBigFloat(big.NewFloat(1.0))),
	}

	// NumberToFloat64 panics intentionally for Complex/BigComplex — those have
	// no real-only float64 representation per Q-i=C3 in the numeric registry design.
	realTypes := allTypes[:5]

	funcs := []struct {
		name  string
		types []Number
		call  func(Number)
	}{
		{"NumberToFloat64", realTypes, func(n Number) { NumberToFloat64(n) }},
		{"NumberToComplex128", allTypes, func(n Number) { NumberToComplex128(n) }},
		{"Simplify", allTypes, func(n Number) { Simplify(n) }},
		{"ExactnessOf", allTypes, func(n Number) { ExactnessOf(n) }},
	}

	for _, fn := range funcs {
		for _, n := range fn.types {
			name := fmt.Sprintf("%s/%T", fn.name, n)
			t.Run(name, func(t *testing.T) {
				defer func() {
					r := recover()
					if r != nil {
						t.Errorf("panicked: %v", r)
					}
				}()
				fn.call(n)
			})
		}
	}
}
