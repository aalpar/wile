package values

import (
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
