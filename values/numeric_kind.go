package values

// NumericKind identifies a concrete numeric type for dispatch table indexing.
//
// Used by the receiver-centric dispatch tables in each numeric type file
// to replace 7-way type switches with O(1) array lookups.
//
// ADDING A NEW NUMERIC TYPE requires updates in these locations:
//
//  1. values/numeric_kind.go         — add KindXxx constant (this file)
//  2. values/xxx.go                  — new type file: implement Number interface,
//     declare [numKinds] dispatch tables, register
//     via init() calling makeXxxDispatch helpers
//  3. values/promotion.go            — add row/column in promotionTable and promoter,
//     update NumberToFloat64, NumberToComplex128
//  4. values/numeric_tower.go        — update Simplify, ExactnessOf
//  5. values/numeric_dispatch_test.go — add new dispatch tables to TestAllDispatchEntriesPopulated
//  6. registry/helpers/value_conv.go  — update ToComplex128, ToFloat64
//  7. extensions/math/prim_conversion.go — update exact->inexact, number->string, etc.
//  8. extensions/math/prim_complex.go — update make-rectangular, make-polar, etc.
//  9. extensions/goast/mapper.go      — update numberToAST if the type maps to a Go literal
//  10. ffi.go                         — update schemeToReflectValue (line ~300)
//  11. internal/parser/parser_number.go — if the type can be parsed from source
//
// The dispatch tables (item 2) are tested by TestAllDispatchEntriesPopulated.
// The type-switch functions (items 3-4, 6) are tested by TestTypeSwitchFunctionsHandleAllTypes.
type NumericKind uint8

const (
	KindInteger NumericKind = iota
	KindBigInteger
	KindFloat
	KindBigFloat
	KindRational
	KindComplex
	KindBigComplex
	numKinds // unexported sentinel for array sizing
)
