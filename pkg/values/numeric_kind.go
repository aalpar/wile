package values

// NumericKind identifies a concrete numeric type for dispatch table indexing.
//
// Used by the receiver-centric dispatch tables in each numeric type file
// to replace 7-way type switches with O(1) array lookups.
//
// ADDING A NEW NUMERIC TYPE requires updates in these locations:
//
//  1. values/numeric_kind.go         — add KindXxx constant (this file); bump numKinds implicitly
//  2. values/xxx.go                  — new type file: implement Number interface,
//     declare [numKinds] dispatch tables, register
//     via init() calling makeXxxDispatch helpers
//  3. values/xxx.go                  — register a NumericTypeSpec in the same init() via
//     registerNumericSpec(KindXxx, NumericTypeSpec{...}). Provide the per-kind
//     helper functions (xxxSimplifyDown, xxxToFloat64WithAccuracy, and either
//     xxxToComplex128WithAccuracy or set isAlwaysReal=true to have the registry
//     auto-derive the complex helper via liftRealToComplex128). Provide the
//     schemeName + isAlwaysExact metadata. The registry-driven cold-path
//     functions (Simplify, ExactnessOf, NumberToFloat64, NumberToComplex128Lossy)
//     pick up the new kind automatically.
//  4. values/promotion.go            — add row/column in promotionTable and promoter
//  5. values/numeric_dispatch_test.go — add new dispatch tables to TestAllDispatchEntriesPopulated
//  6. values/numeric_registry_test.go — add the new kind to equivalenceExemplars()
//  7. registry/helpers/value_conv.go  — update ToComplex128, ToFloat64
//  8. extensions/math/prim_conversion.go — update exact->inexact, number->string, etc.
//  9. extensions/math/prim_complex.go — update make-rectangular, make-polar, etc.
//  10. parser/parser_number.go        — if the type can be parsed from source
//  11. registry/helpers/equality.go   — update Eqv if the type has special eqv? semantics
//
// Several historically-manual cold paths are now derived from the item-3 registry
// (Simplify, ExactnessOf, NumberToFloat64, NumberToComplex128Lossy), so the
// registration in item 3 is usually enough — the surviving sites above are the
// ones the registry does not yet cover.
//
// The dispatch tables (item 2) are tested by TestAllDispatchEntriesPopulated.
// The NumericTypeSpec registration (item 3) is enforced eagerly at package
// init: a missing or incomplete registration panics with ErrNumericRegistry
// at process startup.
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
