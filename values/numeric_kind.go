package values

// NumericKind identifies a concrete numeric type for dispatch table indexing.
//
// Used by the receiver-centric dispatch tables in each numeric type file
// to replace 7-way type switches with O(1) array lookups.
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
