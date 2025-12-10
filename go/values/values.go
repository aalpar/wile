package values

const (
	PrefixCharacter    = `#\`
	PrefixSyntax       = `#'`
	PrefixDirective    = `#!`
	PrefixBox          = `#&`
	PrefixPrimitive    = `#%`
	PrefixBlockComment = `#|`
	PrefixLineComment  = `;`

	SpecialEOF  = PrefixDirective + `eof`
	SpecialVoid = PrefixDirective + `void`
)

// voidType is a sentinel value representing the absence of a value.
// It is used as the result of expressions that have no meaningful return value.
type voidType struct{}

func (voidType) SchemeString() string { return SpecialVoid }
func (voidType) IsVoid() bool         { return true }
func (voidType) EqualTo(v Value) bool { return v != nil && v.IsVoid() }

// Void is the singleton void value.
var Void Value = voidType{}

// eofType represents the end-of-file object.
type eofType struct{}

func (eofType) SchemeString() string { return SpecialEOF }
func (eofType) IsVoid() bool         { return false }
func (eofType) EqualTo(v Value) bool {
	_, ok := v.(eofType)
	return ok
}

// EofObject is the singleton EOF value.
var EofObject Value = eofType{}

type Wrapped interface {
	Value
	Unwrap() Value
	Wrap(Value)
}

type Collection interface {
	AsList() Tuple
}

// ForEachFunc is the type of function called for each element in the Pair list.
type ForEachFunc func(i int, hasNext bool, v Value) error

type Tuple interface {
	Value
	Length() int
	Append(value Value) Value
	ForEach(eachFunc ForEachFunc) (Value, error)
	IsEmptyList() bool
	IsList() bool
	IsVoid() bool
	AsVector() *Vector
	Car() Value
	Cdr() Value
}

type SourceLocation interface {
	Value
	Index() int
	Column() int
	Line() int
}

type Value interface {
	SchemeString() string
	IsVoid() bool
	EqualTo(Value) bool
}

type Comparable interface {
	Value
	CompareTo(Value) int
}

type Number interface {
	Value
	Add(Number) Number
	Subtract(Number) Number
	Multiply(Number) Number
	Divide(Number) Number
	IsZero() bool
	LessThan(Number) bool
}
