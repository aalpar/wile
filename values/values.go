// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package values

import "context"

// Prefix constants for Scheme value representations.
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

// ---------------------------------------------------------------------------
// Sentinel types and singletons
// ---------------------------------------------------------------------------

// voidType is a sentinel value representing the absence of a value.
// It is used as the result of expressions that have no meaningful return value.
type voidType struct{}

func (voidType) SchemeString() string {
	return SpecialVoid
}
func (voidType) IsVoid() bool {
	return true
}
func (voidType) EqualTo(v Value) bool {
	return v != nil && v.IsVoid()
}

// Void is the singleton void value.
var Void Value = voidType{}

// eofType represents the end-of-file object.
type eofType struct{}

func (eofType) SchemeString() string {
	return SpecialEOF
}
func (eofType) IsVoid() bool {
	return false
}
func (eofType) EqualTo(v Value) bool {
	_, ok := v.(eofType)
	return ok
}

// EOFObject is the singleton EOF value.
var EOFObject Value = eofType{}

// ---------------------------------------------------------------------------
// Value — base interface for all Scheme values
// ---------------------------------------------------------------------------

// Value is the base interface for all Scheme values.
//
// Every runtime object in Wile implements Value. The three methods correspond
// to fundamental Scheme operations:
//
//   - SchemeString returns the external representation (R7RS §13.2 write).
//   - IsVoid reports whether this value represents the absence of a result
//     (e.g., the return value of set! or display). A nil receiver must
//     return true so that missing values are treated as void.
//   - EqualTo implements structural equality (R7RS §6.1 equal?).
type Value interface {
	SchemeString() string
	IsVoid() bool
	EqualTo(Value) bool
}

// ---------------------------------------------------------------------------
// Callable — procedure interface
// ---------------------------------------------------------------------------

// Callable represents a Scheme procedure — any value that can be applied
// to arguments.
//
// R7RS §6.1: The procedure? predicate returns #t for all callable types.
// This includes lambdas, case-lambdas, parameter objects (R7RS §4.2.6),
// and composable continuations.
//
// AcceptsArity reports whether the procedure can be called with exactly n
// arguments. This captures arity constraints that are otherwise scattered
// across per-type checks in the VM apply path.
//
// Implemented by: MachineClosure, CaseLambdaClosure, Parameter,
// ComposableContinuation.
type Callable interface {
	Value
	AcceptsArity(n int) bool
}

// ---------------------------------------------------------------------------
// Hashable — hashtable key interface
// ---------------------------------------------------------------------------

// Hashable represents a Value that can be used as a hashtable key.
//
// R7RS §6.10: Hashtables map keys to values. Keys are compared using equal?,
// and the hash function must be consistent with the equality predicate:
// if a.EqualTo(b) then a.HashCode() == b.HashCode().
//
// Implemented by: Integer, BigInteger, Float, BigFloat, Rational, Boolean,
// Character, Symbol, Byte, String.
type Hashable interface {
	Value
	HashCode() uint64
}

// ---------------------------------------------------------------------------
// ForEachFunc + Tuple — list protocol
// ---------------------------------------------------------------------------

// ForEachFunc is the callback signature for iterating over a Tuple.
//
// Parameters:
//   - ctx: context for cancellation
//   - i: zero-based element index
//   - hasNext: true if more elements follow
//   - v: the current element value
//
// Return a non-nil error to stop iteration early.
type ForEachFunc func(ctx context.Context, i int, hasNext bool, v Value) error

// Tuple represents the Scheme list protocol — any value that can be consumed
// as a sequence of car/cdr pairs.
//
// R7RS §6.4: Lists are chains of pairs terminated by the empty list.
// Tuple captures the operations needed to traverse, measure, and convert
// list-shaped values without requiring a concrete *Pair type.
//
// Implemented by: Pair, emptyListType (EmptyList singleton).
//
// IsVoid is listed explicitly because Pair uses a nil-receiver convention
// where (*Pair)(nil) represents void, and the method must be dispatched
// through the interface to handle that case.
type Tuple interface {
	Value
	// Car returns the first element of the pair (R7RS §6.4).
	Car() Value
	// Cdr returns the rest of the list after the first element (R7RS §6.4).
	Cdr() Value
	// ForEach calls fn for each element in order. Returns the tail value
	// (EmptyList for proper lists, the improper cdr otherwise).
	ForEach(ctx context.Context, fn ForEachFunc) (Value, error)
	// Length returns the number of elements. For improper lists, this
	// counts only the proper prefix.
	Length() int
	// Append creates a new list with value appended (R7RS §6.4 append).
	Append(value Value) Value
	// AsVector converts the list to a Vector (R7RS §6.4 list->vector).
	AsVector() *Vector
	// IsList reports whether this is a proper list (R7RS §6.4 list?).
	// Uses Floyd's cycle detection (tortoise-and-hare).
	IsList() bool
	// IsEmptyList reports whether this is the empty list (R7RS §6.4 null?).
	IsEmptyList() bool
	// IsVoid reports whether this value is void (nil receiver handling).
	IsVoid() bool
}

// ---------------------------------------------------------------------------
// Indexable — random-access containers
// ---------------------------------------------------------------------------

// Indexable represents a fixed-size, random-access container of values.
//
// R7RS §6.3.6 (vectors), §6.3.7 (strings as character sequences), §6.4
// (bytevectors): these types support O(1) element access by integer index.
//
// This interface is used for compile-time documentation and type grouping.
// No runtime type assertions against Indexable exist in the codebase;
// dispatch uses concrete types (*Vector, *ByteVector) directly.
//
// Implemented by: Vector, ByteVector.
type Indexable interface {
	Value
	Length() int
	Get(int) Value
	Set(int, Value) error
}

// ---------------------------------------------------------------------------
// SourceLocation — source positions
// ---------------------------------------------------------------------------

// SourceLocation represents a position in source code.
type SourceLocation interface {
	Value
	Index() int
	Column() int
	Line() int
}

// ---------------------------------------------------------------------------
// Port hierarchy — I/O ports
// ---------------------------------------------------------------------------

// Port represents a Scheme I/O port.
//
// R7RS §6.13: All port types support close and open-state queries.
type Port interface {
	Value
	Close() error
	IsClosed() bool
}

// InputPort represents a Scheme input port.
type InputPort interface {
	Port
	Read([]byte) (int, error)
}

// OutputPort represents a Scheme output port.
type OutputPort interface {
	Port
	Write([]byte) (int, error)
	Flush() error
}

// InputOutputPort represents a bidirectional Scheme port.
type InputOutputPort interface {
	InputPort
	OutputPort
}

// TextualReader represents a textual input port capable of rune-level I/O.
//
// R7RS §6.13.2: Textual input ports support read-char, peek-char, read-line, etc.
type TextualReader interface {
	InputPort
	ReadRune() (rune, int, error)
	UnreadRune() error
}

// TextualWriter represents a textual output port capable of rune-level I/O.
//
// R7RS §6.13.3: Textual output ports support write-char, write-string, etc.
type TextualWriter interface {
	OutputPort
	WriteRune(rune) (int, error)
}

// BinaryReader represents a binary input port capable of byte-level I/O.
//
// R7RS §6.13.3: Binary input ports support read-u8, peek-u8, read-bytevector, etc.
type BinaryReader interface {
	InputPort
	ReadByte() (byte, error)
	UnreadByte() error
}

// BinaryWriter represents a binary output port capable of byte-level I/O.
//
// R7RS §6.13.3: Binary output ports support write-u8, write-bytevector, etc.
type BinaryWriter interface {
	OutputPort
	WriteByte(byte) error
}

// ByteVectorExtractor represents a port that can extract its accumulated bytes.
type ByteVectorExtractor interface {
	ReadByteVector() (*ByteVector, error)
}

// ---------------------------------------------------------------------------
// Number hierarchy — numeric tower
// ---------------------------------------------------------------------------

// Number represents a numeric value in the Scheme numeric tower.
//
// R7RS §6.2.1: Numbers form a tower: number ⊃ complex ⊃ real ⊃ rational ⊃ integer.
// All numeric types implement this interface for uniform arithmetic operations.
//
// # Error signaling
//
// Arithmetic methods signal errors by panicking with a static sentinel error
// (e.g., werr.ErrDivisionByZero, werr.ErrNotANumber). This follows the same convention
// used by Go's math/big package, where (*big.Int).Div, (*big.Int).QuoRem,
// and (*big.Float).Quo all panic on division by zero, and mirrors Go's own
// runtime behavior for built-in integer division.
//
// The panic convention is a deliberate design choice: arithmetic methods return
// Number (not (Number, error)), keeping the interface algebraic and composable.
// Callers that need error values should recover panics at their boundary.
// The VM does this in applyForeign, which recovers panics
// and converts them to Scheme exceptions catchable by guard and
// with-exception-handler.
type Number interface {
	Value
	Kind() NumericKind
	Add(Number) Number
	Subtract(Number) Number
	Multiply(Number) Number
	Divide(Number) Number
	Negate() Number
	Abs() Number
	ToExact() Number
	ToInexact() Number
	IsZero() bool
	IsExact() bool
	IsInteger() bool  // R7RS §6.2.6: is this an integer value?
	IsRational() bool // R7RS §6.2.6: is this a rational value?
	IsFinite() bool   // R7RS §6.2.6: is this a finite number?
	IsNaN() bool      // R7RS §6.2.6: is this NaN?
	LessThan(Number) bool
	Compare(Number) int
}

// ComplexNumber represents a complex-valued number with accessible parts.
//
// R7RS §6.2.6: Complex numbers have real and imaginary parts
// accessible via real-part and imag-part.
type ComplexNumber interface {
	Number
	RealPart() Number
	ImagPart() Number
	IsReal() bool
}

// RealNumber represents a real-valued number with sign operations.
//
// R7RS §6.2.6: The positive? and negative? predicates apply only to
// real numbers. Sign returns -1, 0, or 1.
type RealNumber interface {
	Number
	IsPositive() bool
	IsNegative() bool
	Sign() int
}
