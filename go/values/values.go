// Copyright 2025 Aaron Alpar
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

// EOFObject is the singleton EOF value.
var EOFObject Value = eofType{}

// Table represents a key-value mapping interface.
type Table interface {
	HasKey(Value) bool
	Get(Value) (Value, bool)
	Set(Value, Value)
	Keys() Tuple
	Values() Tuple
}

// Wrapped represents a value that wraps another value.
type Wrapped interface {
	Value
	Unwrap() Value
	Wrap(Value)
}

type Indexable interface {
	Value
	Length() int
	Get(int) Value
	Set(int, Value)
}

// Collection represents a container that can be converted to a list.
type Collection interface {
	Value
	AsList() Tuple
}

// Set represents an unordered collection of unique values.
type Set interface {
	Value
	AsList() Tuple
}

// ForEachFunc is the type of function called for each element in the Pair list.
type ForEachFunc func(ctx context.Context, i int, hasNext bool, v Value) error

// Tuple represents a list-like sequence of values.
type Tuple interface {
	Value
	Length() int
	Append(value Value) Value
	ForEach(ctx context.Context, fn ForEachFunc) (Value, error)
	IsEmptyList() bool
	IsList() bool
	IsVoid() bool
	AsVector() *Vector
	Car() Value
	Cdr() Value
}

// SourceLocation represents a position in source code.
type SourceLocation interface {
	Value
	Index() int
	Column() int
	Line() int
}

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

// Value is the base interface for all Scheme values.
type Value interface {
	SchemeString() string
	IsVoid() bool
	EqualTo(Value) bool
}

// Comparable represents a value that can be compared for ordering.
type Comparable interface {
	Value
	CompareTo(Value) int
}

// Number represents a numeric value in the Scheme numeric tower.
//
// R7RS §6.2.1: Numbers form a tower: number ⊃ complex ⊃ real ⊃ rational ⊃ integer.
// All numeric types implement this interface for uniform arithmetic operations.
//
// # Error signaling
//
// Arithmetic methods signal errors by panicking with a static sentinel error
// (e.g., ErrDivisionByZero, ErrNotANumber). This follows the same convention
// used by Go's math/big package, where (*big.Int).Div, (*big.Int).QuoRem,
// and (*big.Float).Quo all panic on division by zero, and mirrors Go's own
// runtime behavior for built-in integer division.
//
// The panic convention is a deliberate design choice: arithmetic methods return
// Number (not (Number, error)), keeping the interface algebraic and composable.
// Callers that need error values should recover panics at their boundary.
// The VM does this in OperationForeignFunctionCall.Apply, which recovers panics
// and converts them to Scheme exceptions catchable by guard and
// with-exception-handler.
type Number interface {
	Value
	Add(Number) Number
	Subtract(Number) Number
	Multiply(Number) Number
	Divide(Number) Number
	Negate() Number
	IsZero() bool
	IsExact() bool
	LessThan(Number) bool
	Compare(Number) int
}
