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

// Port represents an I/O port with source location tracking.
type Port interface {
	Value
	Close() error
}

// InputPort represents an I/O port with source location tracking.
type InputPort interface {
	Port
	Read([]byte) (int, error)
}

// OutputPort represents an I/O port with source location tracking.
type OutputPort interface {
	Port
	Write([]byte) (int, error)
	Flush() error
}

type InputOutputPort interface {
	InputPort
	OutputPort
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
