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

// ValueOrVoid returns v, or the singleton Void when v is nil. It collapses the
// repeated "nil accessor result -> Void" guard used by primitives whose Go
// accessor returns a nil Value for an unset slot (thread/mutex/condvar
// -specific, atomic-box load/swap). Follows the BoolToBoolean / StringOrFalse
// precedent for eliminating repeated if/else patterns.
func ValueOrVoid(v Value) Value {
	if v == nil {
		return Void
	}
	return v
}

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
//   - SchemeString returns the external representation (R7RS §6.13.3 write).
//   - IsVoid reports whether this value represents the absence of a result
//     (e.g., the return value of set! or display). A nil receiver must
//     return true so that missing values are treated as void.
//   - EqualTo implements structural equality (R7RS §6.1 equal?).
//
// # Implementors MUST be Go-comparable
//
// This is a hard requirement of the contract, and it has no compile-time
// expression — the compiler will not stop you from breaking it.
//
// R7RS §6.1 defines eq?/eqv? on aggregates as "denote the same location in the
// store." A Scheme object must therefore HAVE a location, and Wile spells that as
// Go pointer identity: EqIdentity (utils.go) is a bare `a == b` on the interface,
// and it backs eq?, memq, and assq — the hot path. Go panics with "comparing
// uncomparable type" when the dynamic type behind an interface is a slice, map,
// or func. Not an error return: a panic, in the embedder's process.
//
// The RECEIVER, not the underlying type, decides. Vector is []Value and it is
// perfectly safe, because its methods take POINTER receivers — the dynamic type
// boxed into the interface is *Vector, which is a pointer and hence comparable.
// The mistake to avoid is a VALUE receiver on a slice-, map-, or func-backed
// type, which puts the naked slice into the interface:
//
//	type MyThing []Value        // with VALUE receivers: eq? panics on it
//	type MyThing struct{ … }    // with POINTER receivers: *MyThing is comparable, safe
//
// Enforced by TestValue_ImplementorsAreGoComparable over allValueExemplars
// (values) and TestMachineValues_AreGoComparable (machine). A type that
// implements Value and is absent from those rosters is unguarded.
//
// If your type is not a Scheme datum — a compiler or VM container that merely
// wanted SchemeString for diagnostics — do not implement Value at all. Having an
// equality method is not the same as being a Value; give it a concrete
// EqualTo(T) instead. machine.Operations and machine.MultipleValues are the
// worked examples.
//
// ADDING A NEW VALUE TYPE requires at minimum:
//
//  1. values/<type>.go              — implement Value (SchemeString, IsVoid, EqualTo),
//     with POINTER receivers unless the type is already
//     comparable (an empty struct, a scalar)
//  2. values/<type>_test.go         — test the three Value methods + type-specific behavior
//  3. allValueExemplars             — add an exemplar (value_isvoid_convention_test.go);
//     this is what enforces both the IsVoid convention
//     and Go-comparability
//
// Depending on the type's role, also update:
//
//  3. registry/core/prim_predicates.go  — if it needs a type predicate (e.g., box?)
//  4. ffi.go                           — if it maps to/from Go types via RegisterFunc
//  5. values/scheme_writer.go          — if it has internal structure that can be shared/circular
//  6. machine/machine_context_apply.go  — if it is callable (implements Callable)
//  7. machine/native_template.go       — if it can appear as a compile-time literal
//  8. values/value_type.go             — if the type participates in the extension-API type
//     vocabulary: add a ValueType constant + a typeInfos
//     row (name/description) + a check assignment in
//     init(), AND a row in goTypeToValueType so
//     SchemeTypeName renders a Scheme name instead of
//     leaking the Go type via %T.
//     Types named in Scheme but without a ValueType
//     counterpart (Record, Box, Promise) instead get an
//     arm in SchemeTypeName's explicit switch.
//
// If the new type has capability-conditional operations (e.g.,
// optional read/write/seek surfaces), expose them via
// AsXxx() (T, bool) methods following the *PortObject pattern
// (values/port.go — AsReader, AsByteWriter, etc.). Document any new
// slot invariants in a Validate() method that constructors call.
//
// For numeric types, see the more detailed guide in values/numeric_kind.go (12 items).
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
// AcceptsArity reports whether the procedure can be called with n arguments.
// This captures arity constraints that are otherwise scattered across per-type
// checks in the VM apply path. Fixed-arity callables (lambdas) accept only their
// declared count; variadic and continuation callables accept a range or any
// number — continuations resume with whatever number of values they are invoked
// with (R7RS §6.10), so they accept any arity.
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
// Immutable — values that carry their own mutability constraint
// ---------------------------------------------------------------------------

// Immutable is implemented by value types that store their immutability as an
// intrinsic, per-instance property — currently only *String (R7RS §6.7: literal
// strings and symbol->string results are immutable).
//
// It exists so callers can ask "may this value be mutated in place?" without
// knowing the storage mechanism. Pair and Vector deliberately do NOT implement
// it: they are raw [2]Value / []Value types whose immutability is tracked in an
// engine-scoped side-set (see environment.ImmutableLiterals) to keep the
// dominant heap objects word-for-word minimal. The uniform query that spans
// both mechanisms is (*environment.ImmutableLiterals).IsImmutable.
type Immutable interface {
	Value
	// IsImmutable reports whether in-place mutation of this value is forbidden.
	IsImmutable() bool
}

// ---------------------------------------------------------------------------
// Indexable — random-access containers
// ---------------------------------------------------------------------------

// Indexable represents a fixed-size, random-access container of values.
//
// R7RS §6.8 (vectors), §6.7 (strings as character sequences), §6.9 (bytevectors).
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

// Port represents a Scheme I/O port — the marker interface satisfied
// by any port value. Concretely implemented by *PortObject (the sole
// implementer; capability-conditional operations are reached via
// As*() (T, bool) accessors on *PortObject rather than narrower
// interfaces).
//
// R7RS §6.13: All port types support close and open-state queries.
type Port interface {
	Value
	Close() error
	IsClosed() bool
}

// ByteVectorExtractor represents a port that can extract its accumulated bytes.
// Returned by (*PortObject).AsByteVectorExtractor.
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
// Divide returns (Number, error) so callers can propagate division-by-zero
// without panic/recover. All other arithmetic methods remain single-return
// because they cannot fail.
type Number interface {
	Value
	Kind() NumericKind
	Add(Number) Number
	Subtract(Number) Number
	Multiply(Number) Number
	Divide(Number) (Number, error)
	Negate() Number
	Abs() Number
	ToExact() (Number, error)
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

	// SignBit reports whether the value carries a negative sign bit.
	//
	// This is NOT IsNegative(), and the difference is the whole reason it exists.
	// IsNegative asks "is n < 0", which is FALSE for a negative zero: -0.0 is not
	// less than zero. IsPositive is false for it too, and Sign() returns 0 for BOTH
	// +0 and -0 -- so none of the three can see a negative zero at all.
	//
	// SignBit asks the IEEE question, and it is load-bearing wherever a zero's sign
	// picks a branch. (angle -0.0) is π, not 0, because -0.0 lies on the NEGATIVE
	// real axis. The trap is already documented at big_float.go ("Sign() returns 0
	// for ±0 ... but Signbit() sees it") -- and code kept falling into it, because
	// the predicate that sees it did not exist.
	//
	// For the exact kinds (Integer, BigInteger, Rational) there is no signed zero,
	// so SignBit coincides with IsNegative.
	SignBit() bool
}
