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

package helpers

import (
	"errors"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// typeNameFromSentinel extracts the expected-type phrase from a type
// sentinel (e.g., "a string" from werr.ErrNotAString). Returns "" for
// non-type sentinels — the resulting error message will read
// "expected  but got *Foo" which is degraded but unambiguous; the
// TestTypeSentinelsCarryTypeName guard test catches missed sentinels
// at the werr level.
func typeNameFromSentinel(sentinel error) string {
	var se *werr.StaticError
	if errors.As(sentinel, &se) {
		return se.TypeName()
	}
	return ""
}

// RequireArg extracts mc.Arg(index) and asserts it has concrete type T.
// On failure it returns a wrapped error using the given sentinel and primitive name.
// The error message format is "<name>: argument <index+1>: expected <typeName> but got <actual>",
// where typeName is read from the sentinel via *werr.StaticError.TypeName().
// The index is reported 1-indexed to match Scheme convention.
func RequireArg[T any](mc machine.CallContext, index int, sentinel error, name string) (T, error) {
	v := mc.Arg(index)
	result, ok := v.(T)
	if !ok {
		var zero T
		return zero, werr.WrapForeignErrorf(sentinel, "%s: argument %d: expected %s but got %T",
			name, index+1, typeNameFromSentinel(sentinel), v)
	}
	return result, nil
}

// RequireType asserts that v has concrete type T.
// On failure it returns a wrapped error using the given sentinel and primitive name.
// The expected-type phrase is read from the sentinel.
func RequireType[T any](v values.Value, sentinel error, name string) (T, error) {
	result, ok := v.(T)
	if !ok {
		var zero T
		return zero, werr.WrapForeignErrorf(sentinel, "%s: expected %s but got %T",
			name, typeNameFromSentinel(sentinel), v)
	}
	return result, nil
}

// RequireTuple extracts mc.Arg(index) and asserts it is a values.Tuple
// (proper list or empty list). On failure it returns a wrapped ErrNotAList
// error in the canonical "<name>: argument <index+1>: expected a list but
// got <actual>" format. The expected-type phrase is read from
// werr.ErrNotAList via TypeName(). The index is reported 1-indexed.
//
// Use this for any primitive argument that must be a list — Tuple is the
// runtime read-only-list interface (see values/CLAUDE.md).
func RequireTuple(mc machine.CallContext, index int, name string) (values.Tuple, error) {
	v := mc.Arg(index)
	t, ok := v.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAList,
			"%s: argument %d: expected %s but got %T",
			name, index+1, typeNameFromSentinel(werr.ErrNotAList), v)
	}
	return t, nil
}

// ParseOptionalStartEnd extracts optional [start [end]] integer arguments from a rest list.
// defaultEnd is the value used if end is not provided.
// name is used in error messages (e.g., "bytevector-copy").
func ParseOptionalStartEnd(rest values.Value, defaultEnd int64, name string) (int64, int64, error) {
	start := int64(0)
	end := defaultEnd

	if values.IsEmptyList(rest) {
		return start, end, nil
	}

	tuple, ok := rest.(values.Tuple)
	if !ok {
		return 0, 0, werr.WrapForeignErrorf(werr.ErrNotAList, "%s: improper argument list", name)
	}

	startVal, ok := tuple.Car().(*values.Integer)
	if !ok {
		return 0, 0, werr.WrapForeignErrorf(werr.ErrNotAnInteger, "%s: start must be an integer but got %T", name, tuple.Car())
	}
	start = startVal.Value

	cdr := tuple.Cdr()
	if !values.IsEmptyList(cdr) {
		tuple2, ok := cdr.(values.Tuple)
		if !ok {
			return 0, 0, werr.WrapForeignErrorf(werr.ErrNotAList, "%s: improper argument list", name)
		}
		endVal, ok := tuple2.Car().(*values.Integer)
		if !ok {
			return 0, 0, werr.WrapForeignErrorf(werr.ErrNotAnInteger, "%s: end must be an integer but got %T", name, tuple2.Car())
		}
		end = endVal.Value
		tail := tuple2.Cdr()
		if !values.IsEmptyList(tail) {
			_, ok := tail.(values.Tuple)
			if !ok {
				return 0, 0, werr.WrapForeignErrorf(werr.ErrNotAList,
					"%s: improper argument list", name)
			}
			return 0, 0, werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments,
				"%s: too many arguments after end", name)
		}
	}

	return start, end, nil
}

// CheckIndexBounds validates that idx is in range [0, length).
// Returns a wrapped ErrIndexOutOfRange error if the index is out of bounds.
func CheckIndexBounds(idx int64, length int, name string) error {
	if idx < 0 || idx >= int64(length) {
		return werr.WrapForeignErrorf(werr.ErrIndexOutOfRange,
			"%s: index %d out of bounds for length %d", name, idx, length)
	}
	return nil
}

// RequireIndex extracts an exact integer index from mc.Arg(argIdx) and validates
// it is in range [0, length). Accepts *Integer, *BigInteger, and integer-valued
// *Rational via values.ExactInteger, which is more R7RS-correct than requiring
// *Integer alone (R7RS §6.1: indices are exact non-negative integers).
func RequireIndex(mc machine.CallContext, argIdx int, length int, name string) (int, error) {
	k := mc.Arg(argIdx)
	idx, ok := values.ExactInteger(k)
	if !ok {
		return 0, werr.WrapForeignErrorf(werr.ErrNotAnInteger,
			"%s: expected an exact integer index but got %s", name, k.SchemeString())
	}
	err := CheckIndexBounds(idx, length, name)
	if err != nil {
		return 0, err
	}
	return int(idx), nil
}

// ValidateStartEnd checks the invariant 0 <= start <= end <= length.
// Returns a wrapped ErrIndexOutOfRange error if any bound is violated.
func ValidateStartEnd(start, end, length int64, name string) error {
	if start < 0 || end > length || start > end {
		return werr.WrapForeignErrorf(werr.ErrIndexOutOfRange,
			"%s: invalid indices start=%d end=%d for length %d", name, start, end, length)
	}
	return nil
}

// ParseSubrange extracts optional [start [end]] from a rest list, validates
// bounds against length, and returns the range as int values.
// This bundles ParseOptionalStartEnd + ValidateStartEnd + int conversion
// for the common case where the caller works with int-indexed collections.
func ParseSubrange(rest values.Value, length int, name string) (int, int, error) {
	start, end, err := ParseOptionalStartEnd(rest, int64(length), name)
	if err != nil {
		return 0, 0, err
	}
	err = ValidateStartEnd(start, end, int64(length), name)
	if err != nil {
		return 0, 0, err
	}
	return int(start), int(end), nil
}

// ParseOptionalArg extracts a single optional argument from variadic rest args.
// Returns (value, true, nil) if exactly one argument is present.
// Returns (nil, false, nil) if rest is empty.
// Returns an error if rest is not a proper list or contains more than one element.
// name is used in error messages (e.g., "make-vector").
func ParseOptionalArg(rest values.Value, name string) (values.Value, bool, error) {
	if values.IsEmptyList(rest) {
		return nil, false, nil
	}
	tuple, ok := rest.(values.Tuple)
	if !ok {
		return nil, false, werr.WrapForeignErrorf(werr.ErrNotAList,
			"%s: improper argument list", name)
	}
	tail := tuple.Cdr()
	if !values.IsEmptyList(tail) {
		_, ok := tail.(values.Tuple)
		if !ok {
			return nil, false, werr.WrapForeignErrorf(werr.ErrNotAList,
				"%s: improper argument list", name)
		}
		return nil, false, werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments,
			"%s: too many arguments", name)
	}
	return tuple.Car(), true, nil
}

// OptionalArg extracts an optional typed argument from variadic rest args.
// If no argument is present, defaultVal is returned. If present but not of
// type T, an error using sentinel and name is returned (the expected-type
// phrase is read from the sentinel via TypeName()). Extra arguments in
// rest produce an ErrWrongNumberOfArguments error.
func OptionalArg[T any](rest values.Value, defaultVal T, sentinel error, name string) (T, error) {
	v, ok, err := ParseOptionalArg(rest, name)
	if err != nil {
		var zero T
		return zero, err
	}
	if !ok {
		return defaultVal, nil
	}
	return RequireType[T](v, sentinel, name)
}
